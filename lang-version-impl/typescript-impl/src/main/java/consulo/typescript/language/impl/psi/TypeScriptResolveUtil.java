package consulo.typescript.language.impl.psi;

import com.intellij.lang.javascript.psi.JSReferenceList;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSParenthesizedExpression;
import com.intellij.lang.javascript.psi.JSArrayLiteralExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSNewExpression;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.ecmal4.JSReferenceListMember;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.javascript.ecmascript.psi.ES6StringTemplateExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.javascript.impl.library.JavaScriptLibraries;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import consulo.javascript.psi.JSTypeOnlyDeclaration;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.impl.ast.LeafElement;
import consulo.language.impl.psi.stub.StubBasedPsiElementBase;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.PsiReference;
import consulo.language.psi.StubBasedPsiElement;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.stub.NamedStub;
import consulo.language.psi.stub.PsiFileStub;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.StubIndex;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.project.DumbService;
import consulo.typescript.language.impl.stub.TypeScriptStubElementTypes;
import consulo.typescript.language.psi.TypeScriptTypes;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Resolve of the TypeScript PSI on the JavaScript resolve machinery: the TypeScript elements
 * implement the JavaScript interfaces and declare their names through {@code processDeclarations}, so
 * {@link JSResolveUtil#treeWalkUp} and the JavaScript processors resolve TypeScript references.
 *
 * @since 2026-10-30
 */
public final class TypeScriptResolveUtil {
    private TypeScriptResolveUtil() {
    }

    /**
     * The name token of a declaration: its own IDENTIFIER, or the one of a simple binding pattern ({@code const x}).
     */
    @Nullable
    @RequiredReadAction
    public static PsiElement findNameIdentifier(PsiElement declaration) {
        ASTNode node = declaration.getNode();
        ASTNode identifier = node.findChildByType(JSTokenTypes.IDENTIFIER);
        if (identifier != null) {
            return identifier.getPsi();
        }
        ASTNode bindingPattern = node.findChildByType(TypeScriptTypes.BINDING_PATTERN);
        if (bindingPattern != null) {
            ASTNode first = bindingPattern.getFirstChildNode();
            if (first != null && first == bindingPattern.getLastChildNode() && first.getElementType() == JSTokenTypes.IDENTIFIER) {
                return first.getPsi();
            }
        }
        return null;
    }

    /**
     * Renames a declaration or reference by replacing the text of its name token.
     */
    @RequiredReadAction
    public static void rename(@Nullable PsiElement nameIdentifier, String newName) throws IncorrectOperationException {
        if (nameIdentifier == null || !(nameIdentifier.getNode() instanceof LeafElement leaf)) {
            throw new IncorrectOperationException("no name to rename: " + nameIdentifier);
        }
        leaf.replaceWithText(newName);
    }

    /**
     * Declarations seen from inside a function: its parameters (JavaScript's JSFunctionBaseImpl).
     */
    @RequiredReadAction
    public static boolean processFunctionDeclarations(
        JSFunction function,
        PsiScopeProcessor processor,
        ResolveState state,
        @Nullable PsiElement lastParent
    ) {
        if (lastParent != null && lastParent.getParent() == function) {
            if (!processTypeParameters(function, processor, state)) {
                return false;
            }
            for (JSParameter parameter : getParameters(function)) {
                if (!parameter.processDeclarations(processor, state, null, function)) {
                    return false;
                }
            }
            processor.handleEvent(PsiScopeProcessor.Event.SET_DECLARATION_HOLDER, function);
            return true;
        }
        return function.getName() == null || processor.execute(function, state);
    }

    /**
     * The parameters of a function, including the single parameter of {@code x => ...}.
     */
    @RequiredReadAction
    public static List<JSParameter> getParameters(JSFunction function) {
        List<JSParameter> result = new ArrayList<>();
        JSParameterList parameterList = function.getParameterList();
        if (parameterList != null) {
            result.addAll(List.of(parameterList.getParameters()));
        }
        for (PsiElement child = function.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSParameter parameter) {
                result.add(parameter);
            }
        }
        return result;
    }

    /**
     * Declares the variables among the children of a statement ({@code for (let i ...)}, {@code for (const x of ...)}),
     * up to the one containing the place.
     */
    @RequiredReadAction
    public static boolean processChildVariables(
        PsiElement statement,
        PsiScopeProcessor processor,
        ResolveState state,
        @Nullable PsiElement lastParent
    ) {
        for (PsiElement child = statement.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child == lastParent) {
                break;
            }
            if (child instanceof JSVariable variable && !variable.processDeclarations(processor, state, null, statement)) {
                return false;
            }
        }
        return true;
    }

    /**
     * The members of a class body: methods, accessors, properties and the parameter properties of the constructor
     * ({@code constructor(private http: HttpClient)}).
     */
    @RequiredReadAction
    public static List<PsiElement> getClassMembers(JSClass jsClass) {
        if (isNamespace(jsClass)) {
            return getNamespaceMembersByTree(jsClass, null);
        }
        List<PsiElement> members = new ArrayList<>();
        ASTNode body = jsClass.getNode().findChildByType(TypeScriptTypes.CLASS_BODY);
        PsiElement container = body != null ? body.getPsi() : jsClass;
        for (PsiElement child = container.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSFunction function) {
                if (child.getNode().getElementType() == TypeScriptTypes.CONSTRUCTOR_DECLARATION) {
                    for (JSParameter parameter : getParameters(function)) {
                        if (isParameterProperty(parameter)) {
                            members.add(parameter);
                        }
                    }
                }
                else {
                    members.add(function);
                }
            }
            else if (child instanceof JSVariable || child instanceof JSProperty) {
                members.add(child);
            }
        }
        return members;
    }

    private static boolean isParameterProperty(JSParameter parameter) {
        return parameter.getNode().findChildByType(TypeScriptTypes.ATTRIBUTE_LIST) != null
            || parameter.getNode().findChildByType(TypeScriptTypes.PUBLIC_KEYWORD) != null
            || parameter.getNode().findChildByType(TypeScriptTypes.PRIVATE_KEYWORD) != null
            || parameter.getNode().findChildByType(TypeScriptTypes.PROTECTED_KEYWORD) != null;
    }

    /**
     * The classes named by {@code extends} of a class.
     */
    @RequiredReadAction
    public static List<JSClass> getSuperClasses(JSClass jsClass) {
        List<JSClass> result = new ArrayList<>();
        JSReferenceList extendsList = jsClass.getExtendsList();
        if (extendsList == null) {
            return result;
        }
        // interface HTMLElement extends Element of a global declaration file: the names of the stub, the global types
        PsiFile file = jsClass.getContainingFile();
        if (getGreenStub(extendsList) != null && file != null && TypeScriptModuleUtil.isGlobalDeclaration(jsClass)) {
            for (String name : extendsList.getReferenceTexts()) {
                JSClass superClass = findGlobalType(jsClass, name);
                if (superClass != null) {
                    result.add(superClass);
                }
            }
            return result;
        }
        for (JSReferenceExpression reference : extendsList.getExpressions()) {
            if (reference.resolve() instanceof JSClass superClass) {
                result.add(superClass);
            }
        }
        // the type names of an interface's list
        for (JSReferenceListMember member : extendsList.getMembers()) {
            if (!(member instanceof JSReferenceExpression) && member instanceof PsiReference reference
                && reference.resolve() instanceof JSClass superClass) {
                result.add(superClass);
            }
        }
        return result;
    }

    /**
     * The class or interface a type name of a declaration of a global file refers to: a member of its enclosing
     * namespaces first ({@code new (): DateTimeFormat} in {@code namespace Intl}), else a global type. By the stubs.
     */
    @Nullable
    @RequiredReadAction
    public static JSClass findDeclaredType(PsiElement declaration, @Nullable String name) {
        if (name == null) {
            return null;
        }
        for (PsiElement parent = PsiTreeUtil.getStubOrPsiParent(declaration); parent != null && !(parent instanceof PsiFile);
             parent = PsiTreeUtil.getStubOrPsiParent(parent)) {
            if (parent instanceof JSClass namespace && isNamespace(namespace)) {
                for (JSClass declarationOfNamespace : getMergedDeclarations(namespace)) {
                    StubElement<?> stub = getGreenStub(declarationOfNamespace);
                    List<PsiElement> members = stub != null ? findOwnStubMembers(declarationOfNamespace, name) : getNamespaceMembersByTree(declarationOfNamespace, name);
                    for (PsiElement member : members) {
                        if (member instanceof JSClass jsClass) {
                            return jsClass;
                        }
                    }
                }
            }
        }
        return findGlobalType(declaration, name);
    }

    /**
     * A class or interface of the global scripts by name (the declarations of lib.*.d.ts), by the index. Null while
     * indexing and for qualified names (namespaces aren't indexed as classes).
     */
    @Nullable
    @RequiredReadAction
    public static JSClass findGlobalType(PsiElement context, @Nullable String name) {
        if (name == null || name.indexOf('.') >= 0 || DumbService.isDumb(context.getProject())) {
            return null;
        }
        for (JSClass jsClass : StubIndex.getElements(
            JavaScriptIndexKeys.CLASSES_BY_NAME, name, context.getProject(), JavaScriptLibraries.restrictResolveScope(context, context.getResolveScope()), JSClass.class)) {
            if (TypeScriptModuleUtil.isGlobalDeclaration(jsClass)) {
                return jsClass;
            }
        }
        return null;
    }

    /**
     * Resolves a member of a class or of its super classes.
     */
    @RequiredReadAction
    public static List<PsiElement> findMembers(JSClass jsClass, String name) {
        List<PsiElement> result = new ArrayList<>();
        collectMembers(jsClass, name, result, new HashSet<>());
        return result;
    }

    private static void collectMembers(JSClass jsClass, String name, List<PsiElement> result, Set<JSClass> visited) {
        if (!visited.add(jsClass)) {
            return;
        }
        // the declaration itself, then the ones it merges with (other files: the index) only when it lacks the member
        collectOwnMembers(List.of(jsClass), name, result);
        List<JSClass> declarations = List.of(jsClass);
        if (result.isEmpty()) {
            declarations = getMergedDeclarations(jsClass);
            List<JSClass> others = new ArrayList<>(declarations);
            others.remove(jsClass);
            visited.addAll(others);
            collectOwnMembers(others, name, result);
        }
        if (result.isEmpty()) {
            for (JSClass declaration : declarations) {
                for (JSClass superClass : getSuperClasses(declaration)) {
                    collectMembers(superClass, name, result, visited);
                }
            }
        }
    }

    /**
     * The members of the declarations: by the stubs of all, then by the AST - a member found by the stubs doesn't parse
     * the other files.
     */
    @RequiredReadAction
    private static void collectOwnMembers(List<JSClass> declarations, String name, List<PsiElement> result) {
        for (JSClass declaration : declarations) {
            result.addAll(findOwnStubMembers(declaration, name));
        }
        if (!result.isEmpty()) {
            return;
        }
        for (JSClass declaration : declarations) {
            // the members of a stubbed interface or namespace are all in its stub
            if ((declaration.isInterface() || isNamespace(declaration)) && getGreenStub(declaration) != null) {
                continue;
            }
            for (PsiElement member : getClassMembers(declaration)) {
                if (name.equals(((PsiNamedElement) member).getName())) {
                    result.add(member);
                }
            }
        }
    }

    /**
     * The stubbed members of one declaration with the name (methods, accessors, method signatures, class properties):
     * the declarations of other files - lib.dom.d.ts - aren't parsed for them. What isn't stubbed (property signatures
     * of interfaces, parameter properties) needs the AST.
     */
    @RequiredReadAction
    private static List<PsiElement> findOwnStubMembers(JSClass declaration, String name) {
        StubElement<?> stub = getGreenStub(declaration);
        if (stub == null) {
            return List.of();
        }
        List<PsiElement> result = new ArrayList<>();
        collectStubMembers(stub, name, result);
        return result;
    }

    private static void collectStubMembers(StubElement<?> owner, String name, List<PsiElement> result) {
        for (StubElement<?> child : owner.getChildrenStubs()) {
            if (child.getStubType() == TypeScriptStubElementTypes.VAR_STATEMENT) {
                // var Collator: {...} of a namespace
                collectStubMembers(child, name, result);
            }
            else if (child instanceof NamedStub<?> named && name.equals(named.getName())
                && child.getStubType() != TypeScriptStubElementTypes.CONSTRUCTOR_DECLARATION) {
                result.add(child.getPsi());
            }
        }
    }

    @Nullable
    private static StubElement<?> getGreenStub(PsiElement element) {
        return element instanceof StubBasedPsiElementBase<?> stubBased ? stubBased.getGreenStub() : null;
    }

    /**
     * A declaration of the file itself (not in a namespace or a function), by its stub when there is one.
     */
    @RequiredReadAction
    private static boolean isTopLevel(PsiElement declaration) {
        StubElement<?> stub = getGreenStub(declaration);
        if (stub != null) {
            StubElement<?> parent = stub.getParentStub();
            // a variable: its var statement
            if (parent != null && parent.getStubType() == TypeScriptStubElementTypes.VAR_STATEMENT) {
                parent = parent.getParentStub();
            }
            return parent instanceof PsiFileStub;
        }
        ASTNode node = declaration.getNode();
        ASTNode statement = node.getElementType() == TypeScriptStubElementTypes.VARIABLE_DECLARATION ? node.getTreeParent() : node;
        return statement != null && TypeScriptStubElementTypes.isTopLevel(statement);
    }

    /**
     * An interface (or namespace) of a global script and the global interfaces (namespaces) with the same name it merges
     * with (lib.es5.d.ts and lib.es2018.promise.d.ts both declare {@code interface Promise<T>}, eleven files declare
     * {@code namespace Intl}). Only the declaration itself while indexing.
     */
    @RequiredReadAction
    public static List<JSClass> getMergedDeclarations(JSClass jsClass) {
        String name = jsClass.getName();
        PsiFile file = jsClass.getContainingFile();
        boolean namespace = isNamespace(jsClass);
        if (!(jsClass.isInterface() || namespace) || name == null || file == null || DumbService.isDumb(jsClass.getProject())
            || !TypeScriptModuleUtil.isGlobalDeclaration(jsClass) || !isTopLevel(jsClass)) {
            return List.of(jsClass);
        }
        List<JSClass> result = new ArrayList<>();
        result.add(jsClass);
        for (JSClass other : StubIndex.getElements(
            JavaScriptIndexKeys.CLASSES_BY_NAME, name, jsClass.getProject(), JavaScriptLibraries.restrictResolveScope(jsClass, jsClass.getResolveScope()), JSClass.class)) {
            if (other != jsClass && (namespace ? isNamespace(other) : other.isInterface()) && TypeScriptModuleUtil.isGlobalDeclaration(other)) {
                result.add(other);
            }
        }
        return result;
    }

    /**
     * A small part of the TypeScript type evaluation: the class of an expression used as a qualifier - a class
     * ({@code Foo.bar}, static members), {@code new Foo()}, a variable or parameter typed by a class or initialized with
     * {@code new Foo()}.
     */
    @Nullable
    @RequiredReadAction
    public static JSClass evaluateClass(@Nullable PsiElement element, Set<PsiElement> visited) {
        if (element == null || !visited.add(element)) {
            return null;
        }
        if (element instanceof JSClass jsClass) {
            return jsClass;
        }
        if (element instanceof JSNewExpression newExpression) {
            JSExpression constructor = newExpression.getMethodExpression();
            return constructor instanceof JSReferenceExpression reference ? evaluateConstructedClass(reference, visited) : null;
        }
        if (element instanceof JSCallExpression call) {
            // a call: the class of the return type of the function ({@code document.getElementById('a').focus()})
            // of the overloads the first that returns a class: querySelector('#app') - the one of a selector, not of a
            // tag name (HTMLElementTagNameMap[K])
            JSExpression method = call.getMethodExpression();
            if (!(method instanceof JSReferenceExpression reference)) {
                return null;
            }
            for (ResolveResult result : reference.multiResolve(false)) {
                if (result.getElement() instanceof JSFunction jsFunction && visited.add(jsFunction)) {
                    JSClass returned = resolveReturnTypeClass(jsFunction);
                    if (returned != null) {
                        return returned;
                    }
                }
            }
            return null;
        }
        if (element instanceof JSReferenceExpression reference) {
            PsiElement resolved = reference.resolve();
            return resolved instanceof JSClass ? null : evaluateClass(resolved, visited);
        }
        if (element instanceof JSVariable variable) {
            JSClass byType = resolveTypeAnnotationClass(variable);
            if (byType != null) {
                return byType;
            }
            JSExpression initializer = variable.getInitializer();
            return initializer != null ? evaluateClass(initializer, visited) : null;
        }
        if (element instanceof JSLiteralExpression literal) {
            // the members of the primitive values: "a".toUpperCase(), (1).toFixed()
            return findGlobalType(literal, getLiteralTypeName(literal));
        }
        if (element instanceof ES6StringTemplateExpression) {
            return findGlobalType(element, "String");
        }
        if (element instanceof JSArrayLiteralExpression) {
            return findGlobalType(element, "Array");
        }
        if (element instanceof JSParenthesizedExpression parenthesized) {
            return evaluateClass(parenthesized.getInnerExpression(), visited);
        }
        return null;
    }

    @Nullable
    @RequiredReadAction
    private static String getLiteralTypeName(JSLiteralExpression literal) {
        if (literal.isQuotedLiteral()) {
            return "String";
        }
        ASTNode token = literal.getNode().getFirstChildNode();
        IElementType type = token == null ? null : token.getElementType();
        if (type == TypeScriptTypes.NUMERIC_LITERAL) {
            return "Number";
        }
        if (type == TypeScriptTypes.TRUE_KEYWORD || type == TypeScriptTypes.FALSE_KEYWORD) {
            return "Boolean";
        }
        return type == TypeScriptTypes.REGEXP_LITERAL ? "RegExp" : null;
    }

    /**
     * The class of the return type of a function: by the type text of its stub for a declaration of a global file (the
     * methods of lib.dom.d.ts), else by its return type annotation, else a global type of the type text
     * ({@code string} - {@code String}).
     */
    @Nullable
    @RequiredReadAction
    private static JSClass resolveReturnTypeClass(JSFunction function) {
        String typeName = getTypeName(function.getReturnTypeString());
        if (getGreenStub(function) != null && TypeScriptModuleUtil.isGlobalDeclaration(function)) {
            return findDeclaredType(function, typeName);
        }
        ASTNode annotation = function.getNode().findChildByType(TypeScriptTypes.TYPE_ANNOTATION);
        JSClass byReference = annotation == null ? null : resolveTypeClass(annotation.findChildByType(TypeScriptTypes.TYPE_REFERENCE));
        return byReference != null ? byReference : findGlobalType(function, typeName);
    }

    /**
     * The class created by {@code new X()}: the class X; for a global variable X the interface X it merges with - the
     * standard library declares {@code interface Date} and {@code declare var Date: DateConstructor} with
     * {@code new (): Date}, or {@code declare var XMLHttpRequest: { new (): XMLHttpRequest }} - found by the index
     * without parsing the declaring file; else the class the construct signature of the variable's type returns.
     */
    @Nullable
    @RequiredReadAction
    private static JSClass evaluateConstructedClass(JSReferenceExpression reference, Set<PsiElement> visited) {
        JSClass sameName = null;
        for (ResolveResult result : reference.multiResolve(false)) {
            PsiElement target = result.getElement();
            if (target instanceof JSClass jsClass) {
                if (!jsClass.isInterface()) {
                    return jsClass;
                }
                if (sameName == null) {
                    sameName = jsClass;
                }
            }
            else if (target instanceof JSVariable variable && visited.add(variable)) {
                JSClass merged = findGlobalInterface(variable);
                if (merged != null) {
                    return merged;
                }
                JSClass constructed = findConstructSignatureClass(variable);
                if (constructed != null) {
                    return constructed;
                }
            }
        }
        // the value and the interface of the same name merge
        return sameName;
    }

    /**
     * The class {@code new x()} creates for a variable x: the interface merged with a global variable, or what its
     * construct signature returns (the stubs of a declaration file).
     */
    @Nullable
    @RequiredReadAction
    public static JSClass findConstructedClass(JSVariable variable) {
        JSClass merged = findGlobalInterface(variable);
        return merged != null ? merged : findConstructSignatureClass(variable);
    }

    /**
     * {@code interface X} of the global scripts for {@code declare var X} of a global script.
     */
    @Nullable
    @RequiredReadAction
    private static JSClass findGlobalInterface(JSVariable variable) {
        String name = variable.getName();
        PsiFile file = variable.getContainingFile();
        if (name == null || file == null || DumbService.isDumb(variable.getProject()) || !TypeScriptModuleUtil.isGlobalDeclaration(variable)
            || !isTopLevel(variable)) {
            return null;
        }
        for (JSClass jsClass : StubIndex.getElements(
            JavaScriptIndexKeys.CLASSES_BY_NAME, name, variable.getProject(), JavaScriptLibraries.restrictResolveScope(variable, variable.getResolveScope()), JSClass.class)) {
            if (jsClass.isInterface() && TypeScriptModuleUtil.isGlobalDeclaration(jsClass)) {
                return jsClass;
            }
        }
        return null;
    }

    @Nullable
    @RequiredReadAction
    private static JSClass findConstructSignatureClass(JSVariable variable) {
        StubElement<?> stub = getGreenStub(variable);
        if (stub != null) {
            // declare var Image: { new (): HTMLImageElement } - the signatures are stubs of the variable
            JSClass constructed = findStubConstructedClass(stub, variable);
            if (constructed != null) {
                return constructed;
            }
            // declare var Foo: FooConstructor - the signatures of the interface
            JSClass constructorType = findDeclaredType(variable, getTypeName(variable.getTypeString()));
            if (constructorType != null) {
                for (JSClass declaration : getMergedDeclarations(constructorType)) {
                    StubElement<?> declarationStub = getGreenStub(declaration);
                    constructed = declarationStub != null ? findStubConstructedClass(declarationStub, variable) : null;
                    if (constructed != null) {
                        return constructed;
                    }
                }
            }
            if (TypeScriptModuleUtil.isGlobalDeclaration(variable)) {
                return null;
            }
        }
        ASTNode annotation = variable.getNode().findChildByType(TypeScriptTypes.TYPE_ANNOTATION);
        if (annotation == null) {
            return null;
        }
        List<ASTNode> containers = new ArrayList<>();
        ASTNode objectType = annotation.findChildByType(TypeScriptTypes.OBJECT_TYPE);
        if (objectType != null) {
            containers.add(objectType);
        }
        else if (resolveTypeClass(annotation.findChildByType(TypeScriptTypes.TYPE_REFERENCE)) instanceof JSClass constructorType) {
            for (JSClass declaration : getMergedDeclarations(constructorType)) {
                containers.add(declaration.getNode());
            }
        }
        for (ASTNode container : containers) {
            for (ASTNode signature : container.getChildren(TokenSet.create(TypeScriptTypes.CONSTRUCT_SIGNATURE_MEMBER))) {
                ASTNode returnType = signature.findChildByType(TypeScriptTypes.TYPE_ANNOTATION);
                JSClass constructed = returnType == null ? null : resolveTypeClass(returnType.findChildByType(TypeScriptTypes.TYPE_REFERENCE));
                if (constructed != null) {
                    return constructed;
                }
            }
        }
        return null;
    }

    @Nullable
    @RequiredReadAction
    private static JSClass findStubConstructedClass(StubElement<?> owner, PsiElement context) {
        for (StubElement<?> child : owner.getChildrenStubs()) {
            if (child.getStubType() == TypeScriptStubElementTypes.CONSTRUCT_SIGNATURE_MEMBER && child instanceof JSFunctionStub signature) {
                JSClass constructed = findDeclaredType(context, getTypeName(signature.getReturnTypeString()));
                if (constructed != null) {
                    return constructed;
                }
            }
        }
        return null;
    }

    /**
     * {@code x: Foo} - the class named by a simple type annotation.
     */
    @Nullable
    @RequiredReadAction
    private static JSClass resolveTypeAnnotationClass(JSVariable variable) {
        // a declaration of a global declaration file (document: Document): the type text of its stub, a global type
        PsiFile file = variable.getContainingFile();
        if (getGreenStub(variable) != null && file != null && TypeScriptModuleUtil.isGlobalDeclaration(variable)) {
            return findDeclaredType(variable, getTypeName(variable.getTypeString()));
        }
        ASTNode annotation = variable.getNode().findChildByType(TypeScriptTypes.TYPE_ANNOTATION);
        if (annotation == null) {
            return null;
        }
        JSClass byReference = resolveTypeClass(annotation.findChildByType(TypeScriptTypes.TYPE_REFERENCE));
        // let s: string, let e: HTMLElement | null - a global type of the type text
        return byReference != null ? byReference : findGlobalType(variable, getTypeName(variable.getTypeString()));
    }

    /**
     * The class or interface of a type reference ({@code Foo}, {@code Promise<T>}, {@code ns.Foo}).
     */
    @Nullable
    @RequiredReadAction
    private static JSClass resolveTypeClass(@Nullable ASTNode typeReference) {
        ASTNode qualifiedName = typeReference == null ? null : typeReference.findChildByType(TypeScriptTypes.QUALIFIED_NAME);
        if (qualifiedName == null || !(qualifiedName.getPsi() instanceof PsiReference reference)) {
            return null;
        }
        return reference.resolve() instanceof JSClass jsClass ? jsClass : null;
    }

    /**
     * The text of the type annotation of a declaration ({@code x: Foo<T>} - {@code Foo<T>}): the type text of its stub.
     * Null without an annotation or for an inline object type (large, and no name to resolve).
     */
    @Nullable
    @RequiredReadAction
    public static String getTypeAnnotationText(PsiElement declaration) {
        ASTNode annotation = declaration.getNode().findChildByType(TypeScriptTypes.TYPE_ANNOTATION);
        if (annotation == null || annotation.findChildByType(TypeScriptTypes.OBJECT_TYPE) != null) {
            return null;
        }
        ASTNode colon = annotation.findChildByType(JSTokenTypes.COLON);
        String text = colon == null ? annotation.getText() : annotation.getText().substring(colon.getStartOffsetInParent() + 1);
        String type = text.trim().replaceAll("\\s+", " ");
        return type.isEmpty() ? null : type;
    }

    /**
     * The declarations of a function with the same name in its container (the overloads of a declaration file, with
     * itself): by the stubs.
     */
    @RequiredReadAction
    public static List<JSFunction> getOverloads(JSFunction function) {
        String name = function.getName();
        if (name == null) {
            return List.of(function);
        }
        List<JSFunction> result = new ArrayList<>();
        StubElement<?> stub = getGreenStub(function);
        StubElement<?> parentStub = stub == null ? null : stub.getParentStub();
        if (parentStub != null) {
            for (StubElement<?> child : parentStub.getChildrenStubs()) {
                if (child.getPsi() instanceof JSFunction sibling && name.equals(sibling.getName())) {
                    result.add(sibling);
                }
            }
        }
        else if (function.getParent() != null) {
            for (PsiElement child : function.getParent().getChildren()) {
                if (child instanceof JSFunction sibling && name.equals(sibling.getName())) {
                    result.add(sibling);
                }
            }
        }
        return result.isEmpty() ? List.of(function) : result;
    }

    /**
     * The type text of a generic declaration without its type parameters: each one by its default, else by its
     * constraint ({@code querySelector<E extends Element = Element>(s: string): E | null} returns {@code Element | null}).
     * Built into the stub, so the type of a call evaluates without the AST of the declaration file.
     */
    @Nullable
    @RequiredReadAction
    public static String eraseTypeParameters(PsiElement owner, @Nullable String typeText) {
        ASTNode list = typeText == null ? null : owner.getNode().findChildByType(TypeScriptTypes.TYPE_PARAMETER_LIST);
        if (list == null) {
            return typeText;
        }
        String result = typeText;
        for (ASTNode parameter : list.getChildren(TokenSet.create(TypeScriptTypes.TYPE_PARAMETER))) {
            ASTNode name = parameter.findChildByType(JSTokenTypes.IDENTIFIER);
            String replacement = getTypeParameterBound(parameter);
            if (name != null && replacement != null) {
                String pattern = "(?<![\\p{L}\\p{N}_$.])" + java.util.regex.Pattern.quote(name.getText()) + "(?![\\p{L}\\p{N}_$])";
                String bound = replacement.indexOf('|') >= 0 || replacement.indexOf('&') >= 0 ? "(" + replacement + ")" : replacement;
                result = result.replaceAll(pattern, java.util.regex.Matcher.quoteReplacement(bound));
            }
        }
        return result;
    }

    /**
     * The default of a type parameter ({@code = Element}), else its constraint ({@code extends Element}).
     */
    @Nullable
    private static String getTypeParameterBound(ASTNode parameter) {
        String defaultType = null;
        String constraint = null;
        boolean afterExtends = false;
        boolean afterEq = false;
        for (ASTNode child = parameter.getFirstChildNode(); child != null; child = child.getTreeNext()) {
            String text = child.getText();
            if (child.getElementType() == JSTokenTypes.EQ || "=".equals(text)) {
                afterEq = true;
                afterExtends = false;
            }
            else if ("extends".equals(text)) {
                afterExtends = true;
            }
            else if (child.getPsi() instanceof consulo.language.psi.PsiWhiteSpace || child.getPsi() instanceof consulo.language.psi.PsiComment) {
                continue;
            }
            else if (afterEq && defaultType == null) {
                defaultType = text;
            }
            else if (afterExtends && constraint == null) {
                constraint = text;
            }
        }
        String bound = defaultType != null ? defaultType : constraint;
        return bound == null ? null : bound.trim().replaceAll("\\s+", " ");
    }

    /**
     * The name of the class a type text refers to: {@code Promise<T>} - {@code Promise}, {@code Intl.Collator} -
     * {@code Intl.Collator}, {@code HTMLElement | null} - {@code HTMLElement}, {@code string} - {@code String},
     * {@code T[]} - {@code Array}. Null for other unions, function types, tuples, literals...
     */
    @Nullable
    public static String getTypeName(@Nullable String typeText) {
        if (typeText == null) {
            return null;
        }
        String text = typeText.trim();
        if (text.indexOf('|') >= 0) {
            // T | null | undefined: the type besides null and undefined
            String single = null;
            for (String part : splitTopLevel(text, '|')) {
                String trimmed = part.trim();
                if (trimmed.isEmpty() || trimmed.equals("null") || trimmed.equals("undefined")) {
                    continue;
                }
                if (single != null) {
                    return null;
                }
                single = trimmed;
            }
            if (single == null) {
                return null;
            }
            text = single;
        }
        if (text.endsWith("[]")) {
            return "Array";
        }
        int typeArguments = text.indexOf('<');
        String name = (typeArguments < 0 ? text : text.substring(0, typeArguments)).trim();
        String wrapper = PRIMITIVE_WRAPPERS.get(name);
        if (wrapper != null) {
            return wrapper;
        }
        return TYPE_NAME.matcher(name).matches() ? name : null;
    }

    /**
     * The interfaces of the standard library declaring the members of the primitive types.
     */
    private static final java.util.Map<String, String> PRIMITIVE_WRAPPERS = java.util.Map.of(
        "string", "String",
        "number", "Number",
        "boolean", "Boolean",
        "bigint", "BigInt",
        "symbol", "Symbol",
        "object", "Object"
    );

    /**
     * The parts of a type text separated by a separator outside of brackets ({@code Map<A, B | C> | null}).
     */
    private static List<String> splitTopLevel(String text, char separator) {
        List<String> result = new ArrayList<>();
        int depth = 0;
        int start = 0;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (c == '<' || c == '(' || c == '[' || c == '{') {
                depth++;
            }
            else if (c == '>' || c == ')' || c == ']' || c == '}') {
                depth--;
            }
            else if (c == separator && depth == 0) {
                result.add(text.substring(start, i));
                start = i + 1;
            }
        }
        result.add(text.substring(start));
        return result;
    }

    private static final java.util.regex.Pattern TYPE_NAME = java.util.regex.Pattern.compile("[\\p{L}_$][\\p{L}\\p{N}_$]*(\\.[\\p{L}_$][\\p{L}\\p{N}_$]*)*");

    /**
     * Whether a member is {@code static}.
     */
    @RequiredReadAction
    public static boolean isStatic(PsiElement member) {
        StubElement<?> stub = getGreenStub(member);
        StubElement<?> owner = stub == null ? null : stub.getParentStub();
        // by the stubs: the members of an interface aren't static, the ones of a namespace are (Reflect.ownKeys)
        if (owner != null && owner.getStubType() == TypeScriptStubElementTypes.VAR_STATEMENT) {
            owner = owner.getParentStub();
        }
        if (owner != null && owner.getStubType() == TypeScriptStubElementTypes.INTERFACE_DECLARATION) {
            return false;
        }
        if (owner != null && owner.getStubType() == TypeScriptStubElementTypes.NAMESPACE_DECLARATION) {
            return true;
        }
        ASTNode attributes = member.getNode().findChildByType(TypeScriptTypes.ATTRIBUTE_LIST);
        if (attributes != null && attributes.findChildByType(JSTokenTypes.STATIC_KEYWORD) != null) {
            return true;
        }
        PsiElement previous = member.getPrevSibling();
        while (previous != null && !(previous instanceof JSFunction) && !(previous instanceof JSVariable)) {
            if (previous instanceof JSAttributeList attributeList && attributeList.getNode().findChildByType(JSTokenTypes.STATIC_KEYWORD) != null) {
                return true;
            }
            if (previous.getNode().getElementType() == JSTokenTypes.STATIC_KEYWORD) {
                return true;
            }
            previous = previous.getPrevSibling();
        }
        return false;
    }

    /**
     * Declares the type parameters of a generic declaration ({@code class Box<T>}, {@code function f<T>()}).
     */
    @RequiredReadAction
    public static boolean processTypeParameters(PsiElement owner, PsiScopeProcessor processor, ResolveState state) {
        ASTNode list = owner.getNode().findChildByType(TypeScriptTypes.TYPE_PARAMETER_LIST);
        if (list == null) {
            return true;
        }
        for (ASTNode child = list.getFirstChildNode(); child != null; child = child.getTreeNext()) {
            if (child.getElementType() == TypeScriptTypes.TYPE_PARAMETER && !processor.execute(child.getPsi(), state)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Declarations that only name a type: interfaces, type aliases, type parameters.
     */
    public static boolean isTypeOnly(PsiElement element) {
        if (element instanceof JSClass jsClass && jsClass.isInterface() || element instanceof JSTypeOnlyDeclaration) {
            return true;
        }
        // type parameters aren't stubbed: a declaration of another file (by the index) isn't parsed
        if (element instanceof StubBasedPsiElement<?>) {
            return false;
        }
        IElementType type = element.getNode() == null ? null : element.getNode().getElementType();
        return type == TypeScriptTypes.TYPE_ALIAS_DECLARATION || type == TypeScriptTypes.TYPE_PARAMETER;
    }

    /**
     * Declarations that can name a type: classes, interfaces, enums, type aliases, namespaces, type parameters.
     */
    public static boolean isTypeDeclaration(PsiElement element) {
        if (element instanceof JSClass || element instanceof JSTypeOnlyDeclaration) {
            return true;
        }
        // the other type declarations aren't stubbed
        if (element instanceof StubBasedPsiElement<?>) {
            return false;
        }
        IElementType type = element.getNode() == null ? null : element.getNode().getElementType();
        return type == TypeScriptTypes.TYPE_ALIAS_DECLARATION || type == TypeScriptTypes.TYPE_PARAMETER
            || type == TypeScriptTypes.ENUM_DECLARATION || type == TypeScriptTypes.NAMESPACE_DECLARATION;
    }

    /**
     * An enum or a namespace (a container of qualified members). Neither is stubbed: a declaration read from its stub -
     * the global variable of another file - isn't one, its AST isn't loaded to tell.
     */
    @RequiredReadAction
    public static boolean isEnumOrNamespace(PsiElement element) {
        if (element instanceof StubBasedPsiElement<?>) {
            return false;
        }
        IElementType type = element.getNode() == null ? null : element.getNode().getElementType();
        return type == TypeScriptTypes.ENUM_DECLARATION || type == TypeScriptTypes.NAMESPACE_DECLARATION;
    }

    /**
     * The members of a container reached through a qualifier: the declarations of a namespace body, the members of an
     * enum, the exports of a module.
     */
    @RequiredReadAction
    public static List<PsiElement> findQualifiedMembers(PsiElement container, @Nullable String name) {
        if (container instanceof PsiFile module) {
            return name == null ? TypeScriptModuleUtil.findAllExports(module) : TypeScriptModuleUtil.findExports(module, name);
        }
        if (container instanceof JSClass namespace && isNamespace(namespace)) {
            return name == null ? findAllMembers(namespace) : findMembers(namespace, name);
        }
        IElementType type = container.getNode().getElementType();
        List<PsiElement> result = new ArrayList<>();
        if (type == TypeScriptTypes.ENUM_DECLARATION) {
            for (PsiElement child = container.getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child.getNode().getElementType() == TypeScriptTypes.ENUM_MEMBER
                    && (name == null || name.equals(((PsiNamedElement) child).getName()))) {
                    result.add(child);
                }
            }
        }
        return result;
    }

    /**
     * The qualified name of a declaration: prefixed by the names of its enclosing namespaces ({@code Intl.DateTimeFormat}),
     * by the stub parents when there are.
     */
    @Nullable
    @RequiredReadAction
    public static String qualifyByNamespaces(PsiElement declaration, @Nullable String name) {
        if (name == null) {
            return null;
        }
        StringBuilder result = new StringBuilder(name);
        for (PsiElement parent = PsiTreeUtil.getStubOrPsiParent(declaration); parent != null && !(parent instanceof PsiFile);
             parent = PsiTreeUtil.getStubOrPsiParent(parent)) {
            if (parent instanceof JSClass namespace && isNamespace(namespace) && namespace.getName() != null) {
                result.insert(0, namespace.getName() + ".");
            }
        }
        return result.toString();
    }

    /**
     * {@code namespace Intl { ... }}: stubbed like a class (by its stub element type, without the AST).
     */
    public static boolean isNamespace(PsiElement element) {
        return element instanceof StubBasedPsiElementBase<?> stubBased && stubBased.getElementType() == TypeScriptStubElementTypes.NAMESPACE_DECLARATION;
    }

    /**
     * The declarations of the body of a namespace, from its AST (a namespace without a stub).
     */
    @RequiredReadAction
    private static List<PsiElement> getNamespaceMembersByTree(JSClass namespace, @Nullable String name) {
        List<PsiElement> result = new ArrayList<>();
        ASTNode body = namespace.getNode().findChildByType(TypeScriptTypes.BLOCK_STATEMENT);
        if (body != null) {
            ResolveProcessor processor = new ResolveProcessor(name, namespace);
            for (PsiElement child = body.getPsi().getFirstChild(); child != null; child = child.getNextSibling()) {
                child.processDeclarations(processor, ResolveState.initial(), null, namespace);
            }
            for (ResolveResult resolveResult : processor.getResultsAsResolveResults()) {
                result.add(resolveResult.getElement());
            }
        }
        return result;
    }

    /**
     * All members of a class and of its super classes (completion).
     */
    @RequiredReadAction
    public static List<PsiElement> findAllMembers(JSClass jsClass) {
        List<PsiElement> result = new ArrayList<>();
        Set<JSClass> visited = new HashSet<>();
        List<JSClass> queue = new ArrayList<>(List.of(jsClass));
        while (!queue.isEmpty()) {
            JSClass current = queue.remove(0);
            if (visited.add(current)) {
                for (JSClass declaration : getMergedDeclarations(current)) {
                    if (declaration == current || visited.add(declaration)) {
                        result.addAll(getClassMembers(declaration));
                        queue.addAll(getSuperClasses(declaration));
                    }
                }
            }
        }
        return result;
    }

    /**
     * JavaScript's member lookup on a class ({@code new Date().getTime()} in a .js file: JSReferenceExpressionImpl
     * evaluates the class of the qualifier, then calls {@code processDeclarations} with the class as lastParent). A walk
     * up the tree never passes the element itself as lastParent.
     */
    @RequiredReadAction
    public static boolean isMemberLookup(JSClass jsClass, @Nullable PsiElement lastParent) {
        return lastParent == jsClass;
    }

    @RequiredReadAction
    public static boolean processMembers(JSClass jsClass, PsiScopeProcessor processor, ResolveState state) {
        // one name: the members by the stubs; all of them (completion) from the AST
        String name = processor instanceof ResolveProcessor resolveProcessor ? resolveProcessor.getName() : null;
        for (PsiElement member : name != null ? findMembers(jsClass, name) : findAllMembers(jsClass)) {
            if (!processor.execute(member, state)) {
                return false;
            }
        }
        return true;
    }
}
