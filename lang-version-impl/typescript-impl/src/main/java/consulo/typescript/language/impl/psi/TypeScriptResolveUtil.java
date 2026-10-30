package consulo.typescript.language.impl.psi;

import com.intellij.lang.javascript.psi.JSReferenceList;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSNewExpression;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.impl.ast.LeafElement;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
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
            else if (child instanceof JSVariable) {
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
        if (extendsList != null) {
            for (JSReferenceExpression reference : extendsList.getExpressions()) {
                if (reference.resolve() instanceof JSClass superClass) {
                    result.add(superClass);
                }
            }
        }
        return result;
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
        for (PsiElement member : getClassMembers(jsClass)) {
            if (name.equals(((PsiNamedElement) member).getName())) {
                result.add(member);
            }
        }
        if (result.isEmpty()) {
            for (JSClass superClass : getSuperClasses(jsClass)) {
                collectMembers(superClass, name, result, visited);
            }
        }
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
            return constructor instanceof JSReferenceExpression reference ? evaluateClass(reference.resolve(), visited) : null;
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
            return initializer instanceof JSNewExpression ? evaluateClass(initializer, visited) : null;
        }
        return null;
    }

    /**
     * {@code x: Foo} - the class named by a simple type annotation.
     */
    @Nullable
    @RequiredReadAction
    private static JSClass resolveTypeAnnotationClass(JSVariable variable) {
        ASTNode annotation = variable.getNode().findChildByType(TypeScriptTypes.TYPE_ANNOTATION);
        ASTNode typeReference = annotation == null ? null : annotation.findChildByType(TypeScriptTypes.TYPE_REFERENCE);
        ASTNode qualifiedName = typeReference == null ? null : typeReference.findChildByType(TypeScriptTypes.QUALIFIED_NAME);
        if (qualifiedName == null || qualifiedName.getFirstChildNode() != qualifiedName.getLastChildNode()) {
            return null;
        }
        ResolveProcessor processor = new ResolveProcessor(qualifiedName.getText(), variable);
        JSResolveUtil.treeWalkUp(processor, variable, variable, variable);
        return processor.getResult() instanceof JSClass jsClass ? jsClass : null;
    }

    /**
     * Whether a member is {@code static}.
     */
    @RequiredReadAction
    public static boolean isStatic(PsiElement member) {
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
        if (element instanceof JSClass jsClass && jsClass.isInterface()) {
            return true;
        }
        IElementType type = element.getNode() == null ? null : element.getNode().getElementType();
        return type == TypeScriptTypes.TYPE_ALIAS_DECLARATION || type == TypeScriptTypes.TYPE_PARAMETER;
    }

    /**
     * Declarations that can name a type: classes, interfaces, enums, type aliases, namespaces, type parameters.
     */
    public static boolean isTypeDeclaration(PsiElement element) {
        if (element instanceof JSClass) {
            return true;
        }
        IElementType type = element.getNode() == null ? null : element.getNode().getElementType();
        return type == TypeScriptTypes.TYPE_ALIAS_DECLARATION || type == TypeScriptTypes.TYPE_PARAMETER
            || type == TypeScriptTypes.ENUM_DECLARATION || type == TypeScriptTypes.NAMESPACE_DECLARATION;
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
        else if (type == TypeScriptTypes.NAMESPACE_DECLARATION) {
            ASTNode body = container.getNode().findChildByType(TypeScriptTypes.BLOCK_STATEMENT);
            if (body != null) {
                ResolveProcessor processor = new ResolveProcessor(name, container);
                for (PsiElement child = body.getPsi().getFirstChild(); child != null; child = child.getNextSibling()) {
                    child.processDeclarations(processor, ResolveState.initial(), null, container);
                }
                for (ResolveResult resolveResult : processor.getResultsAsResolveResults()) {
                    result.add(resolveResult.getElement());
                }
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
                result.addAll(getClassMembers(current));
                queue.addAll(getSuperClasses(current));
            }
        }
        return result;
    }
}
