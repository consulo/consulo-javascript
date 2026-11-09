package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSGenericSignature;
import com.intellij.lang.javascript.psi.JSReturnStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.impl.language.controlFlow.JavaScriptControlFlow;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.javascript.psi.JSTypeOnlyDeclaration;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.editor.rawHighlight.HighlightDisplayLevel;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNameIdentifierOwner;
import consulo.language.psi.PsiReference;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.localize.LocalizeValue;
import org.jspecify.annotations.Nullable;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Set;

/**
 * The end of a function body with a declared return type is reachable - the checks of tsc 6
 * ({@code checkAllCodePathsInNonVoidFunctionReturnOrThrow}, {@code checkAccessorDeclaration}), strict mode:
 * <ul>
 * <li>TS2534 the type is {@code never}</li>
 * <li>TS2355 the type is neither {@code undefined}, {@code void} nor {@code any}, and the function has no return</li>
 * <li>TS2366 it has one and {@code undefined} is not assignable to the type</li>
 * <li>TS2378 a getter without a return, whatever its type</li>
 * </ul>
 * The type is read from the annotation, the type of an async function from {@code Promise<T>}: the primitives, literal,
 * object, tuple, array and function types, the classes, interfaces and enums, the type parameters, the aliases of
 * these, TReturn of {@code Generator<T, TReturn>}; anything else (another type reference, an intersection, a
 * conditional type) is not checked. tsc also knows a call of a function returning {@code never} and an exhaustive switch over a union end
 * the flow - these need types, here the end stays reachable.
 *
 * @author VISTALL
 */
@ExtensionImpl
public class TypeScriptFunctionReturnInspection extends JavaScriptInspection {
    /**
     * What a declared return type allows at the end of the body
     */
    private enum Kind {
        // void, any, undefined - nothing is reported
        NOTHING_REQUIRED,
        // unknown, a union with undefined - a return is required
        UNDEFINED_ACCEPTED,
        NEVER,
        // undefined is not assignable
        VALUE_REQUIRED,
        // not known here
        UNKNOWN
    }

    // the types of the standard library that do not accept undefined, known without resolving them
    private static final Set<String> LIBRARY_TYPES = Set.of("Array", "ReadonlyArray", "Map", "Set", "WeakMap", "WeakSet", "Date", "RegExp", "Error",
        "Function", "Object", "String", "Number", "Boolean", "Symbol", "BigInt", "Promise", "Record", "Partial", "Required", "Readonly", "Pick", "Omit");

    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.typescriptFunctionReturnDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Override
    public HighlightDisplayLevel getDefaultLevel() {
        return HighlightDisplayLevel.ERROR;
    }

    @Override
    public boolean canBuildVisitor(PsiFile psiFile) {
        return "TypeScript".equals(psiFile.getLanguage().getID());
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return ((LocalizeValue) args[0]).get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        @RequiredReadAction
        public void visitElement(PsiElement element) {
            super.visitElement(element);
            if (!(element instanceof JSFunction) && !JavaScriptControlFlow.isPropertyMethod(element)
                || !(JavaScriptControlFlow.getBody(element) instanceof JSBlockStatement)) {
                return;
            }
            JSFunction getter = element instanceof JSFunction function && function.isGetProperty() ? function : null;
            PsiElement typeElement = returnType(element);
            if (typeElement == null && getter == null) {
                return;
            }
            JavaScriptControlFlow flow = JavaScriptControlFlow.of(element);
            if (flow == null || !flow.isEndReachable()) {
                return;
            }

            boolean explicitReturn = hasReturn(element);
            if (getter != null && !explicitReturn) {
                PsiElement name = getter.getNameIdentifier();
                registerError(name != null ? name : getter, InspectionJSLocalize.getAccessorMustReturnValueErrorString());
            }
            if (typeElement == null) {
                return;
            }

            Kind kind = isGenerator(element) ? generated(typeElement) : isAsync(element) ? promised(typeElement) : kind(typeElement, 0);
            LocalizeValue message = null;
            if (kind == Kind.NEVER) {
                message = InspectionJSLocalize.functionReturningNeverErrorString();
            }
            else if ((kind == Kind.VALUE_REQUIRED || kind == Kind.UNDEFINED_ACCEPTED) && !explicitReturn) {
                message = InspectionJSLocalize.functionMustReturnValueErrorString();
            }
            else if (kind == Kind.VALUE_REQUIRED) {
                message = InspectionJSLocalize.functionLacksEndingReturnErrorString();
            }
            if (message != null) {
                // the type, without the colon of the annotation
                PsiElement start = typeElement.getFirstChild() != null && PsiUtilCore.getElementType(typeElement.getFirstChild()) == JSTokenTypes.COLON
                    ? PsiTreeUtil.skipWhitespacesAndCommentsForward(typeElement.getFirstChild())
                    : typeElement;
                registerError(start, typeElement, ProblemHighlightType.GENERIC_ERROR_OR_WARNING, message);
            }
        }
    }

    /**
     * The type of {@code : T} after the parameters - not the annotation with the colon
     */
    @RequiredReadAction
    private static @Nullable PsiElement returnType(PsiElement function) {
        for (PsiElement child = function.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JavaScriptTypeElement annotation && PsiUtilCore.getElementType(annotation.getFirstChild()) == JSTokenTypes.COLON) {
                List<PsiElement> types = types(annotation);
                // : T[] - the brackets are in the annotation
                return types.size() == 1 && !isArray(annotation) ? types.get(0) : annotation;
            }
        }
        return null;
    }

    private static boolean isGenerator(PsiElement function) {
        return hasChild(function, JSTokenTypes.MULT);
    }

    private static boolean isAsync(PsiElement function) {
        for (PsiElement child = function.getFirstChild(); child != null; child = child.getNextSibling()) {
            // in the function or in its attribute list
            if (PsiUtilCore.getElementType(child) == JSTokenTypes.ASYNC_KEYWORD || child instanceof JSAttributeList && hasChild(child, JSTokenTypes.ASYNC_KEYWORD)) {
                return true;
            }
        }
        return false;
    }

    /**
     * T[] - [ and ] with nothing between
     */
    private static boolean isArray(PsiElement type) {
        for (PsiElement child = type.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (PsiUtilCore.getElementType(child) == JSTokenTypes.LBRACKET) {
                PsiElement next = PsiTreeUtil.skipWhitespacesAndCommentsForward(child);
                if (PsiUtilCore.getElementType(next) == JSTokenTypes.RBRACKET) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * A return statement of the function - not of a nested one
     */
    @RequiredReadAction
    private static boolean hasReturn(PsiElement function) {
        Deque<PsiElement> stack = new ArrayDeque<>();
        stack.push(function);
        while (!stack.isEmpty()) {
            for (PsiElement child = stack.pop().getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child instanceof JSReturnStatement) {
                    return true;
                }
                if (child.getFirstChild() != null && !JavaScriptControlFlow.isOwner(child) && !(child instanceof JSClass)) {
                    stack.push(child);
                }
            }
        }
        return false;
    }

    /**
     * Promise&lt;T&gt; of an async function - T
     */
    @RequiredReadAction
    private static Kind promised(PsiElement type) {
        List<PsiElement> arguments = "Promise".equals(referenceName(type)) ? typeArguments(type) : List.of();
        return arguments.size() == 1 ? kind(arguments.get(0), 0) : Kind.UNKNOWN;
    }

    /**
     * Generator&lt;T, TReturn, TNext&gt; of a generator - TReturn; any if not given, as of the iterator types
     */
    @RequiredReadAction
    private static Kind generated(PsiElement type) {
        String name = referenceName(type);
        if (!"Generator".equals(name) && !"AsyncGenerator".equals(name)) {
            return Kind.UNKNOWN;
        }
        List<PsiElement> arguments = typeArguments(type);
        return arguments.size() < 2 ? Kind.NOTHING_REQUIRED : kind(arguments.get(1), 0);
    }

    private static List<PsiElement> typeArguments(PsiElement type) {
        for (PsiElement child = type.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (!(child instanceof JavaScriptTypeElement) && child.getFirstChild() != null && PsiUtilCore.getElementType(child.getFirstChild()) == JSTokenTypes.LT) {
                return types(child);
            }
        }
        return List.of();
    }

    @RequiredReadAction
    private static Kind kind(PsiElement type, int depth) {
        if (depth > 5) {
            return Kind.UNKNOWN;
        }
        List<PsiElement> members = types(type);
        if (hasChild(type, JSTokenTypes.OR)) {
            return union(members, depth);
        }
        if (hasChild(type, JSTokenTypes.AND)) {
            return Kind.UNKNOWN;
        }
        // T[], [a, b], { a: number }, () => void, `a${b}` - not T['a']
        IElementType first = PsiUtilCore.getElementType(type.getFirstChild());
        if (isArray(type) || first == JSTokenTypes.LBRACKET || first == JSTokenTypes.LBRACE || hasChild(type, JSTokenTypes.DARROW)
            || first == JSTokenTypes.BACKQUOTE) {
            return Kind.VALUE_REQUIRED;
        }
        if (hasChild(type, JSTokenTypes.LBRACKET)) {
            return Kind.UNKNOWN;
        }
        // (T)
        if (first == JSTokenTypes.LPAR && members.size() == 1) {
            return kind(members.get(0), depth + 1);
        }
        // asserts x, asserts x is T - void; x is T - boolean
        if ("asserts".equals(type.getFirstChild() == null ? null : type.getFirstChild().getText())) {
            return Kind.NOTHING_REQUIRED;
        }
        if (hasWord(type, "is")) {
            return Kind.VALUE_REQUIRED;
        }
        // 1, 'a', true, null
        if (type.getFirstChild() != null && type.getFirstChild() == type.getLastChild() && type.getFirstChild().getFirstChild() == null) {
            switch (type.getText()) {
                case "void", "any", "undefined":
                    return Kind.NOTHING_REQUIRED;
                case "unknown":
                    return Kind.UNDEFINED_ACCEPTED;
                case "never":
                    return Kind.NEVER;
                default:
                    return Kind.VALUE_REQUIRED;
            }
        }
        return reference(type, depth);
    }

    /**
     * void or any in it - nothing required; never - gone; undefined or unknown - accepted
     */
    @RequiredReadAction
    private static Kind union(List<PsiElement> members, int depth) {
        boolean accepted = false;
        boolean unknown = false;
        boolean value = false;
        for (PsiElement member : members) {
            Kind kind = kind(member, depth + 1);
            switch (kind) {
                case NOTHING_REQUIRED -> {
                    if (!"undefined".equals(member.getText())) {
                        return Kind.NOTHING_REQUIRED;
                    }
                    accepted = true;
                }
                case UNDEFINED_ACCEPTED -> accepted = true;
                case UNKNOWN -> unknown = true;
                case VALUE_REQUIRED -> value = true;
                case NEVER -> {
                }
            }
        }
        if (unknown) {
            return Kind.UNKNOWN;
        }
        if (accepted) {
            return Kind.UNDEFINED_ACCEPTED;
        }
        return value ? Kind.VALUE_REQUIRED : Kind.NEVER;
    }

    /**
     * A type reference: number, X of a type parameter, a class, an interface, an enum, an alias
     */
    @RequiredReadAction
    private static Kind reference(PsiElement type, int depth) {
        String name = referenceName(type);
        if (name == null) {
            return Kind.UNKNOWN;
        }
        switch (name) {
            case "void", "any", "undefined":
                return Kind.NOTHING_REQUIRED;
            case "unknown":
                return Kind.UNDEFINED_ACCEPTED;
            case "never":
                return Kind.NEVER;
            case "number", "string", "boolean", "bigint", "symbol", "object", "null":
                return Kind.VALUE_REQUIRED;
        }
        if (isTypeParameter(type, name) || LIBRARY_TYPES.contains(name)) {
            return Kind.VALUE_REQUIRED;
        }
        PsiElement qualifiedName = type.getFirstChild();
        PsiReference reference = qualifiedName == null ? null : qualifiedName.getReference();
        PsiElement target = reference == null ? null : reference.resolve();
        if (target instanceof JSClass || target != null && hasChild(target, JSTokenTypes.ENUM_KEYWORD)) {
            return Kind.VALUE_REQUIRED;
        }
        if (target instanceof JSTypeOnlyDeclaration) {
            // type T = ... - its type
            List<PsiElement> types = types(target);
            return types.isEmpty() ? Kind.UNKNOWN : kind(types.get(types.size() - 1), depth + 1);
        }
        return Kind.UNKNOWN;
    }

    /**
     * The name of a reference without a qualifier, with or without type arguments: {@code Promise<T>} - Promise
     */
    @RequiredReadAction
    private static @Nullable String referenceName(PsiElement type) {
        PsiElement name = type.getFirstChild();
        if (name == null || name.getFirstChild() == null || name.getFirstChild() != name.getLastChild()
            || PsiUtilCore.getElementType(name.getFirstChild()) != JSTokenTypes.IDENTIFIER) {
            return null;
        }
        return name.getText();
    }

    /**
     * Declared by a type parameter list of an enclosing function, class or alias
     */
    @RequiredReadAction
    private static boolean isTypeParameter(PsiElement type, String name) {
        for (PsiElement parent = type.getParent(); parent != null && !(parent instanceof PsiFile); parent = parent.getParent()) {
            for (PsiElement child = parent.getFirstChild(); child != null; child = child.getNextSibling()) {
                if (!(child instanceof JSGenericSignature)) {
                    continue;
                }
                for (PsiElement parameter = child.getFirstChild(); parameter != null; parameter = parameter.getNextSibling()) {
                    PsiElement identifier = parameter instanceof PsiNameIdentifierOwner owner ? owner.getNameIdentifier() : parameter.getFirstChild();
                    if (identifier != null && name.equals(identifier.getText())) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    private static List<PsiElement> types(PsiElement element) {
        List<PsiElement> types = new ArrayList<>();
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JavaScriptTypeElement) {
                types.add(child);
            }
        }
        return types;
    }

    /**
     * A keyword token among the children - a contextual one of TypeScript has its own element type
     */
    private static boolean hasWord(PsiElement element, String word) {
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child.getFirstChild() == null && word.equals(child.getText())) {
                return true;
            }
        }
        return false;
    }

    private static boolean hasChild(PsiElement element, IElementType type) {
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (PsiUtilCore.getElementType(child) == type) {
                return true;
            }
        }
        return false;
    }
}
