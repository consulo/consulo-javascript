package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSReturnStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.TypeScriptDeclaredTypes;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.impl.language.controlFlow.JavaScriptControlFlow;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.editor.rawHighlight.HighlightDisplayLevel;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.localize.LocalizeValue;
import org.jspecify.annotations.Nullable;

import java.util.ArrayDeque;
import java.util.Deque;

/**
 * The end of a function body with a declared return type is reachable - the checks of tsc 6
 * ({@code checkAllCodePathsInNonVoidFunctionReturnOrThrow}, {@code checkAccessorDeclaration}), strict mode:
 * <ul>
 * <li>TS2534 the type is {@code never}</li>
 * <li>TS2355 the type is neither {@code undefined}, {@code void} nor {@code any}, and the function has no return</li>
 * <li>TS2366 it has one and {@code undefined} is not assignable to the type</li>
 * <li>TS2378 a getter without a return, whatever its type</li>
 * </ul>
 * The type is read from the annotation ({@link TypeScriptDeclaredTypes}), of an async function from {@code Promise<T>},
 * of a generator from TReturn of {@code Generator<T, TReturn>}; a type not known there is not checked. tsc also knows a
 * call of a function returning {@code never} and an exhaustive switch over a union end the flow - these need types,
 * here the end stays reachable.
 *
 * @author VISTALL
 */
@ExtensionImpl
public class TypeScriptFunctionReturnInspection extends JavaScriptInspection {
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
            PsiElement typeElement = TypeScriptDeclaredTypes.declaredType(element);
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

            TypeScriptDeclaredTypes.Kind kind = isGenerator(element) ? TypeScriptDeclaredTypes.generated(typeElement)
                : isAsync(element) ? TypeScriptDeclaredTypes.promised(typeElement) : TypeScriptDeclaredTypes.kind(typeElement);
            LocalizeValue message = null;
            if (kind == TypeScriptDeclaredTypes.Kind.NEVER) {
                message = InspectionJSLocalize.functionReturningNeverErrorString();
            }
            else if ((kind == TypeScriptDeclaredTypes.Kind.VALUE_REQUIRED || kind == TypeScriptDeclaredTypes.Kind.UNDEFINED_ACCEPTED) && !explicitReturn) {
                message = InspectionJSLocalize.functionMustReturnValueErrorString();
            }
            else if (kind == TypeScriptDeclaredTypes.Kind.VALUE_REQUIRED) {
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

    private static boolean hasChild(PsiElement element, IElementType type) {
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (PsiUtilCore.getElementType(child) == type) {
                return true;
            }
        }
        return false;
    }
}
