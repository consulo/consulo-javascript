package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSArrayLiteralExpression;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSClassExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.lang.javascript.psi.JSNewExpression;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.TypeScriptDeclaredTypes;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.impl.language.controlFlow.JavaScriptControlFlow;
import consulo.javascript.impl.language.controlFlow.JavaScriptFlowVariables;
import consulo.language.ast.IElementType;
import consulo.language.editor.rawHighlight.HighlightDisplayLevel;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.localize.LocalizeValue;
import org.jspecify.annotations.Nullable;

import java.util.Map;

/**
 * A variable read where it may be unassigned (TS2454) - the definite assignment of the control flow graph, reported as
 * tsc does in strict mode ({@code checkIdentifier}): not for a parameter, a read asserted non-null ({@code x!}), a
 * variable asserted assigned ({@code let x!: T}), nor when the type of the variable accepts undefined - its annotation
 * ({@link TypeScriptDeclaredTypes}), or the literal, the array, the object, the new it is initialized with; without
 * either the type evolves from the assignments ({@code let x;}) and nothing is reported, as for a name of a destructuring
 *
 * @author VISTALL
 */
@ExtensionImpl
public class TypeScriptUsedBeforeAssignedInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.typescriptUsedBeforeAssignedDisplayName();
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
            if (!JavaScriptControlFlow.isOwner(element)) {
                return;
            }
            JavaScriptControlFlow flow = JavaScriptControlFlow.of(element);
            if (flow == null) {
                return;
            }
            for (Map.Entry<PsiElement, PsiElement> read : flow.getDefiniteAssignment().getUnassignedReads().entrySet()) {
                PsiElement reference = read.getKey();
                PsiElement variable = read.getValue();
                if (isReported(reference, variable)) {
                    registerError(reference, InspectionJSLocalize.variableUsedBeforeAssignedErrorString(JavaScriptFlowVariables.name(variable)));
                }
            }
        }
    }

    @RequiredReadAction
    private static boolean isReported(PsiElement reference, PsiElement variable) {
        if (!(variable instanceof JSVariable declaration) || JavaScriptFlowVariables.kind(variable) == JavaScriptFlowVariables.Kind.PARAMETER) {
            return false;
        }
        // x! - x is asserted non-null; let x!: number
        PsiElement next = PsiTreeUtil.nextVisibleLeaf(reference);
        if (PsiUtilCore.getElementType(next) == JSTokenTypes.EXCL && next.getParent() == reference.getParent()
            || declaration.getNode().findChildByType(JSTokenTypes.EXCL) != null) {
            return false;
        }
        PsiElement type = TypeScriptDeclaredTypes.declaredType(declaration);
        if (type != null) {
            TypeScriptDeclaredTypes.Kind kind = TypeScriptDeclaredTypes.kind(type);
            return kind == TypeScriptDeclaredTypes.Kind.VALUE_REQUIRED || kind == TypeScriptDeclaredTypes.Kind.NEVER;
        }
        return hasValueType(declaration.getInitializer());
    }

    /**
     * Its type does not accept undefined: a literal but undefined, a template, an array, an object, a new, a function, a
     * class
     */
    private static boolean hasValueType(@Nullable JSExpression initializer) {
        if (initializer instanceof JSLiteralExpression) {
            return !"undefined".equals(initializer.getText());
        }
        IElementType first = PsiUtilCore.getElementType(initializer == null ? null : initializer.getFirstChild());
        return initializer instanceof JSArrayLiteralExpression || initializer instanceof JSObjectLiteralExpression || initializer instanceof JSNewExpression
            || initializer instanceof JSFunction || initializer instanceof JSClass || initializer instanceof JSClassExpression || first == JSTokenTypes.BACKQUOTE;
    }
}
