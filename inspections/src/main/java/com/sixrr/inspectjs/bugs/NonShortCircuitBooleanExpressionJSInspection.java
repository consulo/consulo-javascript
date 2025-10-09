package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class NonShortCircuitBooleanExpressionJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
        public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.nonShortCircuitBooleanExpressionDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.nonShortCircuitBooleanExpressionErrorString().get();
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return new NonShortCircuitBooleanFix();
    }

    private static class NonShortCircuitBooleanFix extends InspectionJSFix {
        @Nonnull
        @Override
        public LocalizeValue getName() {
            return InspectionJSLocalize.replaceWithShortCircuitExpressionFixString();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final JSBinaryExpression expression = (JSBinaryExpression)descriptor.getPsiElement();
            final JSExpression lhs = expression.getLOperand();
            final JSExpression rhs = expression.getROperand();
            final IElementType operationSign = expression.getOperationSign();
            assert rhs != null;
            final String newExpression = lhs.getText() + getShortCircuitOperand(operationSign) + rhs.getText();
            replaceExpression(expression, newExpression);
        }

        private static String getShortCircuitOperand(IElementType tokenType) {
            return JSTokenTypes.AND.equals(tokenType) ? "&&" : "||";
        }
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new NonShortCircuitBooleanVisitor();
    }

    private static class NonShortCircuitBooleanVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            if (!(expression.getROperand() != null)) {
                return;
            }
            final IElementType sign = expression.getOperationSign();
            if (!JSTokenTypes.AND.equals(sign) && !JSTokenTypes.OR.equals(sign)) {
                return;
            }
            registerError(expression);
        }
    }
}