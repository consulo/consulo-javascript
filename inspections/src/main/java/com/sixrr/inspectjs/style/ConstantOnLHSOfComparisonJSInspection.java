package com.sixrr.inspectjs.style;

import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ComparisonUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;

import jakarta.annotation.Nonnull;

@ExtensionImpl
public class ConstantOnLHSOfComparisonJSInspection extends JavaScriptInspection {
    private final SwapComparisonFix fix = new SwapComparisonFix();

    @Override
    @Nonnull
    public String getID() {
        return "ConstantOnLefSideOfComparisonJS";
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.constantOnLeftSideOfComparisonDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.STYLE_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.constantOnLeftSideOfComparisonErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new ConstantOnRHSOfComparisonVisitor();
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class SwapComparisonFix extends InspectionJSFix {
        @Override
        @Nonnull
        public String getName() {
            return InspectionJSLocalize.flipComparisonFix().get();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final JSBinaryExpression expression = (JSBinaryExpression)descriptor.getPsiElement();
            final JSExpression rhs = expression.getROperand();
            final JSExpression lhs = expression.getLOperand();
            final IElementType sign = expression.getOperationSign();
            assert rhs != null;
            final String rhsText = rhs.getText();
            final String flippedComparison = ComparisonUtils.getFlippedComparison(sign);
            final String lhsText = lhs.getText();
            replaceExpression(expression, rhsText + ' ' + flippedComparison + ' ' + lhsText);
        }
    }

    private static class ConstantOnRHSOfComparisonVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            if (!(expression.getROperand() != null)) {
                return;
            }
            if (!ComparisonUtils.isComparison(expression)) {
                return;
            }
            final JSExpression lhs = expression.getLOperand();
            final JSExpression rhs = expression.getROperand();
            if (!(lhs instanceof JSLiteralExpression) || rhs instanceof JSLiteralExpression) {
                return;
            }
            registerError(expression);
        }
    }
}
