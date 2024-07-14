package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.BoolUtils;
import com.sixrr.inspectjs.utils.ParenthesesUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class NegatedConditionalExpressionJSInspection extends JavaScriptInspection {
    private final NegatedConditionalFix fix = new NegatedConditionalFix();

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.negatedConditionalExpressionDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @RequiredReadAction
    @Override
    @Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.negatedConditionalExpressionErrorString().get();
    }

    @Override
    protected InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class NegatedConditionalFix extends InspectionJSFix {
        @Override
        @Nonnull
        public String getName() {
            return InspectionJSLocalize.invertConditionFix().get();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final JSConditionalExpression exp = (JSConditionalExpression) descriptor.getPsiElement();
            assert exp != null;
            final JSExpression elseBranch = exp.getElse();
            final JSExpression thenBranch = exp.getThen();
            final JSExpression condition = exp.getCondition();
            final String negatedCondition = BoolUtils.getNegatedExpressionText(condition);
            assert elseBranch != null;
            assert thenBranch != null;
            final String newStatement = negatedCondition + '?' + elseBranch.getText() + ':' + thenBranch.getText();
            replaceExpression(exp, newStatement);
        }
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSConditionalExpression(JSConditionalExpression exp) {
            super.visitJSConditionalExpression(exp);
            JSExpression condition = exp.getCondition();
            condition = ParenthesesUtils.stripExpression(condition);
            if (!BoolUtils.isNegation(condition) && !isNotEquals(condition)) {
                return;
            }
            registerError(exp);
        }

        private boolean isNotEquals(JSExpression expression) {
            if (!(expression instanceof JSBinaryExpression)) {
                return false;
            }
            final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;
            final IElementType sign = binaryExpression.getOperationSign();
            return JSTokenTypes.NE.equals(sign) ||JSTokenTypes.NEQEQ.equals(sign);
        }
    }
}
