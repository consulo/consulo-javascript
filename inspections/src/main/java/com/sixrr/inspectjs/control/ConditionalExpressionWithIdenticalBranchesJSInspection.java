package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.EquivalenceChecker;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class ConditionalExpressionWithIdenticalBranchesJSInspection extends JavaScriptInspection {
    private InspectionJSFix fix = new CollapseConditional();

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.conditionalExpressionWithIdenticalBranchesDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.conditionalExpressionWithIdenticalBranchesErrorString().get();
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class CollapseConditional extends InspectionJSFix {
        @Override
        @Nonnull
        public String getName() {
            return InspectionJSLocalize.collapseConditionalExpressionFix().get();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final JSConditionalExpression expression = (JSConditionalExpression) descriptor.getPsiElement();

            final JSExpression thenExpression = expression.getThen();
            assert thenExpression != null;
            final String bodyText = thenExpression.getText();
            replaceExpression(expression, bodyText);
        }
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new ConditionalExpressionWithIdenticalBranchesVisitor();
    }

    private static class ConditionalExpressionWithIdenticalBranchesVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSConditionalExpression(JSConditionalExpression expression) {
            super.visitJSConditionalExpression(expression);
            final JSExpression thenExpression = expression.getThen();
            final JSExpression elseExpression = expression.getElse();
            if (EquivalenceChecker.expressionsAreEquivalent(thenExpression, elseExpression)) {
                registerError(expression);
            }
        }
    }
}