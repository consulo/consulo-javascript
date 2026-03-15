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
import consulo.localize.LocalizeValue;
import consulo.project.Project;

@ExtensionImpl
public class ConditionalExpressionWithIdenticalBranchesJSInspection extends JavaScriptInspection {
    private InspectionJSFix fix = new CollapseConditional();

    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.conditionalExpressionWithIdenticalBranchesDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
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
        public LocalizeValue getName() {
            return InspectionJSLocalize.collapseConditionalExpressionFix();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            JSConditionalExpression expression = (JSConditionalExpression) descriptor.getPsiElement();

            JSExpression thenExpression = expression.getThen();
            assert thenExpression != null;
            String bodyText = thenExpression.getText();
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
            JSExpression thenExpression = expression.getThen();
            JSExpression elseExpression = expression.getElse();
            if (EquivalenceChecker.expressionsAreEquivalent(thenExpression, elseExpression)) {
                registerError(expression);
            }
        }
    }
}