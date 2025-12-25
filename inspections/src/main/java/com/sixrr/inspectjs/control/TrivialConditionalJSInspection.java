package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.BoolUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import org.intellij.lang.annotations.Pattern;
import org.jetbrains.annotations.NonNls;

@ExtensionImpl
public class TrivialConditionalJSInspection extends JavaScriptInspection {
    private final TrivialConditionalFix fix = new TrivialConditionalFix();

    @Nonnull
    @Override
    @Pattern(value = "[a-zA-Z_0-9.-]+")
    public String getID() {
        return "RedundantConditionalExpressionJS";
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.redundantConditionalExpressionDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new UnnecessaryConditionalExpressionVisitor();
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        JSConditionalExpression exp = (JSConditionalExpression) args[0];
        return InspectionJSLocalize.trivialConditionalErrorString(exp.getText(), calculateReplacementExpression(exp)).get();
    }

    private static String calculateReplacementExpression(JSConditionalExpression exp) {
        JSExpression thenExpression = exp.getThen();
        JSExpression elseExpression = exp.getElse();
        JSExpression condition = exp.getCondition();

        if (isFalse(thenExpression) && isTrue(elseExpression)) {
            return BoolUtils.getNegatedExpressionText(condition);
        } else {
            return condition.getText();
        }
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class TrivialConditionalFix extends InspectionJSFix {
        @Nonnull
        @Override
        public LocalizeValue getName() {
            return InspectionJSLocalize.simplifyFix();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            JSConditionalExpression expression = (JSConditionalExpression) descriptor.getPsiElement();
            String newExpression = calculateReplacementExpression(expression);
            replaceExpression(expression, newExpression);
        }
    }

    private static class UnnecessaryConditionalExpressionVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSConditionalExpression(JSConditionalExpression exp) {
            super.visitJSConditionalExpression(exp);
            JSExpression thenExpression = exp.getThen();
            if (thenExpression == null) {
                return;
            }
            JSExpression elseExpression = exp.getElse();
            if (elseExpression == null) {
                return;
            }
            if (((isFalse(thenExpression) && isTrue(elseExpression))
                    || (isTrue(thenExpression) && isFalse(elseExpression))) &&
                "Boolean".equals(JSResolveUtil.getExpressionType(exp.getCondition(), exp.getContainingFile())) 
               ) {
                registerError(exp);
            }
        }
    }

    private static boolean isFalse(JSExpression expression) {
        @NonNls String text = expression.getText();
        return "false".equals(text);
    }

    private static boolean isTrue(JSExpression expression) {
        @NonNls String text = expression.getText();
        return "true".equals(text);
    }
}
