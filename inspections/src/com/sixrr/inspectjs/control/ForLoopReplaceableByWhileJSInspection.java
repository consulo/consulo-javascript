package com.sixrr.inspectjs.control;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSForStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.ui.SingleCheckboxOptionsPanel;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class ForLoopReplaceableByWhileJSInspection extends JavaScriptInspection {

    /**
     * @noinspection PublicField
     */
    public boolean m_ignoreLoopsWithoutConditions = false;

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("for.loop.replaceable.by.while.display.name");

    }
    @NotNull
    public String getID() {
        return "ForLoopReplaceableByWhile";
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @NotNull
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message(
                "for.loop.replaceable.by.while.problem.descriptor");
    }

    public JComponent createOptionsPanel() {
        return new SingleCheckboxOptionsPanel(InspectionJSBundle.message(
                "for.loop.replaceable.by.while.ignore.option"),
                this, "m_ignoreLoopsWithoutConditions");
    }

    public InspectionJSFix buildFix(PsiElement location) {
        return new ReplaceForByWhileFix();
    }

    private static class ReplaceForByWhileFix extends InspectionJSFix {

        @NotNull
        public String getName() {
            return InspectionJSBundle.message(
                    "for.loop.replaceable.by.while.replace.quickfix");
        }

        public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final PsiElement forKeywordElement = descriptor.getPsiElement();
            final JSForStatement forStatement =
                    (JSForStatement)forKeywordElement.getParent();
            assert forStatement != null;
            final JSExpression condition = forStatement.getCondition();
            final JSStatement body = forStatement.getBody();
            final String bodyText;
            if (body == null) {
                bodyText = "";
            } else {
                bodyText = body.getText();
            }
            @NonNls final String whileStatement;
            if (condition == null) {
                whileStatement = "while(true)" + bodyText;
            }
            else {
                whileStatement = "while(" + condition.getText() + ')' +
                        bodyText;
            }
            replaceStatement(forStatement, whileStatement);
        }
    }

    public BaseInspectionVisitor buildVisitor() {
        return new ForLoopReplaceableByWhileVisitor();
    }

    private class ForLoopReplaceableByWhileVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSForStatement(@NotNull JSForStatement statement) {
            super.visitJSForStatement(statement);
            final JSVarStatement varStatement = statement.getVarDeclaration();
            if(varStatement!=null)
            {
                return;
            }
            final JSExpression initialization = statement.getInitialization();
            if (initialization != null ) {
                return;
            }
            final JSExpression update = statement.getUpdate();
            if (update != null ) {
                return;
            }
            if (m_ignoreLoopsWithoutConditions) {
                final JSExpression condition = statement.getCondition();
                if (condition == null) {
                    return;
                }
                final String conditionText = condition.getText();
                if ("true".equals(conditionText)) {
                    return;
                }
            }
            registerStatementError(statement);
        }
    }
}