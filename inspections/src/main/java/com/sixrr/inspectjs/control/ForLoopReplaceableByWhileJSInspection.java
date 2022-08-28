package com.sixrr.inspectjs.control;

import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSForStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSVarStatement;
import consulo.project.Project;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.ui.SingleCheckboxOptionsPanel;
import org.jetbrains.annotations.NonNls;
import javax.annotation.Nonnull;

import javax.swing.*;

@ExtensionImpl
public class ForLoopReplaceableByWhileJSInspection extends JavaScriptInspection {

    /**
     * @noinspection PublicField
     */
    public boolean m_ignoreLoopsWithoutConditions = false;

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("for.loop.replaceable.by.while.display.name");

    }
    @Override
	@Nonnull
    public String getID() {
        return "ForLoopReplaceableByWhile";
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
	@Nonnull
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message(
                "for.loop.replaceable.by.while.problem.descriptor");
    }

    @Override
	public JComponent createOptionsPanel() {
        return new SingleCheckboxOptionsPanel(InspectionJSBundle.message(
                "for.loop.replaceable.by.while.ignore.option"),
                this, "m_ignoreLoopsWithoutConditions");
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location) {
        return new ReplaceForByWhileFix();
    }

    private static class ReplaceForByWhileFix extends InspectionJSFix {

        @Override
		@Nonnull
        public String getName() {
            return InspectionJSBundle.message(
                    "for.loop.replaceable.by.while.replace.quickfix");
        }

        @Override
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

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new ForLoopReplaceableByWhileVisitor();
    }

    private class ForLoopReplaceableByWhileVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSForStatement(@Nonnull JSForStatement statement) {
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