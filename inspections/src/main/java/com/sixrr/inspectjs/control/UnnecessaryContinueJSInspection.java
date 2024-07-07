package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class UnnecessaryContinueJSInspection extends JavaScriptInspection {
    private final UnnecessaryContinueFix fix = new UnnecessaryContinueFix();

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("unnecessary.continue.statement.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
	@Override
	public String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("unnecessary.continue.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class UnnecessaryContinueFix extends InspectionJSFix {
        @Override
		@Nonnull
        public String getName() {
            return InspectionJSBundle.message("remove.unnecessary.continue.fix");
        }

        @Override
		public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final PsiElement continueKeywordElement = descriptor.getPsiElement();
            final PsiElement continueStatement =
                    continueKeywordElement.getParent();
            assert continueStatement != null;
            deleteElement(continueStatement);
        }
    }

    private static class Visitor
            extends BaseInspectionVisitor {

        @Override public void visitJSContinueStatement(@Nonnull JSContinueStatement statement) {

            JSStatement continuedStatement =
                    statement.getStatementToContinue();
            if (continuedStatement == null) {
                return;
            }
            if(continuedStatement instanceof JSLabeledStatement)
            {
                continuedStatement = ((JSLabeledStatement)continuedStatement).getStatement();
            }
            if (!(continuedStatement instanceof JSLoopStatement)) {
                return;
            }
            final JSStatement body = ((JSLoopStatement) continuedStatement).getBody();
            if (body == null) {
                return;
            }
            if (body instanceof JSBlockStatement) {
                if (ControlFlowUtils.blockCompletesWithStatement((JSBlockStatement) body,
                        statement)) {
                    registerStatementError(statement);
                }
            } else {
                if (ControlFlowUtils.statementCompletesWithStatement(body,
                        statement)) {
                    registerStatementError(statement);
                }
            }
        }
    }
}
