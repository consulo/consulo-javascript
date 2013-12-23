package com.sixrr.inspectjs.control;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.*;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import org.jetbrains.annotations.NotNull;

public class  UnnecessaryReturnJSInspection extends JavaScriptInspection {
    private final UnnecessaryReturnFix fix = new UnnecessaryReturnFix();

    @Override
	@NotNull
    public String getID() {
        return "UnnecessaryReturnStatementJS";
    }

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("unnecessary.return.statement.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @Override
	public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("unnecessary.return.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new UnnecessaryReturnVisitor();
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    private static class UnnecessaryReturnFix extends InspectionJSFix {
        @Override
		@NotNull
        public String getName() {
            return InspectionJSBundle.message("remove.unnecessary.return.fix");
        }

        @Override
		public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final PsiElement returnKeywordElement = descriptor.getPsiElement();
            final PsiElement returnStatement = returnKeywordElement.getParent();
            assert returnStatement != null;
            deleteElement(returnStatement);
        }
    }

    private static class UnnecessaryReturnVisitor extends BaseInspectionVisitor {

        @Override public void visitJSReturnStatement(@NotNull JSReturnStatement statement) {
            super.visitJSReturnStatement(statement);

            final JSExpression returnValue = statement.getExpression();
            if(returnValue!=null)
            {
                return;
            }
            final JSFunction function =
                    PsiTreeUtil.getParentOfType(statement, JSFunction.class);
            if (function == null) {
                return;
            }
            final PsiElement body = function.getLastChild();
            if (body == null) {
                return;
            }
            if(!(body instanceof JSBlockStatement))
            {
                return;
            }
            if (ControlFlowUtils.statementCompletesWithStatement((JSStatement) body, statement)) {
                registerStatementError(statement);
            }
        }
    }
}
