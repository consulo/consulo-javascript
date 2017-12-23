package com.sixrr.inspectjs.control;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.JSContinueStatement;
import com.intellij.lang.javascript.psi.JSLoopStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class UnnecessaryLabelOnContinueStatementJSInspection extends JavaScriptInspection {
    private final UnnecessaryLabelOnContinueStatementFix fix =
            new UnnecessaryLabelOnContinueStatementFix();

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("unnecessary.label.on.continue.statement.display.name");
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
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("unnecessary.label.on.continue.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    private static class UnnecessaryLabelOnContinueStatementFix extends InspectionJSFix {
        @Override
		@NotNull
        public String getName() {
            return InspectionJSBundle.message("remove.label.fix");
        }

        @Override
		public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final PsiElement continueKeywordElement = descriptor.getPsiElement();
            final JSContinueStatement continueStatement =
                    (JSContinueStatement) continueKeywordElement.getParent();
            replaceStatement(continueStatement, "continue;");
        }
    }
    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSContinueStatement(@NotNull JSContinueStatement statement) {
            super.visitJSContinueStatement(statement);
            if (statement.getLabel() == null) {
                return;
            }
            final JSStatement statementToContinue = statement.getStatementToContinue();
            if(statementToContinue == null)
            {
                return;
            }
            final JSLoopStatement containingLoop = PsiTreeUtil.getParentOfType(statement, JSLoopStatement.class);
            if(containingLoop == null)
            {
                return;
            }
            final PsiElement parent = containingLoop.getParent();
            if(!statementToContinue.equals(parent))
            {
                return;
            }
            registerStatementError(statement);
        }
    }
}
