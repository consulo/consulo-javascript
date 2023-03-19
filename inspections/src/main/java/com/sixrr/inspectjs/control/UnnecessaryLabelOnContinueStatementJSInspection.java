package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSContinueStatement;
import com.intellij.lang.javascript.psi.JSLoopStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.sixrr.inspectjs.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class UnnecessaryLabelOnContinueStatementJSInspection extends JavaScriptInspection {
    private final UnnecessaryLabelOnContinueStatementFix fix =
            new UnnecessaryLabelOnContinueStatementFix();

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("unnecessary.label.on.continue.statement.display.name");
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
	@Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("unnecessary.label.on.continue.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class UnnecessaryLabelOnContinueStatementFix extends InspectionJSFix {
        @Override
		@Nonnull
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

        @Override public void visitJSContinueStatement(@Nonnull JSContinueStatement statement) {
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
