package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSBreakStatement;
import com.intellij.lang.javascript.psi.JSLoopStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSSwitchStatement;
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
public class UnnecessaryLabelOnBreakStatementJSInspection extends JavaScriptInspection {
    private final UnnecessaryLabelOnBreakStatementFix fix =
            new UnnecessaryLabelOnBreakStatementFix();

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("unnecessary.label.on.break.statement.display.name");
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

    @Override
	public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class UnnecessaryLabelOnBreakStatementFix extends InspectionJSFix {
        @Override
		@Nonnull
        public String getName() {
            return InspectionJSBundle.message("remove.label.fix");
        }

        @Override
		public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final PsiElement breakKeywordElement = descriptor.getPsiElement();
            final JSBreakStatement breakStatement =
                    (JSBreakStatement) breakKeywordElement.getParent();
            replaceStatement(breakStatement, "break;");
        }
    }
    @RequiredReadAction
	@Override
	@Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("unnecessary.label.on.break.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSBreakStatement(@Nonnull JSBreakStatement statement) {
            super.visitJSBreakStatement(statement);
            if (statement.getLabel() == null) {
                return;
            }
            final JSStatement statementToBreak = statement.getStatementToBreak();
            if(statementToBreak == null)
            {
                return;
            }
            final JSStatement containingStatement =
                    PsiTreeUtil.getParentOfType(statement, JSLoopStatement.class,
                            JSSwitchStatement.class);
            if(containingStatement == null)
            {
                return;
            }
            final PsiElement parent = containingStatement.getParent();
            if (!statementToBreak.equals(parent)) {
                return;
            }
            registerStatementError(statement);
        }
    }
}
