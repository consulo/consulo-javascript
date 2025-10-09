package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSBreakStatement;
import com.intellij.lang.javascript.psi.JSLoopStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSSwitchStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class UnnecessaryLabelOnBreakStatementJSInspection extends JavaScriptInspection {
    private final UnnecessaryLabelOnBreakStatementFix fix = new UnnecessaryLabelOnBreakStatementFix();

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.unnecessaryLabelOnBreakStatementDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class UnnecessaryLabelOnBreakStatementFix extends InspectionJSFix {
        @Nonnull
        @Override
        public LocalizeValue getName() {
            return InspectionJSLocalize.removeLabelFix();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final PsiElement breakKeywordElement = descriptor.getPsiElement();
            final JSBreakStatement breakStatement = (JSBreakStatement) breakKeywordElement.getParent();
            replaceStatement(breakStatement, "break;");
        }
    }

    @RequiredReadAction
    @Override
    @Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.unnecessaryLabelOnBreakErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSBreakStatement(@Nonnull JSBreakStatement statement) {
            super.visitJSBreakStatement(statement);
            if (statement.getLabel() == null) {
                return;
            }
            final JSStatement statementToBreak = statement.getStatementToBreak();
            if (statementToBreak == null) {
                return;
            }
            final JSStatement containingStatement = PsiTreeUtil.getParentOfType(statement, JSLoopStatement.class, JSSwitchStatement.class);
            if (containingStatement == null) {
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
