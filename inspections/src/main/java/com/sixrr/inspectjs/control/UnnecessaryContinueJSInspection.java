package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class UnnecessaryContinueJSInspection extends JavaScriptInspection {
    private final UnnecessaryContinueFix fix = new UnnecessaryContinueFix();

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.unnecessaryContinueStatementDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.unnecessaryContinueErrorString().get();
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
        @Nonnull
        @Override
        public LocalizeValue getName() {
            return InspectionJSLocalize.removeUnnecessaryContinueFix();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            PsiElement continueKeywordElement = descriptor.getPsiElement();
            PsiElement continueStatement = continueKeywordElement.getParent();
            assert continueStatement != null;
            deleteElement(continueStatement);
        }
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSContinueStatement(@Nonnull JSContinueStatement statement) {
            JSStatement continuedStatement = statement.getStatementToContinue();
            if (continuedStatement == null) {
                return;
            }
            if (continuedStatement instanceof JSLabeledStatement labeledStatement)
            {
                continuedStatement = labeledStatement.getStatement();
            }
            if (!(continuedStatement instanceof JSLoopStatement)) {
                return;
            }
            JSStatement body = ((JSLoopStatement) continuedStatement).getBody();
            if (body == null) {
                return;
            }
            if (body instanceof JSBlockStatement blockStatement) {
                if (ControlFlowUtils.blockCompletesWithStatement(blockStatement, statement)) {
                    registerStatementError(statement);
                }
            } else if (ControlFlowUtils.statementCompletesWithStatement(body, statement)) {
                registerStatementError(statement);
            }
        }
    }
}
