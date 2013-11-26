package com.sixrr.inspectjs.control;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.*;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import org.jetbrains.annotations.NotNull;

public class UnnecessaryContinueJSInspection extends JavaScriptInspection {
    private final UnnecessaryContinueFix fix = new UnnecessaryContinueFix();

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("unnecessary.continue.statement.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    public boolean isEnabledByDefault() {
        return true;
    }

    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("unnecessary.continue.error.string");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    public InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    private static class UnnecessaryContinueFix extends InspectionJSFix {
        @NotNull
        public String getName() {
            return InspectionJSBundle.message("remove.unnecessary.continue.fix");
        }

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

        @Override public void visitJSContinueStatement(@NotNull JSContinueStatement statement) {

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
