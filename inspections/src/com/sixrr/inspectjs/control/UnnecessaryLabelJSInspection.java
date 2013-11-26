package com.sixrr.inspectjs.control;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.JSBreakStatement;
import com.intellij.lang.javascript.psi.JSContinueStatement;
import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import org.jetbrains.annotations.NotNull;

public class UnnecessaryLabelJSInspection extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("unnecessary.label.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    public boolean isEnabledByDefault() {
        return true;
    }

    public BaseInspectionVisitor buildVisitor() {
        return new UnusedLabelVisitor();
    }

    @NotNull
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("unnecessary.label.error.string");

    }

    public InspectionJSFix buildFix(PsiElement location) {
        return new UnusedLabelFix();
    }

    private static class UnusedLabelFix extends InspectionJSFix {

        @NotNull
        public String getName() {
            return InspectionJSBundle.message("remove.label.fix");
        }

        public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final PsiElement label = descriptor.getPsiElement();
            final JSLabeledStatement statement =
                    (JSLabeledStatement) label.getParent();
            assert statement != null;
            final JSStatement labeledStatement = statement.getStatement();
            if (labeledStatement == null) {
                return;
            }
            final String statementText = labeledStatement.getText();
            replaceStatement(statement, statementText);
        }
    }

    private static class UnusedLabelVisitor extends BaseInspectionVisitor {

        @Override public void visitJSLabeledStatement(JSLabeledStatement statement) {
            if (containsBreakOrContinueForLabel(statement)) {
                return;
            }
            final PsiElement labelIdentifier =
                    statement.getLabelIdentifier();
            registerError(labelIdentifier);
        }

        private static boolean containsBreakOrContinueForLabel(
                JSLabeledStatement statement) {
            final LabelFinder labelFinder = new LabelFinder(statement);
            statement.accept(labelFinder);
            return labelFinder.jumpFound();
        }
    }

    private static class LabelFinder extends JSRecursiveElementVisitor {

        private boolean found = false;
        private String label = null;

        private LabelFinder(JSLabeledStatement target) {
            super();
            final PsiElement labelIdentifier = target.getLabelIdentifier();
            label = labelIdentifier.getText();
        }

        @Override public void visitElement(@NotNull PsiElement element) {
            if (!found) {
                super.visitElement(element);
            }
        }

        @Override public void visitJSContinueStatement(
                @NotNull JSContinueStatement continueStatement) {
            if (found) {
                return;
            }
            super.visitJSContinueStatement(continueStatement);

            if (label.equals(continueStatement.getLabel())) {
                found = true;
            }
        }

        @Override public void visitJSBreakStatement(
                @NotNull JSBreakStatement breakStatement) {
            if (found) {
                return;
            }
            super.visitJSBreakStatement(breakStatement);
            if (label.equals(breakStatement.getLabel())) {
                found = true;
            }
        }

        public boolean jumpFound() {
            return found;
        }
    }
}
