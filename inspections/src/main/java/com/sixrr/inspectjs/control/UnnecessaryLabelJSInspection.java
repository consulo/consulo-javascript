package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSBreakStatement;
import com.intellij.lang.javascript.psi.JSContinueStatement;
import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class UnnecessaryLabelJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.unnecessaryLabelDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME.get();
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new UnusedLabelVisitor();
    }

    @RequiredReadAction
    @Override
    @Nonnull
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.unnecessaryLabelErrorString().get();
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return new UnusedLabelFix();
    }

    private static class UnusedLabelFix extends InspectionJSFix {
        @Override
        @Nonnull
        public String getName() {
            return InspectionJSLocalize.removeLabelFix().get();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final PsiElement label = descriptor.getPsiElement();
            final JSLabeledStatement statement = (JSLabeledStatement) label.getParent();
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
        @Override
        public void visitJSLabeledStatement(JSLabeledStatement statement) {
            if (containsBreakOrContinueForLabel(statement)) {
                return;
            }
            final PsiElement labelIdentifier = statement.getLabelIdentifier();
            registerError(labelIdentifier);
        }

        private static boolean containsBreakOrContinueForLabel(JSLabeledStatement statement) {
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

        @Override
        public void visitElement(@Nonnull PsiElement element) {
            if (!found) {
                super.visitElement(element);
            }
        }

        @Override
        public void visitJSContinueStatement(@Nonnull JSContinueStatement continueStatement) {
            if (found) {
                return;
            }
            super.visitJSContinueStatement(continueStatement);

            if (label.equals(continueStatement.getLabel())) {
                found = true;
            }
        }

        @Override
        public void visitJSBreakStatement(@Nonnull JSBreakStatement breakStatement) {
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
