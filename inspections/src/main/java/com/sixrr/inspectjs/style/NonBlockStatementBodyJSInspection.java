package com.sixrr.inspectjs.style;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class NonBlockStatementBodyJSInspection extends JavaScriptInspection {
    private InspectionJSFix fix = new WrapBodyFix();

    @Override
    @Nonnull
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.nonBlockStatementBodyDisplayName();
    }

    @Override
    @Nonnull
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.STYLE_GROUP_NAME;
    }

    @Override
    public boolean isEnabledByDefault() {
        return false;
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        return args[0] instanceof JSIfStatement
            ? InspectionJSLocalize.nonBlockBranchErrorString().get()
            : InspectionJSLocalize.nonBlockBodyErrorString().get();
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class WrapBodyFix extends InspectionJSFix {
        @Override
        @Nonnull
        public LocalizeValue getName() {
            return InspectionJSLocalize.wrapStatementBodyFix();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final PsiElement statementIdentifier = descriptor.getPsiElement();
            final JSStatement statement = (JSStatement) statementIdentifier.getParent();
            if (statement instanceof JSLoopStatement loopStatement) {
                final JSStatement body = loopStatement.getBody();
                wrapStatement(body);
            } else {
                final JSIfStatement ifStatement = (JSIfStatement) statement;
                final JSStatement thenBranch = ifStatement.getThen();
                if (thenBranch != null && !(thenBranch instanceof JSBlockStatement)) {
                    wrapStatement(thenBranch);
                }
                final JSStatement elseBranch = ifStatement.getElse();
                if (elseBranch != null && !(elseBranch instanceof JSBlockStatement)) {
                    wrapStatement(elseBranch);
                }
            }
        }

        private static void wrapStatement(JSStatement statement) throws IncorrectOperationException {
            final String text = statement.getText();
            replaceStatement(statement, '{' + text + '}');
        }
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSDoWhileStatement(@Nonnull JSDoWhileStatement statement) {
            super.visitJSDoWhileStatement(statement);

            final JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }

            if (body instanceof JSBlockStatement) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override
        public void visitJSWhileStatement(@Nonnull JSWhileStatement statement) {
            super.visitJSWhileStatement(statement);

            final JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }

            if (body instanceof JSBlockStatement) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override
        public void visitJSForStatement(@Nonnull JSForStatement statement) {
            super.visitJSForStatement(statement);

            final JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }

            if (body instanceof JSBlockStatement) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override
        public void visitJSForInStatement(@Nonnull JSForInStatement statement) {
            super.visitJSForInStatement(statement);

            final JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }

            if (body instanceof JSBlockStatement) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override
        public void visitJSIfStatement(@Nonnull JSIfStatement statement) {
            super.visitJSIfStatement(statement);

            final JSStatement thenBranch = statement.getThen();
            if (thenBranch != null) {
                if (!(thenBranch instanceof JSBlockStatement)) {
                    registerStatementError(statement, statement);
                    return;
                }
            }
            final JSStatement elseBranch = statement.getElse();

            if (elseBranch != null && !(elseBranch instanceof JSBlockStatement || elseBranch instanceof JSIfStatement)) {
                registerStatementError(statement, statement);
            }
        }
    }
}

