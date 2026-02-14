package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import org.intellij.lang.annotations.Pattern;

@ExtensionImpl
public class EmptyStatementBodyJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    @Pattern(value = "[a-zA-Z_0-9.-]+")
    public String getID() {
        return "StatementWithEmptyBodyJS";
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.statementWithEmptyBodyDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        return args[0] instanceof JSIfStatement
            ? InspectionJSLocalize.statementHasEmptyBranchErrorString().get()
            : InspectionJSLocalize.statementHasEmptyBodyErrorString().get();
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new EmptyStatementBodyJSInspectionState();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new EmptyStatementVisitor();
    }

    private class EmptyStatementVisitor extends BaseInspectionVisitor<EmptyStatementBodyJSInspectionState> {
        @Override
        public void visitJSDoWhileStatement(@Nonnull JSDoWhileStatement statement) {
            super.visitJSDoWhileStatement(statement);

            JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }
            if (!isEmpty(body)) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override
        public void visitJSWhileStatement(@Nonnull JSWhileStatement statement) {
            super.visitJSWhileStatement(statement);

            JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }
            if (!isEmpty(body)) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override
        public void visitJSForStatement(@Nonnull JSForStatement statement) {
            super.visitJSForStatement(statement);

            JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }
            if (!isEmpty(body)) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override
        public void visitJSForInStatement(@Nonnull JSForInStatement statement) {
            super.visitJSForInStatement(statement);

            JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }
            if (!isEmpty(body)) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override
        public void visitJSIfStatement(@Nonnull JSIfStatement statement) {
            super.visitJSIfStatement(statement);

            JSStatement thenBranch = statement.getThen();
            if (thenBranch != null) {
                if (isEmpty(thenBranch)) {
                    registerStatementError(statement, statement);
                    return;
                }
            }
            JSStatement elseBranch = statement.getElse();

            if (elseBranch != null && isEmpty(elseBranch)) {
                registerStatementError(statement, statement);
            }
        }

        private boolean isEmpty(JSElement body) {
            if (body instanceof JSEmptyStatement) {
                return true;
            }
            else if (myState.m_reportEmptyBlocks && body instanceof JSBlockStatement) {
                JSBlockStatement block = (JSBlockStatement)body;
                JSStatement[] statements = block.getStatements();
                return statements.length == 0;
            }
            return false;
        }
    }
}

