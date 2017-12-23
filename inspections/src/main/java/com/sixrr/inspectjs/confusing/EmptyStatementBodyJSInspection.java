package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.ui.SingleCheckboxOptionsPanel;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class EmptyStatementBodyJSInspection extends JavaScriptInspection {
    /**
     * @noinspection PublicField
     */
    public boolean m_reportEmptyBlocks = false;

    @Override
	@NotNull
    public String getID() {
        return "StatementWithEmptyBodyJS";
    }

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("statement.with.empty.body.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @Override
	public String buildErrorString(Object... args) {
        if (args[0] instanceof JSIfStatement) {
            return InspectionJSBundle.message("statement.has.empty.branch.error.string");
        } else {
            return InspectionJSBundle.message("statement.has.empty.body.error.string");
        }
    }

    @Override
	public JComponent createOptionsPanel() {
        return new SingleCheckboxOptionsPanel(InspectionJSBundle.message("include.statement.bodies.that.are.empty.code.blocks.parameter"),
                this, "m_reportEmptyBlocks");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new EmptyStatementVisitor();
    }

    private class EmptyStatementVisitor extends BaseInspectionVisitor {

        @Override public void visitJSDoWhileStatement(@NotNull JSDoWhileStatement statement) {
            super.visitJSDoWhileStatement(statement);

            final JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }
            if (!isEmpty(body)) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override public void visitJSWhileStatement(@NotNull JSWhileStatement statement) {
            super.visitJSWhileStatement(statement);

            final JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }
            if (!isEmpty(body)) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override public void visitJSForStatement(@NotNull JSForStatement statement) {
            super.visitJSForStatement(statement);

            final JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }
            if (!isEmpty(body)) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override
		public void visitJSForInStatement(@NotNull JSForInStatement statement) {
            super.visitJSForInStatement(statement);

            final JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }
            if (!isEmpty(body)) {
                return;
            }
            registerStatementError(statement, statement);
        }

        @Override public void visitJSIfStatement(@NotNull JSIfStatement statement) {
            super.visitJSIfStatement(statement);

            final JSStatement thenBranch = statement.getThen();
            if (thenBranch != null) {
                if (isEmpty(thenBranch)) {
                    registerStatementError(statement, statement);
                    return;
                }
            }
            final JSStatement elseBranch = statement.getElse();

            if (elseBranch != null) {
                if (isEmpty(elseBranch)) {
                    registerStatementError(statement, statement);
                }
            }
        }

        private boolean isEmpty(JSElement body) {
            if (body instanceof JSEmptyStatement) {
                return true;
            } else if (m_reportEmptyBlocks && body instanceof JSBlockStatement) {
                final JSBlockStatement block = (JSBlockStatement) body;
                final JSStatement[] statements = block.getStatements();
                return statements.length == 0;
            }
            return false;
        }
    }
}

