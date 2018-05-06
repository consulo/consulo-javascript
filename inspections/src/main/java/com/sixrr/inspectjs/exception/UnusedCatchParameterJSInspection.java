package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSCatchBlock;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.ui.SingleCheckboxOptionsPanel;
import org.jetbrains.annotations.NonNls;
import javax.annotation.Nonnull;

import javax.swing.*;

public class UnusedCatchParameterJSInspection extends JavaScriptInspection {

    /**
     * @noinspection PublicField
     */
    public boolean m_ignoreCatchBlocksWithComments = false;

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "unused.catch.parameter.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME;
    }

    @Override
	public JComponent createOptionsPanel() {

        return new SingleCheckboxOptionsPanel(InspectionJSBundle.message(
                "unused.catch.parameter.ignore.catch.option"), this,
                "m_ignoreCatchBlocksWithComments");
    }

    @Override
	@Nonnull
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message(
                "unused.catch.parameter.problem.descriptor");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new EmptyCatchBlockVisitor();
    }

    private class EmptyCatchBlockVisitor extends BaseInspectionVisitor {

        @Override public void visitJSTryStatement(@Nonnull JSTryStatement statement) {
            super.visitJSTryStatement(statement);
            final JSCatchBlock jsCatchBlock = statement.getCatchBlock();
            if(jsCatchBlock == null)
            {
                return;
            }
            checkCatchSection(jsCatchBlock);
        }

        private void checkCatchSection(JSCatchBlock section) {
            final JSParameter param = section.getParameter();
            final JSStatement block = section.getStatement();
            if (param == null || block == null) {
                return;
            }
            @NonNls final String paramName = param.getName();
            if ("ignore".equals(paramName) || "ignored".equals(paramName)) {
                return;
            }
            if (m_ignoreCatchBlocksWithComments) {
                final PsiElement[] children = block.getChildren();
                for (final PsiElement child : children) {
                    if (child instanceof PsiComment) {
                        return;
                    }
                }
            }
            final CatchParameterUsedVisitor visitor =
                    new CatchParameterUsedVisitor(param);
            block.accept(visitor);
            if (!visitor.isUsed()) {
                registerVariableError(param);
            }
        }
    }
}