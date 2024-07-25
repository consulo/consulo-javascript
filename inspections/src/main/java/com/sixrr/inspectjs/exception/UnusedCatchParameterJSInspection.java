package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSCatchBlock;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiComment;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class UnusedCatchParameterJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.unusedCatchParameterDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME.get();
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new UnusedCatchParameterJSInspectionState();
    }

    @RequiredReadAction
    @Override
    @Nonnull
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.unusedCatchParameterProblemDescriptor().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new EmptyCatchBlockVisitor();
    }

    private class EmptyCatchBlockVisitor extends BaseInspectionVisitor<UnusedCatchParameterJSInspectionState> {
        @Override
        public void visitJSTryStatement(@Nonnull JSTryStatement statement) {
            super.visitJSTryStatement(statement);
            final JSCatchBlock jsCatchBlock = statement.getCatchBlock();
            if (jsCatchBlock == null) {
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
            final String paramName = param.getName();
            if ("ignore".equals(paramName) || "ignored".equals(paramName)) {
                return;
            }
            if (myState.m_ignoreCatchBlocksWithComments) {
                final PsiElement[] children = block.getChildren();
                for (final PsiElement child : children) {
                    if (child instanceof PsiComment) {
                        return;
                    }
                }
            }
            final CatchParameterUsedVisitor visitor = new CatchParameterUsedVisitor(param);
            block.accept(visitor);
            if (!visitor.isUsed()) {
                registerVariableError(param);
            }
        }
    }
}