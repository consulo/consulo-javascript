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
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class UnusedCatchParameterJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.unusedCatchParameterDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME;
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new UnusedCatchParameterJSInspectionState();
    }

    @Nonnull
    @Override
    @RequiredReadAction
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
            JSCatchBlock jsCatchBlock = statement.getCatchBlock();
            if (jsCatchBlock == null) {
                return;
            }
            checkCatchSection(jsCatchBlock);
        }

        private void checkCatchSection(JSCatchBlock section) {
            JSParameter param = section.getParameter();
            JSStatement block = section.getStatement();
            if (param == null || block == null) {
                return;
            }
            String paramName = param.getName();
            if ("ignore".equals(paramName) || "ignored".equals(paramName)) {
                return;
            }
            if (myState.m_ignoreCatchBlocksWithComments) {
                PsiElement[] children = block.getChildren();
                for (PsiElement child : children) {
                    if (child instanceof PsiComment) {
                        return;
                    }
                }
            }
            CatchParameterUsedVisitor visitor = new CatchParameterUsedVisitor(param);
            block.accept(visitor);
            if (!visitor.isUsed()) {
                registerVariableError(param);
            }
        }
    }
}