package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class VoidExpressionJSInspection extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("void.expression.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("void.expression.error.string");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSPrefixExpression(JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            final PsiElement firstChild = expression.getFirstChild();
            if(firstChild == null)
            {
                return;
            }
            @NonNls final String text = firstChild.getText();
            if (!"void".equals(text)) {
                return;
            }
            registerError(firstChild);
        }
    }
}
