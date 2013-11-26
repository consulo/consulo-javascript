package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class StringLiteralBreaksHTMLJSInspection extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("string.literal.breaks.html.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    public boolean isEnabledByDefault() {
        return false;
    }

    @Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("string.literal.breaks.html.error.string");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {


        @Override public void visitJSLiteralExpression(JSLiteralExpression jsLiteralExpression) {
            super.visitJSLiteralExpression(jsLiteralExpression);
            final String text = jsLiteralExpression.getText();
            if (!text.startsWith("\"") && !text.startsWith("'")) {
                return;
            }
            if (text.contains("</")) {
                registerError(jsLiteralExpression);
            }
        }

    }
}
