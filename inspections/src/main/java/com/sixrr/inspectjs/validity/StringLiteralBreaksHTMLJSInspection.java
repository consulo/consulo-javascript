package com.sixrr.inspectjs.validity;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class StringLiteralBreaksHTMLJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("string.literal.breaks.html.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    @Override
	public boolean isEnabledByDefault() {
        return false;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("string.literal.breaks.html.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {


        @Override public void visitJSLiteralExpression(JSSimpleLiteralExpression jsLiteralExpression) {
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
