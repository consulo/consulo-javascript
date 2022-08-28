package com.sixrr.inspectjs.confusing;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NonNls;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class OctalIntegerJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("octal.integer.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("octal.integer.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSLiteralExpression(JSSimpleLiteralExpression jsLiteralExpression) {
            super.visitJSLiteralExpression(jsLiteralExpression);
            @NonNls final String text = jsLiteralExpression.getText();
            if(text.startsWith("0")&&  !"0".equals(text) &&
                    !text.startsWith("0x") &&!text.startsWith("0X") &&
                    !text.contains(".") &&!text.contains("e")&&!text.contains("E"))
            {
                registerError(jsLiteralExpression);
            }
        }

    }
}
