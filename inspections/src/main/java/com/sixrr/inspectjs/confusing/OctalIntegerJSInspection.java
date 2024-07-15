package com.sixrr.inspectjs.confusing;

import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.jetbrains.annotations.NonNls;

@ExtensionImpl
public class OctalIntegerJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.octalIntegerDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    @Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.octalIntegerErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSLiteralExpression(JSSimpleLiteralExpression jsLiteralExpression) {
            super.visitJSLiteralExpression(jsLiteralExpression);
            @NonNls final String text = jsLiteralExpression.getText();
            if (text.startsWith("0") && !"0".equals(text)
                && !text.startsWith("0x") && !text.startsWith("0X")
                && !text.contains(".") && !text.contains("e") && !text.contains("E"))
            {
                registerError(jsLiteralExpression);
            }
        }
    }
}
