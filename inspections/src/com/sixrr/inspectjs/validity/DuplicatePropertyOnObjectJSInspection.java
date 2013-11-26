package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;

public class DuplicatePropertyOnObjectJSInspection extends JavaScriptInspection {
    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("duplicate.property.on.object.literal.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("duplicate.object.property.error.string");
    }

    public boolean isEnabledByDefault() {
        return true;
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor
            extends BaseInspectionVisitor {

        @Override public void visitJSObjectLiteralExpression(JSObjectLiteralExpression jsObjectLiteralExpression) {
            super.visitJSObjectLiteralExpression(jsObjectLiteralExpression);
            final JSProperty[] properties = jsObjectLiteralExpression.getProperties();
            final boolean[] matched = new boolean[properties.length];
            Arrays.fill(matched, false);
            for (int i = 0; i < properties.length; i++) {
                if (matched[i]) {
                    continue;
                }
                final JSProperty property1 = properties[i];
                for (int j = i + 1; j < properties.length; j++) {
                    if (matched[j]) {
                        continue;
                    }
                    final JSProperty property2 = properties[j];
                    final String property1Name = property1.getName();
                    final String property2Name = property2.getName();
                    if(property1Name !=null && property2Name!=null &&
                            property1Name.equals(property2Name))
                    {
                        registerError(property2.getFirstChild());
                        if (!matched[i]) {
                            registerError(property1.getFirstChild());
                        }
                        matched[i] = true;
                        matched[j] = true;
                    }
                }
            }

        }

    }
}
