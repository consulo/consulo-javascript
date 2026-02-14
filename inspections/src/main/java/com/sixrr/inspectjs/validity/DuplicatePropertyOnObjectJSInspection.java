package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

import java.util.Arrays;

@ExtensionImpl
public class DuplicatePropertyOnObjectJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.duplicatePropertyOnObjectLiteralDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.duplicateObjectPropertyErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSObjectLiteralExpression(JSObjectLiteralExpression jsObjectLiteralExpression) {
            super.visitJSObjectLiteralExpression(jsObjectLiteralExpression);
            JSProperty[] properties = jsObjectLiteralExpression.getProperties();
            boolean[] matched = new boolean[properties.length];
            Arrays.fill(matched, false);
            for (int i = 0; i < properties.length; i++) {
                if (matched[i]) {
                    continue;
                }
                JSProperty property1 = properties[i];
                for (int j = i + 1; j < properties.length; j++) {
                    if (matched[j]) {
                        continue;
                    }
                    JSProperty property2 = properties[j];
                    String property1Name = property1.getName();
                    String property2Name = property2.getName();
                    if (property1Name != null && property2Name != null &&
                        property1Name.equals(property2Name)) {
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
