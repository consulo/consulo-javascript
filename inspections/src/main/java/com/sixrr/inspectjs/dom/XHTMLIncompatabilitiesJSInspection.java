package com.sixrr.inspectjs.dom;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class XHTMLIncompatabilitiesJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.xhtmlIncompatibilitiesDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.DOM_GROUP_NAME;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.xhtmlIncompatabilitiesErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @SuppressWarnings({"HardCodedStringLiteral"})
        @Override
        public void visitJSReferenceExpression(JSReferenceExpression referenceExpression) {
            super.visitJSReferenceExpression(referenceExpression);
            final JSExpression qualifier = referenceExpression.getQualifier();
            if (qualifier == null) {
                return;
            }
            final String qualifierText = qualifier.getText();

            if (!"document".equalsIgnoreCase(qualifierText)) {
                return;
            }
            final String methodName = referenceExpression.getReferencedName();
            if (!"images".equalsIgnoreCase(methodName) &&
                !"body".equalsIgnoreCase(methodName) &&
                !"applets".equalsIgnoreCase(methodName) &&
                !"links".equalsIgnoreCase(methodName) &&
                !"forms".equalsIgnoreCase(methodName) &&
                !"anchors".equalsIgnoreCase(methodName)
            ) {
                return;
            }
            registerError(referenceExpression);
        }
    }
}
