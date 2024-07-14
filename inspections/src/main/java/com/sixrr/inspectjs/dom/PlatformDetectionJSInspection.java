package com.sixrr.inspectjs.dom;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class PlatformDetectionJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.platformDetectionDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.DOM_GROUP_NAME;
    }

    @RequiredReadAction
    @Override
    @Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.platformDetectionErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @SuppressWarnings({"HardCodedStringLiteral"})
        @Override public void visitJSReferenceExpression(JSReferenceExpression referenceExpression) {
            super.visitJSReferenceExpression(referenceExpression);
            final JSExpression qualifier = referenceExpression.getQualifier();
            if (qualifier == null) {
                return;
            }
            final String qualifierText = qualifier.getText();
            if ("navigator".equalsIgnoreCase(qualifierText)) {
                final String methodName = referenceExpression.getReferencedName();
                if (!"userAgent".equalsIgnoreCase(methodName) &&
                        !"appName".equalsIgnoreCase(methodName) &&
                        !"appCodeName".equalsIgnoreCase(methodName) &&
                        !"platform".equalsIgnoreCase(methodName) &&
                        !"oscpu".equalsIgnoreCase(methodName)
                        ) {
                    return;
                }
                registerError(referenceExpression);
            } else if ("document".equalsIgnoreCase(qualifierText)) {
                final String methodName = referenceExpression.getReferencedName();
                if (!"all".equalsIgnoreCase(methodName) &&
                        !"layers".equalsIgnoreCase(methodName)) {
                    return;
                }
                registerError(referenceExpression);
            }
        }
    }
}
