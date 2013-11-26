package com.sixrr.inspectjs.dom;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class PlatformDetectionJSInspection extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("platform.detection.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.DOM_GROUP_NAME;
    }

    @Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("platform.detection.error.string");
    }

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
