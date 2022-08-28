package com.sixrr.inspectjs.dom;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.component.ExtensionImpl;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class XHTMLIncompatabilitiesJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("xhtml.incompatibilities.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.DOM_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("xhtml.incompatabilities.error.string");
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
