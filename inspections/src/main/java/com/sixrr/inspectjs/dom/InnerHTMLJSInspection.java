package com.sixrr.inspectjs.dom;

import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class InnerHTMLJSInspection extends JavaScriptInspection {

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("use.of.innerhtml.property.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.DOM_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("inner.html.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @SuppressWarnings({"HardCodedStringLiteral"})
        @Override public void visitJSReferenceExpression(JSReferenceExpression expression) {
            super.visitJSReferenceExpression(expression);
            final String referenceName = expression.getReferencedName();
            if(!"innerHTML".equalsIgnoreCase(referenceName))
            {
                 return;
            }
            registerError(expression);
        }
    }
}
