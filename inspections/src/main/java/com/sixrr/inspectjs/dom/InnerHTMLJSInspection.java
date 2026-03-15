package com.sixrr.inspectjs.dom;

import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.localize.LocalizeValue;
import org.jspecify.annotations.Nullable;

@ExtensionImpl
public class InnerHTMLJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.useOfInnerhtmlPropertyDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.DOM_GROUP_NAME;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.innerHtmlErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @SuppressWarnings({"HardCodedStringLiteral"})
        @Override
        public void visitJSReferenceExpression(JSReferenceExpression expression) {
            super.visitJSReferenceExpression(expression);
            String referenceName = expression.getReferencedName();
            if (!"innerHTML".equalsIgnoreCase(referenceName)) {
                return;
            }
            registerError(expression);
        }
    }
}
