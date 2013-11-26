package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSThisExpression;
import com.intellij.lang.javascript.psi.JSExpressionCodeFragment;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.PsiFile;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ThisExpressionReferencesGlobalObjectJSInspection extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("this.expression.which.references.the.global.object.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    public boolean isEnabledByDefault() {
        return true;
    }

    @Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("this.expression.references.global.object.error.string");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSThisExpression(JSThisExpression jsThisExpression) {
            super.visitJSThisExpression(jsThisExpression);
            final JSObjectLiteralExpression containingObject = PsiTreeUtil.getParentOfType(jsThisExpression, JSObjectLiteralExpression.class);
            if (containingObject != null) {
                return;
            }
            final JSFunction containingFunction =
                    PsiTreeUtil.getParentOfType(jsThisExpression, JSFunction.class);
            if (containingFunction != null) {
                return;
            }
            final XmlAttributeValue containingAttribute =
                    PsiTreeUtil.getParentOfType(jsThisExpression, XmlAttributeValue.class);
            if (containingAttribute != null) {
                return;
            }

            final PsiFile containingFile = jsThisExpression.getContainingFile();
            if (containingFile instanceof JSExpressionCodeFragment ||
                containingFile.getContext() instanceof XmlAttributeValue) {
                return;
            }
            registerError(jsThisExpression);
        }
    }
}
