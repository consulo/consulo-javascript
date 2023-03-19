package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.psi.JSExpressionCodeFragment;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSThisExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.xml.psi.xml.XmlAttributeValue;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class ThisExpressionReferencesGlobalObjectJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("this.expression.which.references.the.global.object.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
	@Override
	@Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("this.expression.references.global.object.error.string");
    }

    @Override
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
