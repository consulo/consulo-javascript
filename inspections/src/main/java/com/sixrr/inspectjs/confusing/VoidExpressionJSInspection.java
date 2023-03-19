package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class VoidExpressionJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("void.expression.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @RequiredReadAction
	@Override
	@Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("void.expression.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSPrefixExpression(JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            final PsiElement firstChild = expression.getFirstChild();
            if(firstChild == null)
            {
                return;
            }
            @NonNls final String text = firstChild.getText();
            if (!"void".equals(text)) {
                return;
            }
            registerError(firstChild);
        }
    }
}
