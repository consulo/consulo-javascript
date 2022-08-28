package com.sixrr.inspectjs.dom;

import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.component.ExtensionImpl;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class DocumentWriteJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("call.to.document.write.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.DOM_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("document.write.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSCallExpression(JSCallExpression jsCallExpression) {
            super.visitJSCallExpression(jsCallExpression);
            final JSExpression methodExpression;
            try {
                methodExpression = jsCallExpression.getMethodExpression();
            } catch (Exception e) {
                return; //catching an intelliJ CCE
            }
            if(!(methodExpression instanceof JSReferenceExpression))
            {
                return;
            }
            final JSReferenceExpression referenceExpression = (JSReferenceExpression) methodExpression;
            final JSExpression qualifier = referenceExpression.getQualifier();
            if(qualifier == null)
            {
                return;
            }
            @NonNls final String qualifierText = qualifier.getText();
            if(!"document".equals(qualifierText))
            {
                return;
            }
            @NonNls final String methodName = referenceExpression.getReferencedName();
            if(!"write".equals(methodName) && !"writeln".equals(methodName))
            {
                return;
            }
            registerError(methodExpression);
        }
    }
}
