package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSNewExpression;
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
import org.jetbrains.annotations.NonNls;

@ExtensionImpl
public class DynamicallyGeneratedCodeJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.dynamicallyGeneratedCodeDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.dynamicallyGeneratedCodeErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSCallExpression(JSCallExpression jsCallExpression) {
            super.visitJSCallExpression(jsCallExpression);
            JSExpression methodExpression;
            try {
                methodExpression = jsCallExpression.getMethodExpression();
            }
            catch (Exception e) {
                return; //catching an intelliJ CCE
            }
            if (!(methodExpression instanceof JSReferenceExpression)) {
                return;
            }
            JSReferenceExpression referenceExpression = (JSReferenceExpression)methodExpression;
            JSExpression qualifier = referenceExpression.getQualifier();

            @NonNls String methodName = referenceExpression.getReferencedName();
            if (!"eval".equals(methodName) && !"setTimeout".equals(methodName) && !"setInterval".equals(methodName)) {
                return;
            }
            registerError(methodExpression);
        }

        @Override
        public void visitJSNewExpression(JSNewExpression jsNewExpression) {
            super.visitJSNewExpression(jsNewExpression);
            JSExpression methodExpression;
            try {
                methodExpression = jsNewExpression.getMethodExpression();
            }
            catch (Exception e) {
                return; //catching an intelliJ CCE
            }
            if (!(methodExpression instanceof JSReferenceExpression)) {
                return;
            }
            JSReferenceExpression referenceExpression = (JSReferenceExpression)methodExpression;
            JSExpression qualifier = referenceExpression.getQualifier();

            if (qualifier != null) {
                return;
            }
            @NonNls String methodName = referenceExpression.getReferencedName();
            if (!"Function".equals(methodName)) {
                return;
            }
            registerError(methodExpression);
        }
    }
}
