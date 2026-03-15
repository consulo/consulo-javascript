package com.sixrr.inspectjs.dom;

import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
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
public class DocumentWriteJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.callToDocumentWriteDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.DOM_GROUP_NAME;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.documentWriteErrorString().get();
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
            if (qualifier == null) {
                return;
            }
            String qualifierText = qualifier.getText();
            if (!"document".equals(qualifierText)) {
                return;
            }
            String methodName = referenceExpression.getReferencedName();
            if (!"write".equals(methodName) && !"writeln".equals(methodName)) {
                return;
            }
            registerError(methodExpression);
        }
    }
}
