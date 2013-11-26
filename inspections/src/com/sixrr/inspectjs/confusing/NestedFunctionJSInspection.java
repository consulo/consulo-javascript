package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.ui.SingleCheckboxOptionsPanel;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class NestedFunctionJSInspection extends JavaScriptInspection {

    @SuppressWarnings({"PublicField"})
    public boolean m_includeAnonymousFunctions = false;

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("nested.function.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Nullable
    protected String buildErrorString(Object... args) {
        final JSFunction function = (JSFunction) ((PsiElement) args[0]).getParent();
        if(functionHasIdentifier(function))
        {
            return InspectionJSBundle.message("nested.function.error.string");
        }
        return InspectionJSBundle.message("nested.anonymous.function.error.string");
    }


    public JComponent createOptionsPanel() {
        return new SingleCheckboxOptionsPanel(InspectionJSBundle.message("include.anonymous.functions.parameter"),
                this, "m_includeAnonymousFunctions");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSFunctionDeclaration(JSFunction function) {
            super.visitJSFunctionDeclaration(function);
            if(!m_includeAnonymousFunctions &&function.getName() == null )
            {
                return;
            }
            final JSFunction containingFunction = PsiTreeUtil.getParentOfType(function, JSFunction.class, true);
            if(containingFunction == null)
            {
                return;
            }
            registerFunctionError(function);
        }

        public void visitJSFunctionExpression(final JSFunctionExpression node) {
            visitJSFunctionDeclaration(node.getFunction()); 
        }
    }
}
