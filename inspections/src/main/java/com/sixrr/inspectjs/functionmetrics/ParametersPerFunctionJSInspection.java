package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import org.intellij.lang.annotations.Pattern;

@ExtensionImpl
public class ParametersPerFunctionJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    @Pattern(value = "[a-zA-Z_0-9.-]+")
    public String getID() {
        return "OverlyComplexFunctionJS";
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.functionWithTooManyParametersDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME;
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new ParametersPerFunctionJSInspectionState();
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        JSFunction function = (JSFunction)((PsiElement)args[0]).getParent();
        assert function != null;
        JSParameterList parameterList = function.getParameterList();
        JSParameter[] parameters = parameterList.getParameters();
        int numParameters = parameters.length;
        return functionHasIdentifier(function)
            ? InspectionJSLocalize.functionHasTooManyParametersErrorString(numParameters).get()
            : InspectionJSLocalize.anonymousFunctionHasTooManyParametersErrorString(numParameters).get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor<ParametersPerFunctionJSInspectionState> {
        @Override
        public void visitJSFunctionDeclaration(@Nonnull JSFunction function) {
            JSParameterList parameterList = function.getParameterList();
            if (parameterList == null) {
                return;
            }
            JSParameter[] parameters = parameterList.getParameters();
            if (parameters == null) {
                return;
            }
            int numParameters = parameters.length;
            if (numParameters <= myState.getLimit()) {
                return;
            }
            registerFunctionError(function);
        }
    }
}
