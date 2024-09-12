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
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class ParametersPerFunctionJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getID() {
        return "OverlyComplexFunctionJS";
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.functionWithTooManyParametersDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME.get();
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new ParametersPerFunctionJSInspectionState();
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        final JSFunction function = (JSFunction)((PsiElement)args[0]).getParent();
        assert function != null;
        final JSParameterList parameterList = function.getParameterList();
        final JSParameter[] parameters = parameterList.getParameters();
        final int numParameters = parameters.length;
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
            final JSParameterList parameterList = function.getParameterList();
            if (parameterList == null) {
                return;
            }
            final JSParameter[] parameters = parameterList.getParameters();
            if (parameters == null) {
                return;
            }
            final int numParameters = parameters.length;
            if (numParameters <= myState.getLimit()) {
                return;
            }
            registerFunctionError(function);
        }
    }
}
