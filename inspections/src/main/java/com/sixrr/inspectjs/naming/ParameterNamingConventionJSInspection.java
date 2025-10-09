package com.sixrr.inspectjs.naming;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSVariable;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.fix.RenameFix;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class ParameterNamingConventionJSInspection extends ConventionInspection {
    private final RenameFix fix = new RenameFix();

    @Override
    public boolean isEnabledByDefault() {
        return false;
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.functionParameterNamingConventionDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.NAMING_CONVENTIONS_GROUP_NAME;
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new ParameterNamingConventionJSInspectionState();
    }

    @Override
    protected InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    @Override
    protected boolean buildQuickFixesOnlyForOnTheFlyErrors() {
        return true;
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        ParameterNamingConventionJSInspectionState inspectionState = (ParameterNamingConventionJSInspectionState) state;

        final JSParameter parameter = (JSParameter) ((PsiElement) args[0]).getParent();
        assert parameter != null;
        final String parameterName = parameter.getName();
        if (parameterName.length() < inspectionState.m_minLength) {
            return InspectionJSLocalize.parameterNameIsTooShortErrorString().get();
        }
        else if (parameterName.length() > inspectionState.m_maxLength) {
            return InspectionJSLocalize.parameterNameIsTooLongErrorString().get();
        }
        return InspectionJSLocalize.parameterNameDoesntMatchRegexErrorString(inspectionState.m_regex).get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor<ParameterNamingConventionJSInspectionState> {
        @Override
        public void visitJSFunctionDeclaration(JSFunction function) {
            super.visitJSFunctionDeclaration(function);
            final JSParameterList parameterList = function.getParameterList();
            if (parameterList == null) {
                return;
            }
            final JSParameter[] parameters = parameterList.getParameters();
            for (JSVariable variable : parameters) {
                final String name = variable.getName();
                if (name == null) {
                    continue;
                }
                if (isValid(name, myState)) {
                    continue;
                }
                registerVariableError(variable);
            }
        }
    }
}
