package com.sixrr.inspectjs.naming;

import com.intellij.lang.javascript.psi.JSVarStatement;
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
public class LocalVariableNamingConventionJSInspection extends ConventionInspection {
    private final RenameFix fix = new RenameFix();

    @Override
    public boolean isEnabledByDefault() {
        return false;
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.localVariableNamingConventionDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.NAMING_CONVENTIONS_GROUP_NAME;
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
        LocalVariableNamingConventionJSInspectionState inspectionState = (LocalVariableNamingConventionJSInspectionState) state;

        JSVariable variable = (JSVariable) ((PsiElement) args[0]).getParent();
        assert variable != null;
        String variableName = variable.getName();
        if (variableName.length() < inspectionState.m_minLength) {
            return InspectionJSLocalize.variableNameIsTooShortErrorString().get();
        }
        else if (variableName.length() > inspectionState.m_maxLength) {
            return InspectionJSLocalize.variableNameIsTooLongErrorString().get();
        }
        return InspectionJSLocalize.variableNameDoesntMatchRegexErrorString(inspectionState.m_regex).get();
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new LocalVariableNamingConventionJSInspectionState();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor<LocalVariableNamingConventionJSInspectionState> {
        @Override
        public void visitJSVarStatement(JSVarStatement jsVarStatement) {
            super.visitJSVarStatement(jsVarStatement);
            JSVariable[] variables = jsVarStatement.getVariables();
            for (JSVariable variable : variables) {
                String name = variable.getName();
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
