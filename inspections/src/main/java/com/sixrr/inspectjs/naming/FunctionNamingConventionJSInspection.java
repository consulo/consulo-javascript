package com.sixrr.inspectjs.naming;

import com.intellij.lang.javascript.psi.JSFunction;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.fix.RenameFix;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class FunctionNamingConventionJSInspection extends ConventionInspection {
    private final RenameFix fix = new RenameFix();

    @Override
    public boolean isEnabledByDefault() {
        return false;
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new FunctionNamingConventionJSInspectionState();
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.functionNamingConventionDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.NAMING_CONVENTIONS_GROUP_NAME.get();
    }

    @Override
    protected InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    @Override
    protected boolean buildQuickFixesOnlyForOnTheFlyErrors() {
        return true;
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        ConventionInspectionState inspectionState = (ConventionInspectionState)state;
        final String functionName = ((PsiElement)args[0]).getText();

        assert functionName != null;
        if (functionName.length() < inspectionState.m_minLength) {
            return InspectionJSLocalize.functionNameIsTooShortErrorString(functionName).get();
        }
        else if (functionName.length() > inspectionState.m_maxLength) {
            return InspectionJSLocalize.functionNameIsTooLongErrorString(functionName).get();
        }
        return InspectionJSLocalize.functionNameDoesntMatchRegexErrorString(functionName, inspectionState.m_regex).get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor<ConventionInspectionState> {
        @Override
        public void visitJSFunctionDeclaration(JSFunction function) {
            super.visitJSFunctionDeclaration(function);

            final String name = function.getName();
            if (name == null) {
                return;
            }
            if (isValid(name, myState)) {
                return;
            }
            final PsiElement identifier = function.getNameIdentifier();
            if (identifier == null || !PsiTreeUtil.isAncestor(function, identifier, true)) {
                return;
            }
            registerFunctionError(function);
        }
    }
}
