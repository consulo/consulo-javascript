package com.sixrr.inspectjs.naming;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSVariable;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.fix.RenameFix;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

@ExtensionImpl
public class ParameterNamingConventionJSInspection extends ConventionInspection {
    private static final int DEFAULT_MIN_LENGTH = 1;
    private static final int DEFAULT_MAX_LENGTH = 32;
    private final RenameFix fix = new RenameFix();

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("function.parameter.naming.convention.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.NAMING_CONVENTIONS_GROUP_NAME;
    }

    @Override
	protected InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    @Override
	protected boolean buildQuickFixesOnlyForOnTheFlyErrors() {
        return true;
    }

    @Override
	public String buildErrorString(Object... args) {
        final JSParameter parameter = (JSParameter) ((PsiElement) args[0]).getParent();
        assert parameter != null;
        final String parameterName = parameter.getName();
        if (parameterName.length() < getMinLength()) {
            return InspectionJSBundle.message("parameter.name.is.too.short.error.string");
        } else if (parameterName.length() > getMaxLength()) {
            return InspectionJSBundle.message("parameter.name.is.too.long.error.string");
        }
        return InspectionJSBundle.message("parameter.name.doesnt.match.regex.error.string", getRegex());
    }

    @Override
	@NonNls
    protected String getDefaultRegex() {
        return "[a-z][A-Za-z]*";
    }

    @Override
	protected int getDefaultMinLength() {
        return DEFAULT_MIN_LENGTH;
    }

    @Override
	protected int getDefaultMaxLength() {
        return DEFAULT_MAX_LENGTH;
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSFunctionDeclaration(JSFunction function) {
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
                if (isValid(name)) {
                    continue;
                }
                registerVariableError(variable);
            }
        }
    }
}
