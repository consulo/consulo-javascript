package com.sixrr.inspectjs.naming;

import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.fix.RenameFix;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class LocalVariableNamingConventionJSInspection extends ConventionInspection {
    private static final int DEFAULT_MIN_LENGTH = 1;
    private static final int DEFAULT_MAX_LENGTH = 32;
    private final RenameFix fix = new RenameFix();

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("local.variable.naming.convention.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.NAMING_CONVENTIONS_GROUP_NAME;
    }

    protected InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    protected boolean buildQuickFixesOnlyForOnTheFlyErrors() {
        return true;
    }

    public String buildErrorString(Object... args) {
        final JSVariable variable = (JSVariable) ((PsiElement)args[0]).getParent();
        assert variable != null;
        final String variableName = variable.getName();
        if (variableName.length() < getMinLength()) {
            return InspectionJSBundle.message("variable.name.is.too.short.error.string");
        } else if (variableName.length() > getMaxLength()) {
            return InspectionJSBundle.message("variable.name.is.too.long.error.string");
        }
        return InspectionJSBundle.message("variable.name.doesnt.match.regex.error.string", getRegex());
    }

    @NonNls
    protected String getDefaultRegex() {
        return "[a-z][A-Za-z]*";
    }

    protected int getDefaultMinLength() {
        return DEFAULT_MIN_LENGTH;
    }

    protected int getDefaultMaxLength() {
        return DEFAULT_MAX_LENGTH;
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSVarStatement(JSVarStatement jsVarStatement) {
            super.visitJSVarStatement(jsVarStatement);
            final JSVariable[] variables = jsVarStatement.getVariables();
            for (JSVariable variable : variables) {
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
