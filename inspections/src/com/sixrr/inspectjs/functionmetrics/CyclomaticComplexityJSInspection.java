package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import org.jetbrains.annotations.NotNull;

public class CyclomaticComplexityJSInspection
        extends FunctionMetricsInspection {
    @NotNull
    public String getID() {
        return "OverlyComplexFunctionJS";
    }

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("overly.complex.function.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME;
    }

    protected int getDefaultLimit() {
        return 10;
    }

    protected String getConfigurationLabel() {
        return InspectionJSBundle.message("function.complexity.limit.parameter");
    }

    public String buildErrorString(Object... args) {
        final JSFunction function = (JSFunction) ((PsiElement) args[0]).getParent();
        assert function != null;
        final CyclomaticComplexityVisitor visitor = new CyclomaticComplexityVisitor();
        final PsiElement lastChild = function.getLastChild();
        assert lastChild != null;
        lastChild.accept(visitor);
        final int coupling = visitor.getComplexity();
        if (functionHasIdentifier(function)) {
            return InspectionJSBundle.message("function.ref.is.overly.complex.cyclomatic.complexity.error.string", coupling);
        } else {
            return InspectionJSBundle.message("anonymous.function.is.overly.complex.cyclomatic.complexity.error.string", coupling);
        }
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSFunctionDeclaration(@NotNull JSFunction function) {
            final PsiElement lastChild = function.getLastChild();
            if(!(lastChild instanceof JSBlockStatement))
            {
                return;
            }
            final CyclomaticComplexityVisitor visitor = new CyclomaticComplexityVisitor();
            lastChild.accept(visitor);
            final int complexity = visitor.getComplexity();

            if (complexity <= getLimit()) {
                return;
            }
            registerFunctionError(function);
        }
    }
}

