package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import org.jetbrains.annotations.NotNull;

public class NestingDepthJSInspection extends FunctionMetricsInspection {
    @Override
	@NotNull
    public String getID() {
        return "OverlyNestedFunctionJS";
    }

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("overly.nested.function.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME;
    }

    @Override
	protected int getDefaultLimit() {
        return 5;
    }

    @Override
	protected String getConfigurationLabel() {
        return InspectionJSBundle.message("nesting.depth.limit");
    }

    @Override
	public String buildErrorString(Object... args) {
        final JSFunction function = (JSFunction) ((PsiElement) args[0]).getParent();
        assert function != null;
        final NestingDepthVisitor visitor = new NestingDepthVisitor();
        function.accept(visitor);
        final int nestingDepth = visitor.getMaximumDepth();
        if (functionHasIdentifier(function)) {
            return InspectionJSBundle.message("function.is.overly.nested.error.string", nestingDepth);
        }
        else {
            return InspectionJSBundle.message("anonymous.function.is.overly.nested.error.string", nestingDepth);
        }
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSFunctionDeclaration(@NotNull JSFunction function) {
            final NestingDepthVisitor visitor = new NestingDepthVisitor();
            function.accept(visitor);
            final int count = visitor.getMaximumDepth();

            if (count <= getLimit()) {
                return;
            }
            registerFunctionError(function);
        }
    }
}
