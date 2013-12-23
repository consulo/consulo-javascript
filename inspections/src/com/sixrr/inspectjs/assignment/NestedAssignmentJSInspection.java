package com.sixrr.inspectjs.assignment;

import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;

public class NestedAssignmentJSInspection extends JavaScriptInspection {

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("nested.assignment.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME;
    }

    @Override
	@NotNull
    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("nested.assignment.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new NestedAssignmentVisitor();
    }

    private static class NestedAssignmentVisitor extends BaseInspectionVisitor {

        @Override public void visitJSAssignmentExpression(
                @NotNull JSAssignmentExpression expression) {
            super.visitJSAssignmentExpression(expression);
            final PsiElement parent = expression.getParent();
            if (parent == null) {
                return;
            }
            if (!(parent instanceof JSAssignmentExpression )) {
                return;
            }
            registerError(expression);
        }
    }
}