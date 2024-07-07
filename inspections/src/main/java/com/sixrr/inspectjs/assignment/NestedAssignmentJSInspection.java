package com.sixrr.inspectjs.assignment;

import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;

import jakarta.annotation.Nonnull;

@ExtensionImpl
public class NestedAssignmentJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("nested.assignment.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME;
    }

    @RequiredReadAction
	@Override
	@Nonnull
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("nested.assignment.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new NestedAssignmentVisitor();
    }

    private static class NestedAssignmentVisitor extends BaseInspectionVisitor {

        @Override public void visitJSAssignmentExpression(
                @Nonnull JSAssignmentExpression expression) {
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