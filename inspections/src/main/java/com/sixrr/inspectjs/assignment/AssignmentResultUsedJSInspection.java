package com.sixrr.inspectjs.assignment;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class AssignmentResultUsedJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("result.of.assignment.used.displayName");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("result.of.assignment.expression.used.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSAssignmentExpression(JSAssignmentExpression jsAssignmentExpression) {
            super.visitJSAssignmentExpression(jsAssignmentExpression);
            final PsiElement parent = jsAssignmentExpression.getParent();
            if (parent == null) {
                return;
            }
            if (parent instanceof JSForStatement ||
                    parent instanceof JSForInStatement ||
                    parent instanceof JSExpressionStatement ||
                    parent instanceof JSCommaExpression)
            {
                return;
            }
            registerError(jsAssignmentExpression);
        }
    }
}
