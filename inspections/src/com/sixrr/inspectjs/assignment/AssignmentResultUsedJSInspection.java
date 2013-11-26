package com.sixrr.inspectjs.assignment;

import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class AssignmentResultUsedJSInspection extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("result.of.assignment.used.displayName");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME;
    }

    @Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("result.of.assignment.expression.used.error.string");
    }

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
