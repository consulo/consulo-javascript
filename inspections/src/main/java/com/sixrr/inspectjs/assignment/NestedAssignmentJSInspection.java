package com.sixrr.inspectjs.assignment;

import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class NestedAssignmentJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.nestedAssignmentDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    @Nonnull
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.nestedAssignmentErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new NestedAssignmentVisitor();
    }

    private static class NestedAssignmentVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSAssignmentExpression(@Nonnull JSAssignmentExpression expression) {
            super.visitJSAssignmentExpression(expression);
            final PsiElement parent = expression.getParent();
            if (parent == null || !(parent instanceof JSAssignmentExpression)) {
                return;
            }
            registerError(expression);
        }
    }
}