package com.sixrr.inspectjs.assignment;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.EquivalenceChecker;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class SillyAssignmentJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.sillyAssignmentDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME;
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.sillyAssignmentErrorString().get();
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        @RequiredReadAction
        public void visitJSAssignmentExpression(@Nonnull JSAssignmentExpression assignment) {
            super.visitJSAssignmentExpression(assignment);

            IElementType sign = assignment.getOperationSign();
            if (!JSTokenTypes.EQ.equals(sign)) {
                return;
            }
            JSExpression lhs = assignment.getLOperand();
            if (lhs instanceof JSDefinitionExpression lhsDef) {
                lhs = lhsDef.getExpression();
            }
            JSExpression rhs = assignment.getROperand();
            if (rhs == null || lhs == null) {
                return;
            }
            if (!(rhs instanceof JSReferenceExpression rhsRef && lhs instanceof JSReferenceExpression lhsRef)) {
                return;
            }
            JSExpression rhsQualifier = rhsRef.getQualifier();
            JSExpression lhsQualifier = lhsRef.getQualifier();
            if (rhsQualifier != null || lhsQualifier != null) {
                if (!EquivalenceChecker.expressionsAreEquivalent(rhsQualifier, lhsQualifier)) {
                    return;
                }
            }
            String rhsName = rhsRef.getReferencedName();
            String lhsName = lhsRef.getReferencedName();
            if (rhsName == null || lhsName == null) {
                return;
            }
            if (!rhsName.equals(lhsName)) {
                return;
            }
            PsiElement rhsReferent = rhsRef.resolve();
            PsiElement lhsReferent = lhsRef.resolve();
            if (rhsReferent != null && lhsReferent != null && !rhsReferent.equals(lhsReferent)) {
                return;
            }

            if (lhsName.equals("location") && lhsQualifier != null && lhsQualifier.getText().equals("document")) {
                // document.location = document.location causes browser refresh
                return;
            }
            registerError(assignment);
        }
    }
}
