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
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class SillyAssignmentJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.sillyAssignmentDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME;
    }

    @RequiredReadAction
    @Override
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
        public void visitJSAssignmentExpression(@Nonnull JSAssignmentExpression assignment) {
            super.visitJSAssignmentExpression(assignment);

            final IElementType sign = assignment.getOperationSign();
            if (!JSTokenTypes.EQ.equals(sign)) {
                return;
            }
            JSExpression lhs = assignment.getLOperand();
            if(lhs instanceof JSDefinitionExpression)
            {
                lhs = ((JSDefinitionExpression)lhs).getExpression();
            }
            final JSExpression rhs = assignment.getROperand();
            if(rhs == null || lhs == null)
            {
                return;
            }
            if(!(rhs instanceof JSReferenceExpression) ||
                    !(lhs instanceof JSReferenceExpression) )
            {
                return;
            }
            final JSReferenceExpression rhsReference = (JSReferenceExpression) rhs;
            final JSReferenceExpression lhsReference = (JSReferenceExpression) lhs;
            final JSExpression rhsQualifier = rhsReference.getQualifier();
            final JSExpression lhsQualifier = lhsReference.getQualifier();
            if(rhsQualifier !=null || lhsQualifier !=null)
            {
                if(!EquivalenceChecker.expressionsAreEquivalent(rhsQualifier, lhsQualifier))
                {
                    return;
                }
            }
            final String rhsName = rhsReference.getReferencedName();
            final String lhsName = lhsReference.getReferencedName();
            if(rhsName == null || lhsName == null)
            {
                return;
            }
            if(!rhsName.equals(lhsName))
            {
                return;
            }
            final PsiElement rhsReferent = rhsReference.resolve();
            final PsiElement lhsReferent = lhsReference.resolve();
            if(rhsReferent != null && lhsReferent != null &&
                    !rhsReferent.equals(lhsReferent))
            {
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
