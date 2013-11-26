package com.sixrr.inspectjs.dataflow;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ReuseOfLocalVariableJSInspection
        extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "reuse.of.local.variable.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.DATA_FLOW_ISSUES;
    }

    @NotNull
    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message(
                "reuse.of.local.variable.problem.descriptor");
    }


    public BaseInspectionVisitor buildVisitor() {
        return new ReuseOfLocalVariableVisitor();
    }

    private static class ReuseOfLocalVariableVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSAssignmentExpression(
                @NotNull JSAssignmentExpression assignment) {
            super.visitJSAssignmentExpression(assignment);
            if (assignment.getROperand()==null) {
                return;
            }
            final PsiElement assignmentParent = assignment.getParent();
            if (!(assignmentParent instanceof JSExpressionStatement)) {
                return;
            }
            final JSExpression lhs = assignment.getLOperand();
            if (!(lhs instanceof JSDefinitionExpression)) {
                return;
            }
            final JSDefinitionExpression def = (JSDefinitionExpression) lhs;
            final JSExpression defExpression = def.getExpression();
            if(!(defExpression instanceof JSReferenceExpression))
            {
                return;
            }
            final PsiElement referent = ((PsiReference) defExpression).resolve();
            if (!(referent instanceof JSVariable)) {
                return;
            }
            final JSVariable variable = (JSVariable) referent;

            if (variable.getInitializer() == null) {
                return;
            }
            final IElementType tokenType = assignment.getOperationSign();

            if (!JSTokenTypes.EQ.equals(tokenType)) {
                return;
            }
            final JSExpression rhs =  assignment.getROperand();
            if (VariableAccessUtils.variableIsUsed(variable, rhs)) {
                return;
            }
            final JSBlockStatement variableBlock =
                    PsiTreeUtil.getParentOfType(variable, JSBlockStatement.class);
            if (variableBlock == null) {
                return;
            }

            if (loopExistsBetween(assignment, variableBlock)) {
                return;
            }
            if (tryExistsBetween(assignment, variableBlock)) {
                // this could be weakened, slightly, if it could be verified
                // that a variable is used in only one branch of a try statement
                return;
            }
            final PsiElement assignmentBlock =
                    assignmentParent.getParent();
            if (assignmentBlock == null) {
                return;
            }
            if (variableBlock.equals(assignmentBlock)) {
                registerError(lhs);
                return;
            }
            final JSStatement[] statements = variableBlock.getStatements();
            final PsiElement containingStatement =
                    getChildWhichContainsElement(variableBlock, assignment);
            int statementPosition = -1;
            for (int i = 0; i < statements.length; i++) {
                if (statements[i].equals(containingStatement)) {
                    statementPosition = i;
                    break;
                }
            }
            if (statementPosition == -1) {
                return;
            }
            for (int i = statementPosition + 1; i < statements.length; i++) {
                if (VariableAccessUtils.variableIsUsed(variable, statements[i])) {
                    return;
                }
            }
            registerError(lhs);
        }

        private static boolean loopExistsBetween(
                JSAssignmentExpression assignment, JSBlockStatement block) {
            PsiElement elementToTest = assignment;
            while (elementToTest != null) {
                if (elementToTest.equals(block)) {
                    return false;
                }
                if (elementToTest instanceof JSLoopStatement) {
                    return true;
                }
                elementToTest = elementToTest.getParent();
            }
            return false;
        }

        private static boolean tryExistsBetween(
                JSAssignmentExpression assignment, JSBlockStatement block) {
            PsiElement elementToTest = assignment;
            while (elementToTest != null) {
                if (elementToTest.equals(block)) {
                    return false;
                }
                if (elementToTest instanceof JSTryStatement) {
                    return true;
                }
                elementToTest = elementToTest.getParent();
            }
            return false;
        }

        /**
         * @noinspection AssignmentToMethodParameter
         */
        @Nullable
        public static PsiElement getChildWhichContainsElement(
                @NotNull JSBlockStatement ancestor,
                @NotNull PsiElement descendant) {
            PsiElement element = descendant;
            while (!element.equals(ancestor)) {
                descendant = element;
                element = descendant.getParent();
                if (element == null) {
                    return null;
                }
            }
            return descendant;
        }
    }
}
