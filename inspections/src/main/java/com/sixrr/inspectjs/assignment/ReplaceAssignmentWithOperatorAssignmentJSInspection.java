package com.sixrr.inspectjs.assignment;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.EquivalenceChecker;
import com.sixrr.inspectjs.utils.SideEffectChecker;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import org.jetbrains.annotations.NonNls;

@ExtensionImpl
public class ReplaceAssignmentWithOperatorAssignmentJSInspection
        extends JavaScriptInspection {
    @Override
	@Nonnull
    public String getID() {
        return "AssignmentReplaceableWithOperatorAssignmentJS";
    }

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("assignment.replaceable.with.operator.assignment.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME;
    }

    @RequiredReadAction
	@Override
	public String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message(
                "assignment.replaceable.with.operator.assignment.error.string",
                calculateReplacementExpression((JSAssignmentExpression) args[0]));
    }

    private static String calculateReplacementExpression(
            JSAssignmentExpression expression) {
        final JSBinaryExpression rhs =
                (JSBinaryExpression) expression.getROperand();
        final JSExpression lhs = expression.getLOperand();
        assert rhs != null;
        final IElementType sign = rhs.getOperationSign();
        final JSExpression rhsRhs = rhs.getROperand();
        assert rhsRhs != null;
        String signText = getTextForOperator(sign);
        return lhs.getText() + ' ' + signText + "= " + rhsRhs.getText();
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new ReplaceAssignmentWithOperatorAssignmentVisitor();
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location, Object state) {
        return new ReplaceAssignmentWithOperatorAssignmentFix(
                (JSAssignmentExpression) location);
    }

    private static class ReplaceAssignmentWithOperatorAssignmentFix
            extends InspectionJSFix {
        private final String m_name;

        private ReplaceAssignmentWithOperatorAssignmentFix(
                JSAssignmentExpression expression) {
            super();
            final JSBinaryExpression rhs =
                    (JSBinaryExpression) expression.getROperand();
            assert rhs != null;
            final IElementType sign = rhs.getOperationSign();
            String signText = getTextForOperator(sign);
            m_name = InspectionJSBundle.message("replace.with.operator.assign.fix", signText);
        }

        @Override
		@Nonnull
        public String getName() {
            return m_name;
        }

        @Override
		public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final JSAssignmentExpression expression =
                    (JSAssignmentExpression) descriptor.getPsiElement();
            final String newExpression =
                    calculateReplacementExpression(expression);
            replaceExpression(expression, newExpression);
        }
    }

    private static class ReplaceAssignmentWithOperatorAssignmentVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSAssignmentExpression(@Nonnull JSAssignmentExpression assignment) {
            super.visitJSAssignmentExpression(assignment);

            final IElementType sign = assignment.getOperationSign();
            if (!JSTokenTypes.EQ.equals(sign)) {
                return;
            }
            JSExpression lhs = assignment.getLOperand();
            final JSExpression rhs = assignment.getROperand();
            if (rhs == null || lhs == null) {
                return;
            }
            if (!(rhs instanceof JSBinaryExpression)) {
                return;
            }
            final JSBinaryExpression binaryRhs = (JSBinaryExpression) rhs;
            if (!(binaryRhs.getROperand() != null)) {
                return;
            }
            IElementType operationSign = binaryRhs.getOperationSign();
            if (operationSign == JSTokenTypes.ANDAND ||
                operationSign == JSTokenTypes.OROR) {
                return;
            }
            final JSExpression lOperand = binaryRhs.getLOperand();
            if (SideEffectChecker.mayHaveSideEffects(lhs)) {
                return;
            }
            if(lhs instanceof JSDefinitionExpression)
            {
                lhs = ((JSDefinitionExpression)lhs).getExpression();
            }
            if (!EquivalenceChecker.expressionsAreEquivalent(lhs, lOperand)) {
                return;
            }
            registerError(assignment);
        }
    }

    @NonNls
    private static String getTextForOperator(IElementType operator) {
        if (JSTokenTypes.PLUS.equals(operator)) {
            return "+";
        }
        if (JSTokenTypes.MINUS.equals(operator)) {
            return "-";
        }
        if (JSTokenTypes.MULT.equals(operator)) {
            return "*";
        }
        if (JSTokenTypes.DIV.equals(operator)) {
            return "/";
        }
        if (JSTokenTypes.PERC.equals(operator)) {
            return "%";
        }
        if (JSTokenTypes.XOR.equals(operator)) {
            return "^";
        }
        if (JSTokenTypes.ANDAND.equals(operator)) {
            return "&&";
        }
        if (JSTokenTypes.OROR.equals(operator)) {
            return "||";
        }
        if (JSTokenTypes.AND.equals(operator)) {
            return "&";
        }
        if (JSTokenTypes.OR.equals(operator)) {
            return "|";
        }
        if (JSTokenTypes.LTLT.equals(operator)) {
            return "<<";
        }
        if (JSTokenTypes.LTLTEQ.equals(operator)) {
            return "<<=";
        }
        if (JSTokenTypes.GTGT.equals(operator)) {
            return ">>";
        }
        if (JSTokenTypes.GTGTGT.equals(operator)) {
            return ">>>";
        }
        return "unknown";
    }
}
