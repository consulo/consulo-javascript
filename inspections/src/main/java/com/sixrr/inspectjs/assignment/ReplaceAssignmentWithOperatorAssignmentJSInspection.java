package com.sixrr.inspectjs.assignment;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.EquivalenceChecker;
import com.sixrr.inspectjs.utils.SideEffectChecker;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import org.intellij.lang.annotations.Pattern;

import java.util.Map;

@ExtensionImpl
public class ReplaceAssignmentWithOperatorAssignmentJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    @Pattern(value = "[a-zA-Z_0-9.-]+")
    public String getID() {
        return "AssignmentReplaceableWithOperatorAssignmentJS";
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.assignmentReplaceableWithOperatorAssignmentDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME;
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        String expression = calculateReplacementExpression((JSAssignmentExpression) args[0]);
        return InspectionJSLocalize.assignmentReplaceableWithOperatorAssignmentErrorString(expression).get();
    }

    @RequiredReadAction
    private static String calculateReplacementExpression(JSAssignmentExpression expression) {
        JSBinaryExpression rhs = (JSBinaryExpression) expression.getROperand();
        JSExpression lhs = expression.getLOperand();
        assert rhs != null;
        IElementType sign = rhs.getOperationSign();
        JSExpression rhsRhs = rhs.getROperand();
        assert rhsRhs != null;
        String signText = getTextForOperator(sign);
        return lhs.getText() + ' ' + signText + "= " + rhsRhs.getText();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new ReplaceAssignmentWithOperatorAssignmentVisitor();
    }

    @Override
    @RequiredReadAction
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return new ReplaceAssignmentWithOperatorAssignmentFix((JSAssignmentExpression) location);
    }

    private static class ReplaceAssignmentWithOperatorAssignmentFix extends InspectionJSFix {
        @Nonnull
        private final LocalizeValue myName;

        @RequiredReadAction
        private ReplaceAssignmentWithOperatorAssignmentFix(JSAssignmentExpression expression) {
            super();
            JSBinaryExpression rhs = (JSBinaryExpression) expression.getROperand();
            assert rhs != null;
            IElementType sign = rhs.getOperationSign();
            String signText = getTextForOperator(sign);
            myName = InspectionJSLocalize.replaceWithOperatorAssignFix(signText);
        }

        @Nonnull
        @Override
        public LocalizeValue getName() {
            return myName;
        }

        @Override
        @RequiredWriteAction
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            JSAssignmentExpression expression = (JSAssignmentExpression) descriptor.getPsiElement();
            String newExpression = calculateReplacementExpression(expression);
            replaceExpression(expression, newExpression);
        }
    }

    private static class ReplaceAssignmentWithOperatorAssignmentVisitor extends BaseInspectionVisitor {
        @Override
        @RequiredReadAction
        public void visitJSAssignmentExpression(@Nonnull JSAssignmentExpression assignment) {
            super.visitJSAssignmentExpression(assignment);

            IElementType sign = assignment.getOperationSign();
            if (!JSTokenTypes.EQ.equals(sign)) {
                return;
            }
            JSExpression lhs = assignment.getLOperand();
            if (lhs == null) {
                return;
            }
            if (!(assignment.getROperand() instanceof JSBinaryExpression binaryRhs && binaryRhs.getROperand() != null)) {
                return;
            }
            IElementType operationSign = binaryRhs.getOperationSign();
            if (operationSign == JSTokenTypes.ANDAND || operationSign == JSTokenTypes.OROR) {
                return;
            }
            JSExpression lOperand = binaryRhs.getLOperand();
            if (SideEffectChecker.mayHaveSideEffects(lhs)) {
                return;
            }
            if (lhs instanceof JSDefinitionExpression defExpr) {
                lhs = defExpr.getExpression();
            }
            if (!EquivalenceChecker.expressionsAreEquivalent(lhs, lOperand)) {
                return;
            }
            registerError(assignment);
        }
    }

    private static final Map<IElementType, String> TEXT_FOR_OPERATOR = Map.ofEntries(
        Map.entry(JSTokenTypes.PLUS, "+"),
        Map.entry(JSTokenTypes.MINUS, "-"),
        Map.entry(JSTokenTypes.MULT, "*"),
        Map.entry(JSTokenTypes.DIV, "/"),
        Map.entry(JSTokenTypes.PERC, "%"),
        Map.entry(JSTokenTypes.XOR, "^"),
        Map.entry(JSTokenTypes.ANDAND, "&&"),
        Map.entry(JSTokenTypes.OROR, "||"),
        Map.entry(JSTokenTypes.AND, "&"),
        Map.entry(JSTokenTypes.OR, "|"),
        Map.entry(JSTokenTypes.LTLT, "<<"),
        Map.entry(JSTokenTypes.LTLTEQ, "<<="),
        Map.entry(JSTokenTypes.GTGT, ">>"),
        Map.entry(JSTokenTypes.GTGTGT, ">>>")
    );

    private static String getTextForOperator(IElementType operator) {
        return TEXT_FOR_OPERATOR.getOrDefault(operator, "unknown");
    }
}
