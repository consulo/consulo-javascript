package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.BoolUtils;
import com.sixrr.inspectjs.utils.ConditionalUtils;
import com.sixrr.inspectjs.utils.EquivalenceChecker;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import org.intellij.lang.annotations.Pattern;
import org.jetbrains.annotations.NonNls;

@ExtensionImpl
public class TrivialIfJSInspection extends JavaScriptInspection {
    private final TrivialIfFix fix = new TrivialIfFix();

    @Nonnull
    @Override
    @Pattern(value = "[a-zA-Z_0-9.-]+")
    public String getID() {
        return "RedundantIfStatementJS";
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.redundantIfStatementDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new TrivialIfVisitor();
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.trivialIfErrorString().get();
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class TrivialIfFix extends InspectionJSFix {
        @Nonnull
        @Override
        public LocalizeValue getName() {
            return InspectionJSLocalize.simplifyFix();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final PsiElement ifKeywordElement = descriptor.getPsiElement();
            final JSIfStatement statement = (JSIfStatement) ifKeywordElement.getParent();
            if (isSimplifiableAssignment(statement)) {
                replaceSimplifiableAssignment(statement);
            }
            else if (isSimplifiableReturn(statement)) {
                repaceSimplifiableReturn(statement);
            }
            else if (isSimplifiableImplicitReturn(statement)) {
                replaceSimplifiableImplicitReturn(statement);
            }
            else if (isSimplifiableAssignmentNegated(statement)) {
                replaceSimplifiableAssignmentNegated(statement);
            }
            else if (isSimplifiableReturnNegated(statement)) {
                repaceSimplifiableReturnNegated(statement);
            }
            else if (isSimplifiableImplicitReturnNegated(statement)) {
                replaceSimplifiableImplicitReturnNegated(statement);
            }
            else if (isSimplifiableImplicitAssignment(statement)) {
                replaceSimplifiableImplicitAssignment(statement);
            }
            else if (isSimplifiableImplicitAssignmentNegated(statement)) {
                replaceSimplifiableImplicitAssignmentNegated(statement);
            }
        }

        private void replaceSimplifiableImplicitReturn(JSIfStatement statement) throws IncorrectOperationException {
            final JSExpression condition = statement.getCondition();
            final String conditionText = condition.getText();
            final PsiElement nextStatement = PsiTreeUtil.skipSiblingsForward(statement, new Class[]{PsiWhiteSpace.class});
            @NonNls final String newStatement = "return " + conditionText + ';';
            replaceStatement(statement, newStatement);
            assert nextStatement != null;
            deleteElement(nextStatement);
        }

        private void repaceSimplifiableReturn(JSIfStatement statement) throws IncorrectOperationException {
            final JSExpression condition = statement.getCondition();
            final String conditionText = condition.getText();
            @NonNls final String newStatement = "return " + conditionText + ';';
            replaceStatement(statement, newStatement);
        }

        private void replaceSimplifiableAssignment(JSIfStatement statement) throws IncorrectOperationException {
            final JSExpression condition = statement.getCondition();
            final String conditionText = condition.getText();
            final JSStatement thenBranch = statement.getThen();
            final JSExpressionStatement assignmentStatement = (JSExpressionStatement) ConditionalUtils.stripBraces(thenBranch);
            final JSAssignmentExpression assignmentExpression = (JSAssignmentExpression) assignmentStatement.getExpression();
            final IElementType operator = assignmentExpression.getOperationSign();
            final String operatorText = getTextForOperator(operator);
            final JSExpression lhs = assignmentExpression.getLOperand();
            final String lhsText = lhs.getText();
            replaceStatement(statement, lhsText + operatorText + conditionText + ';');
        }

        private void replaceSimplifiableImplicitAssignment(JSIfStatement statement) throws IncorrectOperationException {
            final PsiElement prevStatement = PsiTreeUtil.skipSiblingsBackward(statement, new Class[]{PsiWhiteSpace.class});
            if (prevStatement == null) {
                return;
            }
            final JSExpression condition = statement.getCondition();
            final String conditionText = condition.getText();
            final JSStatement thenBranch = statement.getThen();
            final JSExpressionStatement assignmentStatement = (JSExpressionStatement) ConditionalUtils.stripBraces(thenBranch);
            final JSAssignmentExpression assignmentExpression = (JSAssignmentExpression) assignmentStatement.getExpression();
            final IElementType operator = assignmentExpression.getOperationSign();
            final JSExpression lhs = assignmentExpression.getLOperand();
            final String lhsText = lhs.getText();
            replaceStatement(statement, lhsText + operator + conditionText + ';');
            deleteElement(prevStatement);
        }

        private void replaceSimplifiableImplicitAssignmentNegated(JSIfStatement statement) throws IncorrectOperationException {
            final PsiElement prevStatement = PsiTreeUtil.skipSiblingsBackward(statement, new Class[]{PsiWhiteSpace.class});
            final JSExpression condition = statement.getCondition();
            final String conditionText = BoolUtils.getNegatedExpressionText(condition);
            final JSStatement thenBranch = statement.getThen();
            final JSExpressionStatement assignmentStatement = (JSExpressionStatement) ConditionalUtils.stripBraces(thenBranch);
            final JSAssignmentExpression assignmentExpression = (JSAssignmentExpression) assignmentStatement.getExpression();
            final IElementType operator = assignmentExpression.getOperationSign();
            final String operatorText = getTextForOperator(operator);
            final JSExpression lhs = assignmentExpression.getLOperand();
            final String lhsText = lhs.getText();
            replaceStatement(statement, lhsText + operatorText + conditionText + ';');
            assert prevStatement != null;
            deleteElement(prevStatement);
        }

        private void replaceSimplifiableImplicitReturnNegated(JSIfStatement statement) throws IncorrectOperationException {
            final JSExpression condition = statement.getCondition();

            final String conditionText = BoolUtils.getNegatedExpressionText(condition);
            final PsiElement nextStatement = PsiTreeUtil.skipSiblingsForward(statement, new Class[]{PsiWhiteSpace.class});
            if (nextStatement == null) {
                return;
            }
            @NonNls final String newStatement = "return " + conditionText + ';';
            replaceStatement(statement, newStatement);
            deleteElement(nextStatement);
        }

        private void repaceSimplifiableReturnNegated(JSIfStatement statement) throws IncorrectOperationException {
            final JSExpression condition = statement.getCondition();
            final String conditionText = BoolUtils.getNegatedExpressionText(condition);
            @NonNls final String newStatement = "return " + conditionText + ';';
            replaceStatement(statement, newStatement);
        }

        private void replaceSimplifiableAssignmentNegated(JSIfStatement statement) throws IncorrectOperationException {
            final JSExpression condition = statement.getCondition();
            final String conditionText = BoolUtils.getNegatedExpressionText(condition);
            final JSStatement thenBranch = statement.getThen();
            final JSExpressionStatement assignmentStatement = (JSExpressionStatement) ConditionalUtils.stripBraces(thenBranch);
            final JSAssignmentExpression assignmentExpression = (JSAssignmentExpression) assignmentStatement.getExpression();
            final IElementType operator = assignmentExpression.getOperationSign();
            final String operatorText = getTextForOperator(operator);
            final JSExpression lhs = assignmentExpression.getLOperand();
            final String lhsText = lhs.getText();
            replaceStatement(statement, lhsText + operatorText + conditionText + ';');
        }

    }

    private static class TrivialIfVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSIfStatement(@Nonnull JSIfStatement ifStatement) {
            super.visitJSIfStatement(ifStatement);
            final JSExpression condition = ifStatement.getCondition();
            if (condition == null) {
                return;
            }
            if (isSimplifiableAssignment(ifStatement)) {
                registerStatementError(ifStatement);
                return;
            }

            if (isSimplifiableReturn(ifStatement)) {
                registerStatementError(ifStatement);
                return;
            }

            if (isSimplifiableImplicitReturn(ifStatement)) {
                registerStatementError(ifStatement);
                return;
            }
            if (isSimplifiableAssignmentNegated(ifStatement)) {
                registerStatementError(ifStatement);
                return;
            }

            if (isSimplifiableReturnNegated(ifStatement)) {
                registerStatementError(ifStatement);
                return;
            }

            if (isSimplifiableImplicitReturnNegated(ifStatement)) {
                registerStatementError(ifStatement);
                return;
            }
            if (isSimplifiableImplicitAssignment(ifStatement)) {
                registerStatementError(ifStatement);
                return;
            }

            if (isSimplifiableImplicitAssignmentNegated(ifStatement)) {
                registerStatementError(ifStatement);
            }
        }
    }

    public static boolean isSimplifiableImplicitReturn(JSIfStatement ifStatement) {
        if (ifStatement.getElse() != null) {
            return false;
        }
        JSStatement thenBranch = ifStatement.getThen();
        thenBranch = ConditionalUtils.stripBraces(thenBranch);
        final PsiElement nextStatement = PsiTreeUtil.skipSiblingsForward(ifStatement, new Class[]{PsiWhiteSpace.class});
        if (!(nextStatement instanceof JSStatement)) {
            return false;
        }

        final JSStatement elseBranch = (JSStatement) nextStatement;
        return ConditionalUtils.isReturn(thenBranch, "true") && ConditionalUtils.isReturn(elseBranch, "false");
    }

    public static boolean isSimplifiableImplicitReturnNegated(JSIfStatement ifStatement) {
        if (ifStatement.getElse() != null) {
            return false;
        }
        JSStatement thenBranch = ifStatement.getThen();
        thenBranch = ConditionalUtils.stripBraces(thenBranch);

        final PsiElement nextStatement = PsiTreeUtil.skipSiblingsForward(ifStatement, new Class[]{PsiWhiteSpace.class});
        if (!(nextStatement instanceof JSStatement)) {
            return false;
        }
        final JSStatement elseBranch = (JSStatement) nextStatement;
        return ConditionalUtils.isReturn(thenBranch, "false") && ConditionalUtils.isReturn(elseBranch, "true");
    }

    public static boolean isSimplifiableReturn(JSIfStatement ifStatement) {
        JSStatement thenBranch = ifStatement.getThen();
        thenBranch = ConditionalUtils.stripBraces(thenBranch);
        JSStatement elseBranch = ifStatement.getElse();
        elseBranch = ConditionalUtils.stripBraces(elseBranch);
        return ConditionalUtils.isReturn(thenBranch, "true") && ConditionalUtils.isReturn(elseBranch, "false");
    }

    public static boolean isSimplifiableReturnNegated(JSIfStatement ifStatement) {
        JSStatement thenBranch = ifStatement.getThen();
        thenBranch = ConditionalUtils.stripBraces(thenBranch);
        JSStatement elseBranch = ifStatement.getElse();
        elseBranch = ConditionalUtils.stripBraces(elseBranch);
        return ConditionalUtils.isReturn(thenBranch, "false") && ConditionalUtils.isReturn(elseBranch, "true");
    }

    public static boolean isSimplifiableAssignment(JSIfStatement ifStatement) {
        JSStatement thenBranch = ifStatement.getThen();
        thenBranch = ConditionalUtils.stripBraces(thenBranch);
        JSStatement elseBranch = ifStatement.getElse();
        elseBranch = ConditionalUtils.stripBraces(elseBranch);
        if (ConditionalUtils.isAssignment(thenBranch, "true") && ConditionalUtils.isAssignment(elseBranch, "false")) {
            final JSAssignmentExpression thenExpression = (JSAssignmentExpression) ((JSExpressionStatement) thenBranch).getExpression();
            final JSAssignmentExpression elseExpression = (JSAssignmentExpression) ((JSExpressionStatement) elseBranch).getExpression();
            final IElementType thenSign = thenExpression.getOperationSign();
            final IElementType elseSign = elseExpression.getOperationSign();
            if (!thenSign.equals(elseSign)) {
                return false;
            }
            final JSExpression thenLhs = thenExpression.getLOperand();
            final JSExpression elseLhs = elseExpression.getLOperand();
            return EquivalenceChecker.expressionsAreEquivalent(thenLhs, elseLhs);
        }
        else {
            return false;
        }
    }

    public static boolean isSimplifiableAssignmentNegated(JSIfStatement ifStatement) {
        JSStatement thenBranch = ifStatement.getThen();
        thenBranch = ConditionalUtils.stripBraces(thenBranch);
        JSStatement elseBranch = ifStatement.getElse();
        elseBranch = ConditionalUtils.stripBraces(elseBranch);
        if (ConditionalUtils.isAssignment(thenBranch, "false") && ConditionalUtils.isAssignment(elseBranch, "true")) {
            final JSAssignmentExpression thenExpression = (JSAssignmentExpression) ((JSExpressionStatement) thenBranch).getExpression();
            final JSAssignmentExpression elseExpression = (JSAssignmentExpression) ((JSExpressionStatement) elseBranch).getExpression();
            final IElementType thenSign = thenExpression.getOperationSign();
            final IElementType elseSign = elseExpression.getOperationSign();
            if (!thenSign.equals(elseSign)) {
                return false;
            }
            final JSExpression thenLhs = thenExpression.getLOperand();
            final JSExpression elseLhs = elseExpression.getLOperand();
            return EquivalenceChecker.expressionsAreEquivalent(thenLhs, elseLhs);
        }
        else {
            return false;
        }
    }

    public static boolean isSimplifiableImplicitAssignment(JSIfStatement ifStatement) {
        if (ifStatement.getElse() != null) {
            return false;
        }
        JSStatement thenBranch = ifStatement.getThen();
        thenBranch = ConditionalUtils.stripBraces(thenBranch);
        final PsiElement nextStatement = PsiTreeUtil.skipSiblingsBackward(ifStatement, new Class[]{PsiWhiteSpace.class});
        if (!(nextStatement instanceof JSStatement)) {
            return false;
        }
        JSStatement elseBranch = (JSStatement) nextStatement;

        elseBranch = ConditionalUtils.stripBraces(elseBranch);
        if (ConditionalUtils.isAssignment(thenBranch, "true") && ConditionalUtils.isAssignment(elseBranch, "false")) {
            final JSAssignmentExpression thenExpression = (JSAssignmentExpression) ((JSExpressionStatement) thenBranch).getExpression();
            final JSAssignmentExpression elseExpression = (JSAssignmentExpression) ((JSExpressionStatement) elseBranch).getExpression();
            final IElementType thenSign = thenExpression.getOperationSign();
            final IElementType elseSign = elseExpression.getOperationSign();
            if (!thenSign.equals(elseSign)) {
                return false;
            }
            final JSExpression thenLhs = thenExpression.getLOperand();
            final JSExpression elseLhs = elseExpression.getLOperand();
            return EquivalenceChecker.expressionsAreEquivalent(thenLhs, elseLhs);
        }
        else {
            return false;
        }
    }

    public static boolean isSimplifiableImplicitAssignmentNegated(JSIfStatement ifStatement) {
        if (ifStatement.getElse() != null) {
            return false;
        }
        JSStatement thenBranch = ifStatement.getThen();
        thenBranch = ConditionalUtils.stripBraces(thenBranch);
        final PsiElement nextStatement = PsiTreeUtil.skipSiblingsBackward(ifStatement, new Class[]{PsiWhiteSpace.class});
        if (!(nextStatement instanceof JSStatement)) {
            return false;
        }
        JSStatement elseBranch = (JSStatement) nextStatement;

        elseBranch = ConditionalUtils.stripBraces(elseBranch);
        if (ConditionalUtils.isAssignment(thenBranch, "false") && ConditionalUtils.isAssignment(elseBranch, "true")) {
            final JSAssignmentExpression thenExpression = (JSAssignmentExpression) ((JSExpressionStatement) thenBranch).getExpression();
            final JSAssignmentExpression elseExpression = (JSAssignmentExpression) ((JSExpressionStatement) elseBranch).getExpression();
            final IElementType thenSign = thenExpression.getOperationSign();
            final IElementType elseSign = elseExpression.getOperationSign();
            if (!thenSign.equals(elseSign)) {
                return false;
            }
            final JSExpression thenLhs = thenExpression.getLOperand();
            final JSExpression elseLhs = elseExpression.getLOperand();
            return EquivalenceChecker.expressionsAreEquivalent(thenLhs, elseLhs);
        }
        else {
            return false;
        }
    }

    @NonNls
    private static String getTextForOperator(IElementType operator) {
        if (JSTokenTypes.EQ.equals(operator)) {
            return "=";
        }
        if (JSTokenTypes.NE.equals(operator)) {
            return "!=";
        }
        if (JSTokenTypes.LE.equals(operator)) {
            return "<=";
        }
        if (JSTokenTypes.GE.equals(operator)) {
            return ">=";
        }
        if (JSTokenTypes.LT.equals(operator)) {
            return "<=";
        }
        if (JSTokenTypes.GT.equals(operator)) {
            return ">=";
        }
        if (JSTokenTypes.EQEQ.equals(operator)) {
            return "==";
        }
        if (JSTokenTypes.EQEQEQ.equals(operator)) {
            return "===";
        }
        if (JSTokenTypes.NEQEQ.equals(operator)) {
            return "!==";
        }
        if (JSTokenTypes.PLUSEQ.equals(operator)) {
            return "+=";
        }
        if (JSTokenTypes.MINUSEQ.equals(operator)) {
            return "-=";
        }
        if (JSTokenTypes.MULTEQ.equals(operator)) {
            return "*=";
        }
        if (JSTokenTypes.DIVEQ.equals(operator)) {
            return "/=";
        }
        if (JSTokenTypes.PERCEQ.equals(operator)) {
            return "%=";
        }
        if (JSTokenTypes.XOREQ.equals(operator)) {
            return "^=";
        }
        if (JSTokenTypes.ANDEQ.equals(operator)) {
            return "&=";
        }
        if (JSTokenTypes.OREQ.equals(operator)) {
            return "|=";
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
        if (JSTokenTypes.GTGTEQ.equals(operator)) {
            return ">>=";
        }
        if (JSTokenTypes.GTGTGT.equals(operator)) {
            return ">>>";
        }
        if (JSTokenTypes.GTGTGTEQ.equals(operator)) {
            return ">>>=";
        }
        return "unknown";
    }
}
