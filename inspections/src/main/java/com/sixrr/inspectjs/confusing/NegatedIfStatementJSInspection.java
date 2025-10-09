package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.BoolUtils;
import com.sixrr.inspectjs.utils.ParenthesesUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiComment;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.jetbrains.annotations.NonNls;

@ExtensionImpl
public class NegatedIfStatementJSInspection extends JavaScriptInspection {
    private final NegatedIfElseFix fix = new NegatedIfElseFix();

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.negatedIfStatementDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.negatedRefStatementErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    @Override
    protected InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class NegatedIfElseFix extends InspectionJSFix {
        @Nonnull
        @Override
        public LocalizeValue getName() {
            return InspectionJSLocalize.invertIfConditionFix();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final PsiElement ifToken = descriptor.getPsiElement();
            final JSIfStatement ifStatement = (JSIfStatement)ifToken.getParent();
            assert ifStatement != null;
            final JSStatement elseBranch = ifStatement.getElse();
            final JSStatement thenBranch = ifStatement.getThen();
            final JSExpression condition = ifStatement.getCondition();
            final String negatedCondition = BoolUtils.getNegatedExpressionText(condition);
            String elseText = elseBranch.getText();
            final PsiElement lastChild = elseBranch.getLastChild();
            if (lastChild instanceof PsiComment) {
                final PsiComment comment = (PsiComment)lastChild;
                final IElementType tokenType = comment.getTokenType();
                if (JSTokenTypes.END_OF_LINE_COMMENT.equals(tokenType)) {
                    elseText += '\n';
                }
            }
            @NonNls final String newStatement = "if(" + negatedCondition + ')' + elseText + " else " + thenBranch.getText();
            replaceStatement(ifStatement, newStatement);
        }
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSIfStatement(JSIfStatement statement) {
            super.visitJSIfStatement(statement);
            final PsiElement parent = statement.getParent();
            if (parent instanceof JSIfStatement parentStatement) {
                final JSStatement elseBranch = parentStatement.getElse();
                if (statement.equals(elseBranch)) {
                    return;
                }
            }
            if (statement.getElse() == null) {
                return;
            }

            JSExpression condition = statement.getCondition();
            condition = ParenthesesUtils.stripExpression(condition);
            if (condition == null || (!BoolUtils.isNegation(condition) && !isNotEquals(condition))) {
                return;
            }
            registerStatementError(statement);
        }

        private boolean isNotEquals(JSExpression expression) {
            if (!(expression instanceof JSBinaryExpression)) {
                return false;
            }
            final JSBinaryExpression binaryExpression = (JSBinaryExpression)expression;
            final IElementType sign = binaryExpression.getOperationSign();
            return JSTokenTypes.NE.equals(sign) || JSTokenTypes.NEQEQ.equals(sign);
        }
    }
}
