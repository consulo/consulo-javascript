package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.ui.SingleIntegerFieldOptionsPanel;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.HashSet;
import java.util.Set;

public class OverlyComplexArithmeticExpressionJSInspection extends JavaScriptInspection {
    private static final int TERM_LIMIT = 6;

    @SuppressWarnings({"PublicField"})
    public int m_limit = TERM_LIMIT;

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("overly.complex.arithmetic.expression.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    private int getLimit() {
        return m_limit;
    }

    @Override
	public JComponent createOptionsPanel() {
        return new SingleIntegerFieldOptionsPanel(InspectionJSBundle.message("maximum.number.of.terms.parameter"),
                this, "m_limit");
    }

    @Override
	protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("overly.complex.arithmetic.expression.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor {
        private final Set<IElementType> arithmeticTokens = new HashSet<IElementType>(5);

        {
            arithmeticTokens.add(JSTokenTypes.PLUS);
            arithmeticTokens.add(JSTokenTypes.MINUS);
            arithmeticTokens.add(JSTokenTypes.MULT);
            arithmeticTokens.add(JSTokenTypes.DIV);
            arithmeticTokens.add(JSTokenTypes.PERC);
        }

        @Override public void visitJSBinaryExpression(@NotNull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            checkExpression(expression);
        }

        @Override public void visitJSPrefixExpression(@NotNull JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            checkExpression(expression);
        }

        @Override public void visitJSParenthesizedExpression(JSParenthesizedExpression expression) {
            super.visitJSParenthesizedExpression(expression);
            checkExpression(expression);
        }

        private void checkExpression(JSExpression expression) {
            if (isParentArithmetic(expression)) {
                return;
            }
            if (!isArithmetic(expression)) {
                return;
            }
            if (containsStringConcatenation(expression)) {
                return;
            }
            final int numTerms = countTerms(expression);
            if (numTerms <= getLimit()) {
                return;
            }
            registerError(expression);
        }

        private int countTerms(JSExpression expression) {
            if (expression == null) {
                return 0;
            }
            if (!isArithmetic(expression)) {
                return 1;
            }
            if (expression instanceof JSBinaryExpression) {
                final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;
                final JSExpression lhs = binaryExpression.getLOperand();
                final JSExpression rhs = binaryExpression.getROperand();
                return countTerms(lhs) + countTerms(rhs);
            } else if (expression instanceof JSPrefixExpression) {
                final JSPrefixExpression prefixExpression = (JSPrefixExpression) expression;
                final JSExpression operand = prefixExpression.getExpression();
                return countTerms(operand);
            } else if (expression instanceof JSParenthesizedExpression) {
                final JSParenthesizedExpression parenthesizedExpression = (JSParenthesizedExpression) expression;
                final JSExpression contents = parenthesizedExpression.getInnerExpression();
                return countTerms(contents);
            }
            return 1;
        }

        private boolean isParentArithmetic(JSExpression expression) {
            final PsiElement parent = expression.getParent();
            if (!(parent instanceof JSExpression)) {
                return false;
            }
            return isArithmetic((JSExpression) parent);
        }

        private boolean isArithmetic(JSExpression expression) {
            if (expression instanceof JSBinaryExpression) {

                final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;
                final IElementType sign = binaryExpression.getOperationSign();
                return arithmeticTokens.contains(sign);
            } else if (expression instanceof JSPrefixExpression) {
                final JSPrefixExpression prefixExpression = (JSPrefixExpression) expression;
                final IElementType sign = prefixExpression.getOperationSign();
                return arithmeticTokens.contains(sign);
            } else if (expression instanceof JSParenthesizedExpression) {
                final JSParenthesizedExpression parenthesizedExpression = (JSParenthesizedExpression) expression;
                final JSExpression contents = parenthesizedExpression.getInnerExpression();
                return isArithmetic(contents);
            }
            return false;
        }

        private boolean containsStringConcatenation(JSExpression expression) {
            if (isStringLiteral(expression)) {
                return true;
            }
            if (expression instanceof JSBinaryExpression) {

                final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;
                final JSExpression lhs = binaryExpression.getLOperand();

                if (containsStringConcatenation(lhs)) {
                    return true;
                }
                final JSExpression rhs = binaryExpression.getROperand();
                return containsStringConcatenation(rhs);
            } else if (expression instanceof JSPrefixExpression) {
                final JSPrefixExpression prefixExpression = (JSPrefixExpression) expression;
                final IElementType sign = prefixExpression.getOperationSign();
                return arithmeticTokens.contains(sign);
            } else if (expression instanceof JSParenthesizedExpression) {
                final JSParenthesizedExpression parenthesizedExpression = (JSParenthesizedExpression) expression;
                final JSExpression contents = parenthesizedExpression.getInnerExpression();
                return containsStringConcatenation(contents);
            }
            return false;
        }

        private boolean isStringLiteral(JSExpression expression) {
            if (expression instanceof JSLiteralExpression) {
                final JSLiteralExpression literal = (JSLiteralExpression) expression;
                final String text = literal.getText();
                return text.startsWith("'") || text.startsWith("\"");
            }
            if (expression instanceof JSReferenceExpression) {
                final JSReferenceExpression reference = (JSReferenceExpression) expression;
                final PsiElement referent = reference.resolve();
                if (referent instanceof JSVariable) {
                    final JSVariable variable = (JSVariable) referent;
                    if (variable.isConst()) {
                        final JSExpression initializer = variable.getInitializer();
                        if (initializer != null) {
                            final String text = initializer.getText();
                            return text.startsWith("'") || text.startsWith("\"");
                        }
                    }
                }
            }
            return false;
        }
    }
}
