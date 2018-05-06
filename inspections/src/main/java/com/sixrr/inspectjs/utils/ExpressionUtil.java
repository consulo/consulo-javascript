package com.sixrr.inspectjs.utils;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.tree.IElementType;
import consulo.javascript.psi.JSSimpleLiteralExpression;

import java.util.HashMap;
import java.util.Map;

import javax.annotation.Nullable;

public class ExpressionUtil {
    private ExpressionUtil() {
    }

    public static boolean isConstantExpression(JSExpression expression) {
        if (expression == null) {
            return false;
        }
        final IsConstantExpressionVisitor visitor = new IsConstantExpressionVisitor();
        expression.accept(visitor);
        return visitor.isConstant;
    }

    private static class IsConstantExpressionVisitor extends JSElementVisitor {
        private boolean isConstant = false;
        private final Map<JSVariable, Boolean> isVariableConstant = new HashMap<JSVariable, Boolean>();

        public boolean isConstant() {
            return isConstant;
        }

        @Override public void visitJSExpression(JSExpression expression) {
            isConstant = false;
        }

        @Override public void visitJSLiteralExpression(JSSimpleLiteralExpression expression) {
            isConstant = true;
        }

        @Override public void visitJSParenthesizedExpression(JSParenthesizedExpression expression) {
            final JSExpression expr = expression.getInnerExpression();
            if (expr != null) {
                expr.accept(this);
            }
        }

        @Override public void visitJSPrefixExpression(JSPrefixExpression expression) {
            final JSExpression operand = expression.getExpression();
            if (operand == null) {
                isConstant = false;
                return;
            }

            operand.accept(this);
            if (!isConstant) {
                return;
            }

            final IElementType opType = expression.getOperationSign();

            if (JSTokenTypes.PLUS.equals(opType) ||
                    JSTokenTypes.MINUS.equals(opType) ||
                    JSTokenTypes.TILDE.equals(opType) ||
                    JSTokenTypes.EXCL.equals(opType)) {
                return;
            }
            isConstant = false;
        }

        @Override public void visitJSBinaryExpression(JSBinaryExpression expression) {
            expression.getLOperand().accept(this);
            if (!isConstant) {
                return;
            }
            final JSExpression rOperand = expression.getROperand();
            if (rOperand != null) {
                rOperand.accept(this);
            }
        }

        @Override public void visitJSConditionalExpression(JSConditionalExpression expression) {
            final JSExpression thenExpr = expression.getThen();
            final JSExpression elseExpr = expression.getElse();
            if (thenExpr == null || elseExpr == null) {
                isConstant = false;
                return;
            }

            expression.getCondition().accept(this);
            if (!isConstant) {
                return;
            }
            thenExpr.accept(this);
            if (!isConstant) {
                return;
            }
            elseExpr.accept(this);
        }

        @Override public void visitJSReferenceExpression(JSReferenceExpression expression) {
            final JSElement refElement = (JSElement) expression.resolve();

            if (!(refElement instanceof JSVariable)) {
                isConstant = false;
                return;
            }

            final JSVariable variable = (JSVariable) refElement;
            final Boolean isConst = isVariableConstant.get(variable);

            if (isConst != null) {
                isConstant &= isConst;
                return;
            }

            isVariableConstant.put(variable, Boolean.FALSE);
            if (!variable.hasInitializer()) {
                isConstant = false;
                return;
            }

            final JSExpression initializer = variable.getInitializer();

            initializer.accept(this);
            isVariableConstant.put(variable, isConstant);
        }
    }

    @Nullable
    public static Object computeConstantExpression(JSExpression expression) {
        if (expression == null) {
            return null;
        }

        //try {
        //    final PsiManager manager = expression.getManager();
        //    final PsiElementFactory factory = manager.getElementFactory();
        //    final PsiConstantEvaluationHelper helper = manager.getConstantEvaluationHelper();
        //    PsiExpression expr = factory.createExpressionFromText(expression.getText(), expression);
        //    final PsiType exprType = expr.getType();
        //
        //    if (exprType != null && exprType.equals(PsiType.INT)) {
        //        expr = factory.createExpressionFromText("(int)" + expression.getText(), expression);
        //    }
        //
        //    return helper.computeConstantExpression(expr);
        //} catch (IncorrectOperationException ignore) {
        //    return null;
        //}
        // TODO: constant expressions are not evaluated by PsiManager et al
        return null;
    }
}
