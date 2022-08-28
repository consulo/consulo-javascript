/*
 * Copyright 2005-2006 Olivier Descout
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.intellij.idea.lang.javascript.psiutil;

import java.util.HashMap;
import java.util.Map;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.ast.IElementType;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import consulo.language.psi.PsiElement;

public class ExpressionUtil {

    private ExpressionUtil() {}

    public static boolean isConstantExpression(JSExpression expression) {
        if (expression == null) {
            return false;
        }
        IsConstantExpressionVisitor visitor = new IsConstantExpressionVisitor();
        expression.accept(visitor);
        return visitor.isConstant;
    }

    public static boolean isIncrementDecrementExpression(@Nonnull PsiElement expression) {
        if (expression instanceof JSPostfixExpression) {
            final IElementType operator = ((JSPostfixExpression) expression).getOperationSign();
            return (JSTokenTypes.PLUSPLUS.equals(operator) || JSTokenTypes.MINUSMINUS.equals(operator));
        } else if (expression instanceof JSPrefixExpression) {
            final IElementType operator = ((JSPrefixExpression) expression).getOperationSign();
            return (JSTokenTypes.PLUSPLUS.equals(operator) || JSTokenTypes.MINUSMINUS.equals(operator));
        } else {
            return false;
        }
    }

    public static class IsConstantExpressionVisitor extends JSElementVisitor {
        protected boolean                        isConstant;
        private   final Map<JSVariable, Boolean> isVariableConstant = new HashMap<JSVariable, Boolean>();

        public boolean isConstant() {
            return this.isConstant;
        }

        @Override public void visitJSExpression(JSExpression expression) {
            this.isConstant = false;
        }

        @Override public void visitJSLiteralExpression(JSSimpleLiteralExpression expression) {
            this.isConstant = true;
        }

        @Override public void visitJSParenthesizedExpression(JSParenthesizedExpression expression) {
            JSExpression expr = expression.getInnerExpression();
            if (expr != null) {
                expr.accept(this);
            }
        }

        @Override public void visitJSPrefixExpression(JSPrefixExpression expression) {
            this.visitJSPrefixOrPostfixExpression(expression.getExpression(), expression.getOperationSign());
        }

        @Override public void visitJSPostfixExpression(JSPostfixExpression expression) {
            this.visitJSPrefixOrPostfixExpression(expression.getExpression(), expression.getOperationSign());
        }

        public void visitJSPrefixOrPostfixExpression(JSExpression operand, IElementType sign) {
            if (operand == null) {
                this.isConstant = false;
                return;
            }

            operand.accept(this);
            if (!this.isConstant) {
                return;
            }

            if (sign == JSTokenTypes.PLUS  ||
                sign == JSTokenTypes.MINUS ||
                sign == JSTokenTypes.TILDE ||
                sign == JSTokenTypes.EXCL) {
                return;
            }
            this.isConstant = false;
        }

        @Override public void visitJSBinaryExpression(JSBinaryExpression expression) {
            final JSExpression jsExpression = expression.getLOperand();
            if (jsExpression == null) return;
            jsExpression.accept(this);
            if (!this.isConstant) {
                return;
            }
            JSExpression rOperand = expression.getROperand();
            if (rOperand != null) {
                rOperand.accept(this);
            }
        }

        @Override public void visitJSConditionalExpression(JSConditionalExpression expression) {
            JSExpression thenExpr = expression.getThen();
            JSExpression elseExpr = expression.getElse();
            if (thenExpr == null || elseExpr == null) {
                this.isConstant = false;
                return;
            }

            expression.getCondition().accept(this);
            if (!this.isConstant) {
                return;
            }
            thenExpr.accept(this);
            if (!this.isConstant) {
                return;
            }
            elseExpr.accept(this);
        }

        @Override public void visitJSReferenceExpression(JSReferenceExpression expression) {
            JSElement refElement = (JSElement) expression.resolve();

            if (!(refElement instanceof JSVariable)) {
                this.isConstant = false;
                return;
            }

            JSVariable variable = (JSVariable) refElement;
            Boolean    isConst  = this.isVariableConstant.get(variable);

            if (isConst != null) {
                this.isConstant &= isConst;
                return;
            }

            this.isVariableConstant.put(variable, Boolean.FALSE);
            this.isConstant = false;
        }
    }

    @Nullable
    public static Object computeConstantExpression(JSExpression expression) {
        if (expression == null) {
            return null;
        }

        //try {
        //    final PsiManager                  manager  = expression.getManager();
        //    final PsiElementFactory           factory  = manager.getElementFactory();
        //    final PsiConstantEvaluationHelper helper   = manager.getConstantEvaluationHelper();
        //    PsiExpression                     expr     = factory.createExpressionFromText(expression.getText(), expression);
        //    final PsiType                     exprType = expr.getType();
        //
        //    if (exprType != null && exprType.equals(PsiType.INT)) {
        //        expr = factory.createExpressionFromText("(long)" + expression.getText(), expression);
        //    }
        //
        //    return helper.computeConstantExpression(expr);
        //} catch (IncorrectOperationException e) {
        //    return null;
        //}

      // TODO: constant expressions are not evaluated by PsiManager et al
      return null;
    }
}
