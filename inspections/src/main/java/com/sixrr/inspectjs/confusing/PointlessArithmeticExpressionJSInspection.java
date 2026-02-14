package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import org.jetbrains.annotations.NonNls;

import java.util.HashSet;
import java.util.Set;

@ExtensionImpl
public class PointlessArithmeticExpressionJSInspection extends JavaScriptInspection {
    private final PointlessArithmeticFix fix = new PointlessArithmeticFix();

    @Override
    @Nonnull
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.pointlessArithmeticExpressionDisplayName();
    }

    @Override
    @Nonnull
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.pointlessArithmeticErrorMessage(calculateReplacementExpression((JSExpression)args[0])).get();
    }

    private String calculateReplacementExpression(JSExpression expression) {
        JSBinaryExpression exp = (JSBinaryExpression) expression;
        IElementType sign = exp.getOperationSign();
        JSExpression lhs = exp.getLOperand();
        JSExpression rhs = exp.getROperand();
        assert rhs != null;
        if (JSTokenTypes.PLUS.equals(sign)) {
            if (isZero(lhs)) {
                return rhs.getText();
            } else {
                return lhs.getText();
            }
        } else if (JSTokenTypes.MINUS.equals(sign)) {
            return lhs.getText();
        } else if (JSTokenTypes.MULT.equals(sign)) {
            if (isOne(lhs)) {
                return rhs.getText();
            } else if (isOne(rhs)) {
                return lhs.getText();
            } else {
                return "0";
            }
        } else if (JSTokenTypes.DIV.equals(sign)) {
            return lhs.getText();
        } else {
            return "";
        }
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new PointlessArithmeticVisitor();
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private class PointlessArithmeticFix extends InspectionJSFix {
        @Nonnull
        @Override
        public LocalizeValue getName() {
            return InspectionJSLocalize.simplifyFix();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            JSExpression expression = (JSExpression) descriptor.getPsiElement();
            String newExpression = calculateReplacementExpression(expression);
            replaceExpression(expression, newExpression);
        }
    }

    private class PointlessArithmeticVisitor extends BaseInspectionVisitor {
        private final Set<IElementType> arithmeticTokens = new HashSet<>(4);

        {
            arithmeticTokens.add(JSTokenTypes.PLUS);
            arithmeticTokens.add(JSTokenTypes.MINUS);
            arithmeticTokens.add(JSTokenTypes.MULT);
            arithmeticTokens.add(JSTokenTypes.DIV);
        }

        @Override
        public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            if (!(expression.getROperand() != null)) {
                return;
            }
            IElementType sign = expression.getOperationSign();
            if (!arithmeticTokens.contains(sign)) {
                return;
            }
            JSExpression rhs = expression.getROperand();
            JSExpression lhs = expression.getLOperand();

            if (rhs == null) {
                return;
            }

            boolean isPointless;
            if (sign.equals(JSTokenTypes.PLUS)) {
                isPointless = additionExpressionIsPointless(lhs, rhs);
            } else if (sign.equals(JSTokenTypes.MINUS)) {
                isPointless = subtractionExpressionIsPointless(rhs);
            } else if (sign.equals(JSTokenTypes.MULT)) {
                isPointless = multiplyExpressionIsPointless(lhs, rhs);
            } else if (sign.equals(JSTokenTypes.DIV)) {
                isPointless = divideExpressionIsPointless(rhs);
            } else {
                isPointless = false;
            }
            if (!isPointless) {
                return;
            }

            registerError(expression);
        }
    }

    private boolean subtractionExpressionIsPointless(JSExpression rhs) {
        return isZero(rhs);
    }

    private boolean additionExpressionIsPointless(JSExpression lhs, JSExpression rhs) {
        return (isZero(lhs) && !isString(rhs)) || (isZero(rhs) && !isString(lhs));
    }

    private boolean multiplyExpressionIsPointless(JSExpression lhs, JSExpression rhs) {
        return isZero(lhs) || isZero(rhs) || isOne(lhs) || isOne(rhs);
    }

    private boolean divideExpressionIsPointless(JSExpression rhs) {
        return isOne(rhs);
    }

    /**
     * @noinspection FloatingPointEquality
     */
    private static boolean isZero(JSExpression expression) {
        @NonNls String text = expression.getText();
        return text.equals("0")||
                text.equals("0x0")||
                text.equals("0X0")||
                text.equals("0.0")||
                text.equals("0L")||
                text.equals("0l");
    }

    private static boolean isString(JSExpression expression) {
        if (expression instanceof JSLiteralExpression) {
            String s = expression.getText();
            return s.startsWith("'") || s.startsWith("\"");
        }
        return false;
    }

    /**
     * @noinspection FloatingPointEquality
     */
    private static boolean isOne(JSExpression expression) {
        @NonNls String text = expression.getText();
        return text.equals("1") ||
                text.equals("0x1") ||
                text.equals("0X1") ||
                text.equals("1.0") ||
                text.equals("1L") ||
                text.equals("1l");
    }
}
