package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ComparisonUtils;
import com.sixrr.inspectjs.utils.ParenthesesUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.jetbrains.annotations.NonNls;

import java.util.HashSet;
import java.util.Set;

@ExtensionImpl
public class PointlessBooleanExpressionJSInspection extends JavaScriptInspection {
    private final BooleanLiteralComparisonFix fix = new BooleanLiteralComparisonFix();

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.pointlessBooleanExpressionDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new PointlessBooleanExpressionVisitor();
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        return args[0] instanceof JSBinaryExpression binaryExpression
            ? InspectionJSLocalize.pointlessBooleanErrorString(calculateSimplifiedBinaryExpression(binaryExpression)).get()
            : InspectionJSLocalize.pointlessBooleanErrorString(calculateSimplifiedPrefixExpression((JSPrefixExpression)args[0])).get();
    }

    @Nullable
    private String calculateSimplifiedBinaryExpression(JSBinaryExpression expression) {
        IElementType sign = expression.getOperationSign();
        JSExpression lhs = expression.getLOperand();

        JSExpression rhs = expression.getROperand();
        if (rhs == null) {
            return null;
        }
        String rhsText = rhs.getText();
        String lhsText = lhs.getText();
        if (JSTokenTypes.ANDAND.equals(sign) || JSTokenTypes.AND.equals(sign)) {
            return isTrue(lhs) ? rhsText : lhsText;
        } else if (JSTokenTypes.OROR.equals(sign) || JSTokenTypes.OR.equals(sign)) {
            return isFalse(lhs) ? rhsText : lhsText;
        } else if (JSTokenTypes.XOR.equals(sign) || JSTokenTypes.NE.equals(sign)
            /*|| // IMPORTANT: simplifiying !== changes code semantic
                sign.equals(JSTokenTypes.NEQEQ)*/) {
            if (isFalse(lhs)) {
                return rhsText;
            } else if (isFalse(rhs)) {
                return lhsText;
            } else if (isTrue(lhs)) {
                return createStringForNegatedExpression(rhs);
            } else {
                return createStringForNegatedExpression(lhs);
            }
        } else if (JSTokenTypes.EQEQ.equals(sign) /*|| // IMPORTANT: simplifiying === changes code semantic
                sign.equals(JSTokenTypes.EQEQEQ)*/) {
            if (isTrue(lhs)) {
                return rhsText;
            } else if (isTrue(rhs)) {
                return lhsText;
            } else if (isFalse(lhs)) {
                return createStringForNegatedExpression(rhs);
            } else {
                return createStringForNegatedExpression(lhs);
            }
        } else {
            return "";
        }
    }

    private static String createStringForNegatedExpression(JSExpression exp) {
        if (ComparisonUtils.isComparison(exp)) {
            JSBinaryExpression binaryExpression = (JSBinaryExpression) exp;
            IElementType sign = binaryExpression.getOperationSign();
            String negatedComparison = ComparisonUtils.getNegatedComparison(sign);
            JSExpression lhs = binaryExpression.getLOperand();
            JSExpression rhs = binaryExpression.getROperand();
            assert rhs != null;
            return lhs.getText() + negatedComparison + rhs.getText();
        } else {
            return ParenthesesUtils.getPrecendence(exp) > ParenthesesUtils.PREFIX_PRECEDENCE
                ? "!(" + exp.getText() + ')'
                : '!' + exp.getText();
        }
    }

    @NonNls
    private static String calculateSimplifiedPrefixExpression(JSPrefixExpression expression) {
        JSExpression operand = expression.getExpression();
        return isTrue(operand) ? "false" : "true";
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private class BooleanLiteralComparisonFix extends InspectionJSFix {
        @Nonnull
        @Override
        public LocalizeValue getName() {
            return InspectionJSLocalize.simplifyFix();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            PsiElement element = descriptor.getPsiElement();
            if (element instanceof JSBinaryExpression expression) {
                String replacementString = calculateSimplifiedBinaryExpression(expression);
                replaceExpression(expression, replacementString);
            } else {
                JSPrefixExpression expression = (JSPrefixExpression) element;
                String replacementString = calculateSimplifiedPrefixExpression(expression);
                replaceExpression(expression, replacementString);
            }
        }
    }

    private class PointlessBooleanExpressionVisitor extends BaseInspectionVisitor {
        private final Set<IElementType> booleanTokens = new HashSet<>(10);

        {
            booleanTokens.add(JSTokenTypes.ANDAND);
            booleanTokens.add(JSTokenTypes.AND);
            booleanTokens.add(JSTokenTypes.OROR);
            booleanTokens.add(JSTokenTypes.OR);
            booleanTokens.add(JSTokenTypes.XOR);
            booleanTokens.add(JSTokenTypes.EQEQ);
            //booleanTokens.add(JSTokenTypes.EQEQEQ); // === has strict semantic so do not report it
            booleanTokens.add(JSTokenTypes.NE);
            //booleanTokens.add(JSTokenTypes.NEQEQ); // !== has strict semantic so do not report it
        }

        @Override
        public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            if (!(expression.getROperand() != null)) {
                return;
            }
            IElementType sign = expression.getOperationSign();
            if (!booleanTokens.contains(sign)) {
                return;
            }
            JSExpression rhs = expression.getROperand();
            JSExpression lhs = expression.getLOperand();
            boolean isPointless;
            if (JSTokenTypes.EQEQ.equals(sign) /*||
                    JSTokenTypes.EQEQEQ.equals(sign) ||
                    JSTokenTypes.NE.equals(sign) ||
                    JSTokenTypes.NEQEQ.equals(sign)*/) {
                isPointless = equalityExpressionIsPointless(lhs, rhs);
            } else if (JSTokenTypes.ANDAND.equals(sign) ||
                    JSTokenTypes.AND.equals(sign)) {
                isPointless = andExpressionIsPointless(lhs, rhs);
            } else if (JSTokenTypes.OROR.equals(sign) ||
                    JSTokenTypes.OR.equals(sign)) {
                isPointless = orExpressionIsPointless(lhs, rhs);
            } else if (JSTokenTypes.XOR.equals(sign)) {
                isPointless = xorExpressionIsPointless(lhs, rhs);
            } else {
                isPointless = false;
            }
            if (!isPointless) {
                return;
            }
            registerError(expression);
        }

        @Override
        public void visitJSPrefixExpression(@Nonnull JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            IElementType sign = expression.getOperationSign();
            if (sign == null) {
                return;
            }
            JSExpression operand = expression.getExpression();
            if (JSTokenTypes.EXCL.equals(sign) &&
                    notExpressionIsPointless(operand)) {
                registerError(expression);
            }
        }
    }

    private static boolean equalityExpressionIsPointless(JSExpression lhs, JSExpression rhs) {
        return isTrue(lhs) || isTrue(rhs) || isFalse(lhs) || isFalse(rhs);
    }

    private static boolean andExpressionIsPointless(JSExpression lhs, JSExpression rhs) {
        return isTrue(lhs) || isTrue(rhs);
    }

    private static boolean orExpressionIsPointless(JSExpression lhs, JSExpression rhs) {
        return isFalse(lhs) /*|| isFalse(rhs) // since variable can be undefined*/;
    }

    private static boolean xorExpressionIsPointless(JSExpression lhs, JSExpression rhs) {
        return isTrue(lhs) || isTrue(rhs) || isFalse(lhs) || isFalse(rhs);
    }

    private static boolean notExpressionIsPointless(JSExpression arg) {
        return isFalse(arg) || isTrue(arg);      //TODO: double negation
    }

    private static boolean isTrue(JSExpression expression) {
        if (expression == null || !(expression instanceof JSLiteralExpression)) {
            return false;
        }
        @NonNls String text = expression.getText();
        return "true".equals(text);
    }

    private static boolean isFalse(JSExpression expression) {
        if (expression == null || !(expression instanceof JSLiteralExpression)) {
            return false;
        }
        @NonNls String text = expression.getText();
        return "false".equals(text);
    }
}
