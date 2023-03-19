package com.sixrr.inspectjs.confusing;

import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import consulo.language.psi.PsiElement;
import consulo.language.ast.IElementType;
import consulo.language.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.ComparisonUtils;
import com.sixrr.inspectjs.utils.ParenthesesUtils;
import consulo.project.Project;
import org.jetbrains.annotations.NonNls;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.HashSet;
import java.util.Set;

@ExtensionImpl
public class PointlessBooleanExpressionJSInspection extends JavaScriptInspection {

    private final BooleanLiteralComparisonFix fix =
            new BooleanLiteralComparisonFix();

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("pointless.boolean.expression.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
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

    @RequiredReadAction
	@Override
	public String buildErrorString(Object state, Object... args) {
        if ( args[0]instanceof JSBinaryExpression) {
            return InspectionJSBundle.message("pointless.boolean.error.string", calculateSimplifiedBinaryExpression((JSBinaryExpression) args[0]));
        } else {
            return InspectionJSBundle.message("pointless.boolean.error.string", calculateSimplifiedPrefixExpression((JSPrefixExpression) args[0]));
        }
    }

    @Nullable
    private String calculateSimplifiedBinaryExpression(JSBinaryExpression expression) {
        final IElementType sign = expression.getOperationSign();
        final JSExpression lhs = expression.getLOperand();

        final JSExpression rhs = expression.getROperand();
        if (rhs == null) {
            return null;
        }
        final String rhsText = rhs.getText();
        final String lhsText = lhs.getText();
        if (JSTokenTypes.ANDAND.equals(sign) ||
                JSTokenTypes.AND.equals(sign)) {
            if (isTrue(lhs)) {
                return rhsText;
            } else {
                return lhsText;
            }
        } else if (JSTokenTypes.OROR.equals(sign) ||
                JSTokenTypes.OR.equals(sign)) {
            if (isFalse(lhs)) {
                return rhsText;
            } else {
                return lhsText;
            }
        } else if (JSTokenTypes.XOR.equals(sign) ||
                JSTokenTypes.NE.equals(sign) /*|| // IMPORTANT: simplifiying !== changes code semantic
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
            final JSBinaryExpression binaryExpression =
                    (JSBinaryExpression) exp;
            final IElementType sign = binaryExpression.getOperationSign();
            final String negatedComparison =
                    ComparisonUtils.getNegatedComparison(sign);
            final JSExpression lhs = binaryExpression.getLOperand();
            final JSExpression rhs = binaryExpression.getROperand();
            assert rhs != null;
            return lhs.getText() + negatedComparison + rhs.getText();
        } else {
            if (ParenthesesUtils.getPrecendence(exp) >
                    ParenthesesUtils.PREFIX_PRECEDENCE) {
                return "!(" + exp.getText() + ')';
            } else {
                return '!' + exp.getText();
            }
        }
    }

    @NonNls
    private static String calculateSimplifiedPrefixExpression(JSPrefixExpression expression) {
        final JSExpression operand = expression.getExpression();
        if (isTrue(operand)) {
            return "false";
        } else {
            return "true";
        }
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private class BooleanLiteralComparisonFix
            extends InspectionJSFix {
        @Override
		@Nonnull
        public String getName() {
            return InspectionJSBundle.message("simplify.fix");
        }

        @Override
		public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final PsiElement element = descriptor.getPsiElement();
            if (element instanceof JSBinaryExpression) {
                final JSBinaryExpression expression =
                        (JSBinaryExpression) element;
                final String replacementString =
                        calculateSimplifiedBinaryExpression(expression);
                replaceExpression(expression, replacementString);
            } else {
                final JSPrefixExpression expression =
                        (JSPrefixExpression) element;
                final String replacementString =
                        calculateSimplifiedPrefixExpression(expression);
                replaceExpression(expression, replacementString);
            }
        }
    }

    private class PointlessBooleanExpressionVisitor
            extends BaseInspectionVisitor {
        private final Set<IElementType> booleanTokens =
                new HashSet<IElementType>(10);

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

        @Override public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            if (!(expression.getROperand() != null)) {
                return;
            }
            final IElementType sign = expression.getOperationSign();
            if (!booleanTokens.contains(sign)) {
                return;
            }
            final JSExpression rhs = expression.getROperand();
            final JSExpression lhs = expression.getLOperand();
            final boolean isPointless;
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

        @Override public void visitJSPrefixExpression(@Nonnull JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            final IElementType sign = expression.getOperationSign();
            if (sign == null) {
                return;
            }
            final JSExpression operand = expression.getExpression();
            if (JSTokenTypes.EXCL.equals(sign) &&
                    notExpressionIsPointless(operand)) {
                registerError(expression);
            }
        }
    }

    private static boolean equalityExpressionIsPointless(JSExpression lhs,
                                                  JSExpression rhs) {
        return isTrue(lhs) || isTrue(rhs) || isFalse(lhs) || isFalse(rhs);
    }

    private static boolean andExpressionIsPointless(JSExpression lhs,
                                             JSExpression rhs) {
        return isTrue(lhs) || isTrue(rhs);
    }

    private static boolean orExpressionIsPointless(JSExpression lhs,
                                            JSExpression rhs) {
        return isFalse(lhs) /*|| isFalse(rhs) // since variable can be undefined*/;
    }

    private static boolean xorExpressionIsPointless(JSExpression lhs,
                                             JSExpression rhs) {
        return isTrue(lhs) || isTrue(rhs) || isFalse(lhs) || isFalse(rhs);
    }

    private static boolean notExpressionIsPointless(JSExpression arg) {
        return isFalse(arg) || isTrue(arg);      //TODO: double negation
    }

    private static boolean isTrue(JSExpression expression) {
        if (expression == null) {
            return false;
        }
        if (!(expression instanceof JSLiteralExpression)) {
            return false;
        }
        @NonNls final String text = expression.getText();
        return "true".equals(text);
    }

    private static boolean isFalse(JSExpression expression) {
        if (expression == null) {
            return false;
        }
        if (!(expression instanceof JSLiteralExpression)) {
            return false;
        }
        @NonNls final String text = expression.getText();
        return "false".equals(text);
    }
}
