package com.sixrr.inspectjs.bitwise;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.ExpressionUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;

import javax.annotation.Nonnull;
import java.util.HashSet;
import java.util.Set;

@ExtensionImpl
public class PointlessBitwiseExpressionJSInspection extends JavaScriptInspection
{
	static final Set<IElementType> bitwiseTokens = new HashSet<IElementType>(6);

	static
	{
		bitwiseTokens.add(JSTokenTypes.AND);
		bitwiseTokens.add(JSTokenTypes.OR);
		bitwiseTokens.add(JSTokenTypes.XOR);
		bitwiseTokens.add(JSTokenTypes.LTLT);
		bitwiseTokens.add(JSTokenTypes.GTGT);
		bitwiseTokens.add(JSTokenTypes.GTGTGT);
	}

	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSBundle.message(
				"pointless.bitwise.expression.display.name");
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.BITWISE_GROUP_NAME;
	}

	@RequiredReadAction
	@Override
	@Nonnull
	public String buildErrorString(Object state, Object... args)
	{
		final String replacementExpression = calculateReplacementExpression((JSExpression) args[0], (PointlessBitwiseExpressionJSInspectionState) state);
		return InspectionJSBundle.message(
				"pointless.bitwise.expression.problem.descriptor",
				replacementExpression);
	}

	@Override
	public boolean isEnabledByDefault()
	{
		return true;
	}

	@Nonnull
	@Override
	public InspectionToolState<?> createStateProvider()
	{
		return new PointlessBitwiseExpressionJSInspectionState();
	}

	@RequiredReadAction
	String calculateReplacementExpression(JSExpression expression, PointlessBitwiseExpressionJSInspectionState state)
	{
		final JSBinaryExpression exp = (JSBinaryExpression) expression;
		final JSExpression lhs = exp.getLOperand();
		final JSExpression rhs = exp.getROperand();
		final IElementType tokenType = exp.getOperationSign();
		assert rhs != null;
		if(tokenType.equals(JSTokenTypes.AND))
		{
			if(isZero(lhs, state) || isAllOnes(rhs, state))
			{
				return lhs.getText();
			}
			else
			{
				return rhs.getText();
			}
		}
		else if(tokenType.equals(JSTokenTypes.OR))
		{
			if(isZero(lhs, state) || isAllOnes(rhs, state))
			{
				return rhs.getText();
			}
			else
			{
				return lhs.getText();
			}
		}
		else if(tokenType.equals(JSTokenTypes.XOR))
		{
			if(isAllOnes(lhs, state))
			{
				return '~' + rhs.getText();
			}
			else if(isAllOnes(rhs, state))
			{
				return '~' + lhs.getText();
			}
			else if(isZero(rhs, state))
			{
				return lhs.getText();
			}
			else
			{
				return rhs.getText();
			}
		}
		else if(tokenType.equals(JSTokenTypes.LTLT) ||
				tokenType.equals(JSTokenTypes.GTGT) ||
				tokenType.equals(JSTokenTypes.GTGTGT))
		{
			return lhs.getText();
		}
		else
		{
			return "";
		}
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new PointlessBitwiseVisitor();
	}

	@Override
	public InspectionJSFix buildFix(PsiElement location, Object state)
	{
		return new PointlessBitwiseFix(state);
	}

	private class PointlessBitwiseFix extends InspectionJSFix
	{
		private final PointlessBitwiseExpressionJSInspectionState myState;

		public PointlessBitwiseFix(Object state)
		{
			myState = (PointlessBitwiseExpressionJSInspectionState) state;
		}

		@Override
		@Nonnull
		public String getName()
		{
			return InspectionJSBundle.message(
					"pointless.bitwise.expression.simplify.quickfix");
		}

		@Override
		@RequiredReadAction
		public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException
		{
			final JSExpression expression = (JSExpression) descriptor.getPsiElement();
			final String newExpression = calculateReplacementExpression(expression, myState);
			replaceExpression(expression, newExpression);
		}
	}

	private class PointlessBitwiseVisitor extends BaseInspectionVisitor<PointlessBitwiseExpressionJSInspectionState>
	{
		@Override
		@RequiredReadAction
		public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression)
		{
			super.visitJSBinaryExpression(expression);
			final IElementType sign = expression.getOperationSign();
			if(!bitwiseTokens.contains(sign))
			{
				return;
			}

			final JSExpression rhs = expression.getROperand();
			if(rhs == null)
			{
				return;
			}

			final JSExpression lhs = expression.getLOperand();

			final boolean isPointless;
			if(JSTokenTypes.AND.equals(sign))
			{
				isPointless = andExpressionIsPointless(lhs, rhs);
			}
			else if(JSTokenTypes.OR.equals(sign))
			{
				isPointless = orExpressionIsPointless(lhs, rhs);
			}
			else if(JSTokenTypes.XOR.equals(sign))
			{
				isPointless = xorExpressionIsPointless(lhs, rhs);
			}
			else if(JSTokenTypes.LTLT.equals(sign) ||
					JSTokenTypes.GTGT.equals(sign) ||
					JSTokenTypes.GTGTGT.equals(sign))
			{
				isPointless = shiftExpressionIsPointless(rhs);
			}
			else
			{
				isPointless = false;
			}
			if(isPointless)
			{
				registerError(expression, expression);
			}
		}

		private boolean andExpressionIsPointless(JSExpression lhs,
												 JSExpression rhs)
		{
			return isZero(lhs, myState) || isZero(rhs, myState)
					|| isAllOnes(lhs, myState) || isAllOnes(rhs, myState);
		}

		private boolean orExpressionIsPointless(JSExpression lhs,
												JSExpression rhs)
		{
			return isZero(lhs, myState) || isZero(rhs, myState)
					|| isAllOnes(lhs, myState) || isAllOnes(rhs, myState);
		}

		private boolean xorExpressionIsPointless(JSExpression lhs,
												 JSExpression rhs)
		{
			return isZero(lhs, myState) || isZero(rhs, myState)
					|| isAllOnes(lhs, myState) || isAllOnes(rhs, myState);
		}

		private boolean shiftExpressionIsPointless(JSExpression rhs
		)
		{
			return isZero(rhs, myState);
		}
	}

	private boolean isZero(JSExpression expression, PointlessBitwiseExpressionJSInspectionState state)
	{
		if(state.m_ignoreExpressionsContainingConstants && !(expression instanceof JSLiteralExpression))
		{
			return false;
		}
		final Object value =
				ExpressionUtil.computeConstantExpression(expression);
		return value instanceof Integer && (Integer) value == 0;
	}

	private boolean isAllOnes(JSExpression expression, PointlessBitwiseExpressionJSInspectionState state)
	{
		if(state.m_ignoreExpressionsContainingConstants && !(expression instanceof JSLiteralExpression))
		{
			return false;
		}
		final Object value =
				ExpressionUtil.computeConstantExpression(expression);
		return value != null && value instanceof Integer && (Integer) value == 0xffffffff;
	}
}