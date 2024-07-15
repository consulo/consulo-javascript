package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;

import java.util.HashSet;
import java.util.Set;

@ExtensionImpl
public class OverlyComplexArithmeticExpressionJSInspection extends JavaScriptInspection
{
	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSLocalize.overlyComplexArithmeticExpressionDisplayName().get();
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.CONFUSING_GROUP_NAME.get();
	}

	@Nonnull
	@Override
	public InspectionToolState<?> createStateProvider()
	{
		return new OverlyComplexArithmeticExpressionJSInspectionState();
	}

	@RequiredReadAction
	@Override
	protected String buildErrorString(Object state, Object... args)
	{
		return InspectionJSLocalize.overlyComplexArithmeticExpressionErrorString().get();
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new Visitor();
	}

	private class Visitor extends BaseInspectionVisitor<OverlyComplexArithmeticExpressionJSInspectionState>
	{
		private final Set<IElementType> arithmeticTokens = new HashSet<>(5);

		public Visitor()
		{
			arithmeticTokens.add(JSTokenTypes.PLUS);
			arithmeticTokens.add(JSTokenTypes.MINUS);
			arithmeticTokens.add(JSTokenTypes.MULT);
			arithmeticTokens.add(JSTokenTypes.DIV);
			arithmeticTokens.add(JSTokenTypes.PERC);
		}

		@Override
		public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression)
		{
			super.visitJSBinaryExpression(expression);
			checkExpression(expression);
		}

		@Override
		public void visitJSPrefixExpression(@Nonnull JSPrefixExpression expression)
		{
			super.visitJSPrefixExpression(expression);
			checkExpression(expression);
		}

		@Override
		public void visitJSParenthesizedExpression(JSParenthesizedExpression expression)
		{
			super.visitJSParenthesizedExpression(expression);
			checkExpression(expression);
		}

		private void checkExpression(JSExpression expression)
		{
			if(isParentArithmetic(expression))
			{
				return;
			}
			if(!isArithmetic(expression))
			{
				return;
			}
			if(containsStringConcatenation(expression))
			{
				return;
			}
			final int numTerms = countTerms(expression);
			if(numTerms <= myState.m_limit)
			{
				return;
			}
			registerError(expression);
		}

		private int countTerms(JSExpression expression)
		{
			if(expression == null)
			{
				return 0;
			}
			if(!isArithmetic(expression))
			{
				return 1;
			}
			if(expression instanceof JSBinaryExpression)
			{
				final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;
				final JSExpression lhs = binaryExpression.getLOperand();
				final JSExpression rhs = binaryExpression.getROperand();
				return countTerms(lhs) + countTerms(rhs);
			}
			else if(expression instanceof JSPrefixExpression)
			{
				final JSPrefixExpression prefixExpression = (JSPrefixExpression) expression;
				final JSExpression operand = prefixExpression.getExpression();
				return countTerms(operand);
			}
			else if(expression instanceof JSParenthesizedExpression)
			{
				final JSParenthesizedExpression parenthesizedExpression = (JSParenthesizedExpression) expression;
				final JSExpression contents = parenthesizedExpression.getInnerExpression();
				return countTerms(contents);
			}
			return 1;
		}

		private boolean isParentArithmetic(JSExpression expression)
		{
			final PsiElement parent = expression.getParent();
			if(!(parent instanceof JSExpression))
			{
				return false;
			}
			return isArithmetic((JSExpression) parent);
		}

		private boolean isArithmetic(JSExpression expression)
		{
			if(expression instanceof JSBinaryExpression)
			{

				final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;
				final IElementType sign = binaryExpression.getOperationSign();
				return arithmeticTokens.contains(sign);
			}
			else if(expression instanceof JSPrefixExpression)
			{
				final JSPrefixExpression prefixExpression = (JSPrefixExpression) expression;
				final IElementType sign = prefixExpression.getOperationSign();
				return arithmeticTokens.contains(sign);
			}
			else if(expression instanceof JSParenthesizedExpression)
			{
				final JSParenthesizedExpression parenthesizedExpression = (JSParenthesizedExpression) expression;
				final JSExpression contents = parenthesizedExpression.getInnerExpression();
				return isArithmetic(contents);
			}
			return false;
		}

		private boolean containsStringConcatenation(JSExpression expression)
		{
			if(isStringLiteral(expression))
			{
				return true;
			}
			if(expression instanceof JSBinaryExpression)
			{

				final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;
				final JSExpression lhs = binaryExpression.getLOperand();

				if(containsStringConcatenation(lhs))
				{
					return true;
				}
				final JSExpression rhs = binaryExpression.getROperand();
				return containsStringConcatenation(rhs);
			}
			else if(expression instanceof JSPrefixExpression)
			{
				final JSPrefixExpression prefixExpression = (JSPrefixExpression) expression;
				final IElementType sign = prefixExpression.getOperationSign();
				return arithmeticTokens.contains(sign);
			}
			else if(expression instanceof JSParenthesizedExpression)
			{
				final JSParenthesizedExpression parenthesizedExpression = (JSParenthesizedExpression) expression;
				final JSExpression contents = parenthesizedExpression.getInnerExpression();
				return containsStringConcatenation(contents);
			}
			return false;
		}

		private boolean isStringLiteral(JSExpression expression)
		{
			if(expression instanceof JSLiteralExpression)
			{
				final JSLiteralExpression literal = (JSLiteralExpression) expression;
				final String text = literal.getText();
				return text.startsWith("'") || text.startsWith("\"");
			}
			if(expression instanceof JSReferenceExpression)
			{
				final JSReferenceExpression reference = (JSReferenceExpression) expression;
				final PsiElement referent = reference.resolve();
				if(referent instanceof JSVariable)
				{
					final JSVariable variable = (JSVariable) referent;
					if(variable.isConst())
					{
						final JSExpression initializer = variable.getInitializer();
						if(initializer != null)
						{
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
