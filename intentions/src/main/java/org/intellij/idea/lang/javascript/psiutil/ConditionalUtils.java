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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import org.jetbrains.annotations.NonNls;

public class ConditionalUtils
{
	private ConditionalUtils()
	{
	}

	public static JSStatement stripBraces(JSStatement branch)
	{
		if(branch != null && branch instanceof JSBlockStatement)
		{
			final JSBlockStatement block = (JSBlockStatement) branch;
			final JSStatement[] statements = block.getStatements();

			if(statements.length == 1)
			{
				return statements[0];
			}
			else
			{
				return block;
			}
		}

		return branch;
	}

	public static boolean isReturn(JSStatement statement, String value)
	{
		if(statement == null)
		{
			return false;
		}
		if(!(statement instanceof JSReturnStatement))
		{
			return false;
		}

		final JSExpression returnExpression = ((JSReturnStatement) statement).getExpression();

		return (returnExpression != null && value.equals(returnExpression.getText()));
	}

	public static boolean isAssignment(JSStatement statement, String value)
	{
		if(statement == null || !(statement instanceof JSExpressionStatement))
		{
			return false;
		}

		final JSExpression expression = ((JSExpressionStatement) statement).getExpression();

		if(!(expression instanceof JSAssignmentExpression))
		{
			return false;
		}

		final JSExpression rhs = ((JSAssignmentExpression) expression).getROperand();

		return (rhs != null && rhs.getText().equals(value));
	}

	public static boolean isAssignment(JSStatement statement)
	{
		if(!(statement instanceof JSExpressionStatement))
		{
			return false;
		}

		final JSExpressionStatement expressionStatement = (JSExpressionStatement) statement;

		return (expressionStatement.getExpression() instanceof JSAssignmentExpression);
	}

	public static boolean isConditional(JSExpression expression)
	{
		expression = ParenthesesUtils.stripParentheses(expression);

		if(expression == null)
		{
			return false;
		}

		if(expression instanceof JSConditionalExpression)
		{
			return true;
		}

		if(expression instanceof JSBinaryExpression)
		{
			final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;
			final IElementType sign = binaryExpression.getOperationSign();

			if(JSTokenTypes.XOR.equals(sign))
			{
				return (isConditional(binaryExpression.getLOperand()) &&
						isConditional(binaryExpression.getROperand()));
			}

			return (JSTokenTypes.ANDAND.equals(sign) || JSTokenTypes.OROR.equals(sign) ||
					JSTokenTypes.EQ.equals(sign) || JSTokenTypes.NE.equals(sign) ||
					JSTokenTypes.LT.equals(sign) || JSTokenTypes.LE.equals(sign) ||
					JSTokenTypes.GT.equals(sign) || JSTokenTypes.GE.equals(sign));
		}

		return BoolUtils.isBooleanLiteral(expression);
	}

	public static boolean isSimplifiableImplicitReturn(JSIfStatement ifStatement, boolean negated)
	{
		final JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());
		final PsiElement nextStatement = JSElementFactory.getNonWhiteSpaceSibling(ifStatement, true);

		if(!(nextStatement instanceof JSStatement))
		{
			return false;
		}

		final JSStatement elseBranch = (JSStatement) nextStatement;

		if(negated)
		{
			return (ConditionalUtils.isReturn(thenBranch, BoolUtils.FALSE) &&
					ConditionalUtils.isReturn(elseBranch, BoolUtils.TRUE));
		}
		else
		{
			return (ConditionalUtils.isReturn(thenBranch, BoolUtils.TRUE) &&
					ConditionalUtils.isReturn(elseBranch, BoolUtils.FALSE));
		}
	}

	public static boolean isSimplifiableReturn(JSIfStatement ifStatement, boolean negated)
	{
		final JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());
		final JSStatement elseBranch = ConditionalUtils.stripBraces(ifStatement.getElse());

		if(negated)
		{
			return (ConditionalUtils.isReturn(thenBranch, BoolUtils.FALSE) &&
					ConditionalUtils.isReturn(elseBranch, BoolUtils.TRUE));
		}
		else
		{
			return (ConditionalUtils.isReturn(thenBranch, BoolUtils.TRUE) &&
					ConditionalUtils.isReturn(elseBranch, BoolUtils.FALSE));
		}
	}

	public static boolean isSimplifiableAssignment(JSIfStatement ifStatement, boolean negated)
	{
		final JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());
		final JSStatement elseBranch = ConditionalUtils.stripBraces(ifStatement.getElse());

		final boolean isAssignment;

		if(negated)
		{
			isAssignment = (ConditionalUtils.isAssignment(thenBranch, BoolUtils.FALSE) &&
					ConditionalUtils.isAssignment(elseBranch, BoolUtils.TRUE));
		}
		else
		{
			isAssignment = (ConditionalUtils.isAssignment(thenBranch, BoolUtils.TRUE) &&
					ConditionalUtils.isAssignment(elseBranch, BoolUtils.FALSE));
		}

		if(isAssignment)
		{
			final JSAssignmentExpression thenExpression = (JSAssignmentExpression) ((JSExpressionStatement) thenBranch).getExpression();
			final JSAssignmentExpression elseExpression = (JSAssignmentExpression) ((JSExpressionStatement) elseBranch).getExpression();
			final IElementType thenSign = thenExpression.getOperationSign();
			final IElementType elseSign = elseExpression.getOperationSign();

			if(!thenSign.equals(elseSign))
			{
				return false;
			}

			final JSExpression thenLhs = thenExpression.getLOperand();
			final JSExpression elseLhs = elseExpression.getLOperand();

			return EquivalenceChecker.expressionsAreEquivalent(thenLhs, elseLhs);
		}
		else
		{
			return false;
		}
	}

	public static boolean isSimplifiableImplicitAssignment(JSIfStatement ifStatement, boolean negated)
	{
		if(ifStatement.getElse() != null)
		{
			return false;
		}
		final JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());
		final PsiElement nextStatement = JSElementFactory.getNonWhiteSpaceSibling(ifStatement, false);

		if(!(nextStatement instanceof JSStatement))
		{
			return false;
		}

		final JSStatement elseBranch = ConditionalUtils.stripBraces((JSStatement) nextStatement);

		final boolean isAssignment;

		if(negated)
		{
			isAssignment = (ConditionalUtils.isAssignment(thenBranch, BoolUtils.FALSE) &&
					ConditionalUtils.isAssignment(elseBranch, BoolUtils.TRUE));
		}
		else
		{
			isAssignment = (ConditionalUtils.isAssignment(thenBranch, BoolUtils.TRUE) &&
					ConditionalUtils.isAssignment(elseBranch, BoolUtils.FALSE));
		}

		if(isAssignment)
		{
			final JSAssignmentExpression thenExpression = (JSAssignmentExpression) ((JSExpressionStatement) thenBranch).getExpression();
			final JSAssignmentExpression elseExpression = (JSAssignmentExpression) ((JSExpressionStatement) elseBranch).getExpression();
			final IElementType thenSign = thenExpression.getOperationSign();
			final IElementType elseSign = elseExpression.getOperationSign();

			if(!thenSign.equals(elseSign))
			{
				return false;
			}

			final JSExpression thenLhs = thenExpression.getLOperand();
			final JSExpression elseLhs = elseExpression.getLOperand();

			return EquivalenceChecker.expressionsAreEquivalent(thenLhs, elseLhs);
		}
		else
		{
			return false;
		}
	}

	public static void replaceSimplifiableImplicitReturn(JSIfStatement statement, boolean negated)
			throws IncorrectOperationException
	{
		final JSExpression condition = statement.getCondition();
		final String conditionText = (negated ? BoolUtils.getNegatedExpressionText(condition) : condition.getText());
		final JSElement nextStatement = (JSElement) JSElementFactory.getNonWhiteSpaceSibling(statement, true);
		@NonNls final String newStatement = "return " + conditionText + ';';

		JSElementFactory.replaceStatement(statement, newStatement);
		assert (nextStatement != null);
		JSElementFactory.removeElement(nextStatement);
	}

	public static void replaceSimplifiableReturn(JSIfStatement statement, boolean negated)
			throws IncorrectOperationException
	{
		final JSExpression condition = statement.getCondition();
		final String conditionText = (negated ? BoolUtils.getNegatedExpressionText(condition) : condition.getText());
		@NonNls final String newStatement = "return " + conditionText + ';';

		JSElementFactory.replaceStatement(statement, newStatement);
	}

	public static void replaceSimplifiableAssignment(JSIfStatement statement, boolean negated)
			throws IncorrectOperationException
	{
		final JSExpression condition = statement.getCondition();
		final String conditionText = (negated ? BoolUtils.getNegatedExpressionText(condition) : condition.getText());
		final JSExpressionStatement assignmentStatement = (JSExpressionStatement) ConditionalUtils.stripBraces(statement.getThen());
		final JSAssignmentExpression assignmentExpression = (JSAssignmentExpression) assignmentStatement.getExpression();
		final IElementType operator = assignmentExpression.getOperationSign();
		final String operand = BinaryOperatorUtils.getOperatorText(operator);
		final JSExpression lhs = assignmentExpression.getLOperand();
		final String lhsText = lhs.getText();

		JSElementFactory.replaceStatement(statement, lhsText + operand + conditionText + ';');
	}

	public static void replaceSimplifiableImplicitAssignment(JSIfStatement statement, boolean negated)
			throws IncorrectOperationException
	{
		final JSElement prevStatement = (JSElement) JSElementFactory.getNonWhiteSpaceSibling(statement, false);
		final JSExpression condition = statement.getCondition();
		final String conditionText = (negated ? BoolUtils.getNegatedExpressionText(condition) : condition.getText());
		final JSExpressionStatement assignmentStatement = (JSExpressionStatement) ConditionalUtils.stripBraces(statement.getThen());
		final JSAssignmentExpression assignmentExpression = (JSAssignmentExpression) assignmentStatement.getExpression();
		final IElementType operator = assignmentExpression.getOperationSign();
		final String operand = BinaryOperatorUtils.getOperatorText(operator);
		final JSExpression lhs = assignmentExpression.getLOperand();
		final String lhsText = lhs.getText();

		JSElementFactory.replaceStatement(statement, lhsText + operand + conditionText + ';');

		assert (prevStatement != null);

		JSElementFactory.removeElement(prevStatement);
	}

	public static void replaceAssignmentOrReturnIfSimplifiable(JSIfStatement statement)
			throws IncorrectOperationException
	{
		if(isSimplifiableAssignment(statement, false))
		{
			replaceSimplifiableAssignment(statement, false);
		}
		else if(isSimplifiableAssignment(statement, true))
		{
			replaceSimplifiableAssignment(statement, true);
		}
		else if(isSimplifiableReturn(statement, false))
		{
			replaceSimplifiableReturn(statement, false);
		}
		else if(isSimplifiableReturn(statement, true))
		{
			replaceSimplifiableReturn(statement, true);
		}
		else if(isSimplifiableImplicitReturn(statement, false))
		{
			replaceSimplifiableImplicitReturn(statement, false);
		}
		else if(isSimplifiableImplicitReturn(statement, true))
		{
			replaceSimplifiableImplicitReturn(statement, true);
		}
		else if(isSimplifiableImplicitAssignment(statement, false))
		{
			replaceSimplifiableImplicitAssignment(statement, false);
		}
		else if(isSimplifiableImplicitAssignment(statement, true))
		{
			replaceSimplifiableImplicitAssignment(statement, true);
		}
	}

	public static void replaceConditionalWithIf(JSConditionalExpression conditional)
			throws IncorrectOperationException
	{
		final JSStatement statement = PsiTreeUtil.getParentOfType(conditional, JSStatement.class);

		assert (statement != null);

		final String statementText = statement.getText();
		final String conditionalText = ParenthesesUtils.unstripParentheses(conditional).getText();
		final int conditionalIndex = statementText.indexOf(conditionalText);
		final String statementStart = statementText.substring(0, conditionalIndex);
		final String statementEnd = statementText.substring(conditionalIndex + conditionalText.length());
		final JSExpression condition = ParenthesesUtils.stripParentheses(conditional.getCondition());
		final JSExpression thenExpression = ParenthesesUtils.stripParentheses(conditional.getThen());
		final JSExpression elseExpression = ParenthesesUtils.stripParentheses(conditional.getElse());

		@NonNls final String ifStatementText = "if (" + condition.getText() + ") {" +
				statementStart + thenExpression.getText() + statementEnd +
				"} else {" +
				statementStart + elseExpression.getText() + statementEnd +
				'}';
		JSElementFactory.replaceStatement(statement, ifStatementText);
	}
}
