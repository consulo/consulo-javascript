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
package org.intellij.idea.lang.javascript.intention.number;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntentionBundle;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.BinaryOperatorUtils;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.ParenthesesUtils;

import jakarta.annotation.Nonnull;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSReplaceMultiplyWithShiftIntention",
	categories = {"JavaScript", "Shift"},
	fileExtensions = "js"
)
public class JSReplaceMultiplyWithShiftIntention extends JSMutablyNamedIntention
{
	@Override
	protected String getBasicText() {
		return JSIntentionBundle.message("number.replace-multiply-with-shift.family-name");
	}

	@Override
	@RequiredReadAction
	protected String getTextForElement(PsiElement element)
	{
		final IElementType tokenType = ((JSBinaryExpression) element).getOperationSign();
		final String operatorString;

		if (element instanceof JSAssignmentExpression)
		{
			operatorString = tokenType.equals(JSTokenTypes.MULTEQ) ? "<<=" : ">>=";
		}
		else
		{
			operatorString = tokenType.equals(JSTokenTypes.MULT) ? "<<" : ">>";
		}

		return JSIntentionBundle.message(
			"number.replace-multiply-with-shift.display-name",
			BinaryOperatorUtils.getOperatorText(tokenType),
			operatorString
		);
  }

	@Override
	@Nonnull
	public JSElementPredicate getElementPredicate()
	{
		return new MultiplyByPowerOfTwoPredicate();
	}

	@Override
	@RequiredReadAction
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		if (element instanceof JSAssignmentExpression assignmentExpression)
		{
			this.replaceMultiplyOrDivideAssignWithShiftAssign(assignmentExpression);
		}
		else
		{
			this.replaceMultiplyOrDivideWithShift((JSBinaryExpression) element);
		}
	}

	@RequiredReadAction
	private void replaceMultiplyOrDivideAssignWithShiftAssign(JSAssignmentExpression exp) throws IncorrectOperationException
	{
		final JSExpression lhs = exp.getLOperand();
		final JSExpression rhs = exp.getROperand();
		final IElementType tokenType = exp.getOperationSign();
		final String assignString = JSTokenTypes.MULTEQ.equals(tokenType) ? "<<=" : ">>=";
		final String expString = lhs.getText() + assignString + ShiftUtils.getLogBase2(rhs);

		JSElementFactory.replaceExpression(exp, expString);
	}

	@RequiredReadAction
	private void replaceMultiplyOrDivideWithShift(JSBinaryExpression exp) throws IncorrectOperationException
	{
		JSExpression lhs = exp.getLOperand();
		JSExpression rhs = exp.getROperand();
		final IElementType tokenType = exp.getOperationSign();
		final String operatorString = JSTokenTypes.MULT.equals(tokenType) ? "<<" : ">>";

		if (ShiftUtils.isPowerOfTwo(lhs) && JSTokenTypes.MULT.equals(tokenType))
		{
			JSExpression swap = lhs;

			lhs = rhs;
			rhs = swap;
		}

		final String lhsText = ParenthesesUtils.getParenthesized(lhs, ParenthesesUtils.SHIFT_PRECENDENCE);
		String expString = lhsText + operatorString + ShiftUtils.getLogBase2(rhs);
		final JSElement parent = (JSElement) exp.getParent();

		if (parent != null && parent instanceof JSExpression parentExpression && !(parent instanceof JSParenthesizedExpression)
			&& ParenthesesUtils.getPrecendence(parentExpression) < ParenthesesUtils.SHIFT_PRECENDENCE) {
			expString = '(' + expString + ')';
		}
		JSElementFactory.replaceExpression(exp, expString);
	}

	private static class MultiplyByPowerOfTwoPredicate implements JSElementPredicate
	{
		@Override
		@RequiredReadAction
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			return element instanceof JSAssignmentExpression assignmentExpression
				? isMultiplyByPowerOfTwo(assignmentExpression)
				: element instanceof JSBinaryExpression binaryExpression && isMultiplyByPowerOfTwo(binaryExpression);
		}

		@RequiredReadAction
		private static boolean isMultiplyByPowerOfTwo(JSAssignmentExpression expression)
		{
			final IElementType operator = expression.getOperationSign();

			if (operator == null || !(operator.equals(JSTokenTypes.MULTEQ) || operator.equals(JSTokenTypes.DIVEQ)))
			{
				return false;
			}

			final JSExpression rightExpression = expression.getROperand();

			return rightExpression != null && ShiftUtils.isPowerOfTwo(rightExpression);
		}

		@RequiredReadAction
		private static boolean isMultiplyByPowerOfTwo(JSBinaryExpression expression)
		{
			final IElementType operator = expression.getOperationSign();

			if (operator == null || !(operator.equals(JSTokenTypes.MULT) || operator.equals(JSTokenTypes.DIV)))
			{
				return false;
			}

			final JSExpression leftOperand = expression.getLOperand();
			final JSExpression rightOperand = expression.getROperand();

			return leftOperand != null && rightOperand != null
				&& (ShiftUtils.isPowerOfTwo(leftOperand) || ShiftUtils.isPowerOfTwo(rightOperand));
		}
	}
}
