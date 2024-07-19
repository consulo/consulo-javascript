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
package org.intellij.idea.lang.javascript.intention.opassign;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntentionBundle;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.*;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSReplaceWithOperatorAssignmentIntention",
	categories = {"JavaScript", "Other"},
	fileExtensions = "js"
)
public class JSReplaceWithOperatorAssignmentIntention extends JSMutablyNamedIntention
{
	@Nonnull
	@Override
	protected String getBasicText()
	{
		return JSIntentionBundle.message("opassign.replace.with.operator.assignment");
	}

	@Override
	@RequiredReadAction
	public String getTextForElement(PsiElement element)
	{
		final JSAssignmentExpression exp = (JSAssignmentExpression) element;
		final JSBinaryExpression rhs = (JSBinaryExpression) exp.getROperand();
		assert (rhs != null);
		final IElementType sign = rhs.getOperationSign();

		return JSIntentionBundle.message("opassign.replace.with.operator.assignment.message", BinaryOperatorUtils.getOperatorText(sign));
  }

	@Override
	@Nonnull
	public JSElementPredicate getElementPredicate()
	{
		return new Predicate();
	}

	@Override
	@RequiredReadAction
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		final JSAssignmentExpression exp = (JSAssignmentExpression) element;
		final JSBinaryExpression rhs = (JSBinaryExpression) exp.getROperand();
		final JSExpression lhs = exp.getLOperand();

		assert (rhs != null);

		final IElementType sign = rhs.getOperationSign();
		final String operand = BinaryOperatorUtils.getOperatorText(sign);
		final JSExpression rhsrhs = rhs.getROperand();

		assert (rhsrhs != null);

		JSElementFactory.replaceExpression(exp, lhs.getText() + operand + '=' + rhsrhs.getText());
	}

	private static class Predicate implements JSElementPredicate
	{
		@Override
		@RequiredReadAction
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			if (!(element instanceof JSAssignmentExpression) || ErrorUtil.containsError(element)) {
        return false;
			}
			final JSAssignmentExpression assignment = (JSAssignmentExpression) element;
			final IElementType tokenType = assignment.getOperationSign();
			if (!JSTokenTypes.EQ.equals(tokenType))
			{
				return false;
			}
			JSExpression lhs = assignment.getLOperand();
			final JSExpression rhs = assignment.getROperand();

			if (lhs instanceof JSDefinitionExpression definitionExpression)
			{
				lhs = definitionExpression.getExpression();
			}
			if (lhs == null || !(rhs instanceof JSBinaryExpression)) {
        return false;
			}
			final JSBinaryExpression binaryRhs = (JSBinaryExpression) rhs;
			final JSExpression rhsRhs = binaryRhs.getROperand();
			final JSExpression rhsLhs = binaryRhs.getLOperand();

			if (rhsRhs == null)
			{
				return false;
			}

			final IElementType rhsTokenType = binaryRhs.getOperationSign();

			return !JSTokenTypes.OROR.equals(rhsTokenType)
				&& !JSTokenTypes.ANDAND.equals(rhsTokenType)
				&& !SideEffectChecker.mayHaveSideEffects(lhs)
				&& EquivalenceChecker.expressionsAreEquivalent(lhs, rhsLhs);
		}
	}
}
