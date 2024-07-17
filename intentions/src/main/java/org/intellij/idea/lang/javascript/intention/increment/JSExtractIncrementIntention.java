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
package org.intellij.idea.lang.javascript.intention.increment;

import com.intellij.lang.javascript.psi.*;
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
	ignoreId = "JSExtractIncrementIntention",
	categories = {"JavaScript", "Other"},
	fileExtensions = "js"
)
public class JSExtractIncrementIntention extends JSMutablyNamedIntention
{
	@Override
	protected String getBasicText() {
		return JSIntentionBundle.message("increment.extract-increment.family-name");
	}

	@Override
	public String getTextForElement(PsiElement element)
	{
		return JSIntentionBundle.message("increment.extract-increment.display-name", BinaryOperatorUtils.getOperatorText(getOperationSign(element)));
  }

	@Override
	@Nonnull
	public JSElementPredicate getElementPredicate()
	{
		return new ExtractIncrementPredicate();
	}

	@Override
	@RequiredReadAction
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		final boolean isPostfix = (element instanceof JSPostfixExpression);
		final JSExpression operand = isPostfix
			? ((JSPostfixExpression) element).getExpression()
			: ((JSPrefixExpression) element).getExpression();
		final JSStatement statement = TreeUtil.getParentOfType(element, JSStatement.class);

		assert (statement != null);

		if (isPostfix)
		{
			JSElementFactory.addStatementAfter(statement, element.getText() + ';');
		}
		else
		{
			JSElementFactory.addStatementBefore(statement, element.getText() + ';');
		}
		JSElementFactory.replaceExpression((JSExpression) element, operand.getText());
	}

	@RequiredReadAction
	private static IElementType getOperationSign(PsiElement element)
	{
		return element instanceof JSPostfixExpression postfixExpression
				? postfixExpression.getOperationSign()
				: ((JSPrefixExpression) element).getOperationSign();
	}

	private static class ExtractIncrementPredicate implements JSElementPredicate
	{
		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			if (!ExpressionUtil.isIncrementDecrementExpression(element)
				|| ErrorUtil.containsError(element)
				|| element.getParent() instanceof JSExpressionStatement) {
        return false;
			}

			final JSStatement containingStatement = TreeUtil.getParentOfType(element, JSStatement.class);

			if (element instanceof JSPostfixExpression
				&& (containingStatement instanceof JSReturnStatement || containingStatement instanceof JSThrowStatement))
			{
				return false;
			}
			return (containingStatement != null);
		}
	}
}
