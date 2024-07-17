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
package org.intellij.idea.lang.javascript.intention.bool;

import com.intellij.lang.javascript.psi.JSBinaryExpression;
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
import org.intellij.idea.lang.javascript.psiutil.ComparisonUtils;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSFlipComparisonIntention",
	categories = {"JavaScript", "Boolean"},
	fileExtensions = "js"
)
public class JSFlipComparisonIntention extends JSMutablyNamedIntention
{
	@Override
	protected String getBasicText() {
		return JSIntentionBundle.message("bool.flip-comparison.family-name");
	}

	@Override
	@RequiredReadAction
	public String getTextForElement(PsiElement element)
	{
		final JSBinaryExpression exp = (JSBinaryExpression) element;
		String operatorText = null;
		String flippedOperatorText = null;

		if (exp != null)
		{
			operatorText = ComparisonUtils.getOperatorText(exp.getOperationSign());
			flippedOperatorText = ComparisonUtils.getFlippedOperatorText(exp.getOperationSign());
		}

		if (exp == null)
		{
			return JSIntentionBundle.message("bool.flip-comparison.display-name.unknown");
		}
		else if (operatorText.equals(flippedOperatorText))
		{
			return JSIntentionBundle.message("bool.flip-comparison.display-name.equals", operatorText);
		}
		else
		{
			return JSIntentionBundle.message("bool.flip-comparison.display-name.not-equals", operatorText, flippedOperatorText);
		}
	}

	@Override
	@Nonnull
	public JSElementPredicate getElementPredicate()
	{
		return new ComparisonPredicate();
	}

	@Override
	@RequiredReadAction
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		final JSBinaryExpression exp = (JSBinaryExpression) element;
		final JSExpression lhs = exp.getLOperand();
		final JSExpression rhs = exp.getROperand();
		final IElementType sign = exp.getOperationSign();

		assert (rhs != null);

		final String expString = rhs.getText() + ComparisonUtils.getFlippedOperatorText(sign) + lhs.getText();
		JSElementFactory.replaceExpression(exp, expString);
	}
}
