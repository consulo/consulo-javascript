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
import com.intellij.lang.javascript.psi.JSElement;
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
import org.intellij.idea.lang.javascript.psiutil.BinaryOperatorUtils;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSFlipConjunctionIntention",
	categories = {"JavaScript", "Boolean"},
	fileExtensions = "js"
)
public class JSFlipConjunctionIntention extends JSMutablyNamedIntention
{
	@Nonnull
	@Override
	protected String getBasicText()
	{
		return JSIntentionBundle.message("bool.flip-conjunction.family-name");
	}

	@Override
	@RequiredReadAction
	protected String getTextForElement(PsiElement element)
	{
		final JSBinaryExpression binaryExpression = (JSBinaryExpression) element;
		final IElementType sign = binaryExpression.getOperationSign();

		return JSIntentionBundle.message("bool.flip-conjunction.display-name", BinaryOperatorUtils.getOperatorText(sign));
  }

	@Override
	@Nonnull
	public JSElementPredicate getElementPredicate()
	{
		return new ConjunctionPredicate();
	}

	@Override
	@RequiredReadAction
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		final JSBinaryExpression binaryExpression = (JSBinaryExpression) element;
		JSExpression exp = binaryExpression;

		final IElementType sign = binaryExpression.getOperationSign();
		JSElement parent = (JSElement) exp.getParent();

		while (isConjunctionExpression(parent, sign))
		{
			exp = (JSExpression) parent;
			assert (exp != null);
			parent = (JSElement) exp.getParent();
		}
		JSElementFactory.replaceExpression(exp, this.flipExpression(exp, sign));
	}

	@RequiredReadAction
	private String flipExpression(JSExpression exp, IElementType conjunctionType)
	{
		if (isConjunctionExpression(exp, conjunctionType))
		{
			final JSBinaryExpression andExpression = (JSBinaryExpression) exp;

			return this.flipExpression(andExpression.getROperand(), conjunctionType) + ' ' +
				BinaryOperatorUtils.getOperatorText(conjunctionType) + ' ' +
				this.flipExpression(andExpression.getLOperand(), conjunctionType);
		}
		else
		{
			return exp.getText();
		}
	}

	@RequiredReadAction
	private static boolean isConjunctionExpression(JSElement expression, IElementType conjunctionType)
	{
		return expression instanceof JSBinaryExpression binaryExpression && binaryExpression.getOperationSign().equals(conjunctionType);
	}
}
