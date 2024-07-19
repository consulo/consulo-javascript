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
package org.intellij.idea.lang.javascript.intention.string;

import com.intellij.lang.javascript.psi.JSLiteralExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSSingleToDoubleQuotedStringIntention",
	categories = {"JavaScript", "Other"},
	fileExtensions = "js"
)
public class JSSingleToDoubleQuotedStringIntention extends JSIntention
{
	@Override
	@Nonnull
	public String getText()
	{
		return JSIntentionLocalize.stringSingleToDoubleQuotedString().get();
	}

	@Override
	@Nonnull
	protected JSElementPredicate getElementPredicate()
	{
		return new SingleToDoubleQuotedStringPredicate();
	}

	@Override
	@RequiredReadAction
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		final JSLiteralExpression charLiteral = (JSLiteralExpression) element;

		JSElementFactory.replaceExpression(charLiteral, changeQuotes(charLiteral.getText()));
	}

	static String changeQuotes(String charLiteral)
	{
		StringBuilder buffer = new StringBuilder(charLiteral);
		int simpleIndex = charLiteral.lastIndexOf(StringUtil.SIMPLE_QUOTE, charLiteral.length() - 2);
		int doubleIndex = charLiteral.lastIndexOf(StringUtil.DOUBLE_QUOTE);

		while (simpleIndex > 0 || doubleIndex >= 0)
		{
			if (simpleIndex > doubleIndex)
			{
				if (charLiteral.charAt(simpleIndex - 1) == StringUtil.BACKSLASH)
				{
					buffer.deleteCharAt(simpleIndex - 1);
				}
				simpleIndex = charLiteral.lastIndexOf(StringUtil.SIMPLE_QUOTE, simpleIndex - 2);
			}
			else
			{
				if (charLiteral.charAt(doubleIndex - 1) != StringUtil.BACKSLASH)
				{
					buffer.insert(doubleIndex, StringUtil.BACKSLASH);
				}
				doubleIndex = charLiteral.lastIndexOf(StringUtil.DOUBLE_QUOTE, doubleIndex - 1);
			}
		}
		buffer.setCharAt(0, StringUtil.DOUBLE_QUOTE);
		buffer.setCharAt(buffer.length() - 1, StringUtil.DOUBLE_QUOTE);

		return buffer.toString();
	}

	private static class SingleToDoubleQuotedStringPredicate implements JSElementPredicate
	{
		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			return element instanceof JSLiteralExpression expression && StringUtil.isSimpleQuoteStringLiteral(expression);
		}
	}
}
