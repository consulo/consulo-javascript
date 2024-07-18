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
package org.intellij.idea.lang.javascript.intention.braces;

import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiComment;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntentionBundle;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSAddBracesIntention",
	categories = {"JavaScript", "Control Flow"},
	fileExtensions = "js"
)
public class JSAddBracesIntention extends JSMutablyNamedIntention
{
	@Override
	@Nonnull
	protected JSElementPredicate getElementPredicate()
	{
		return new AddBracesPredicate();
	}

	@Nonnull
  @Override
	protected String getBasicText()
	{
		return JSIntentionBundle.message("braces.add-braces.family-name");
	}

	@Override
	@RequiredReadAction
	protected String getTextForElement(PsiElement element)
	{
		final JSElement parent = (JSElement) element.getParent();
		final String keyword;

		assert (parent != null);

		if (parent instanceof JSIfStatement ifStatement)
		{
			final JSStatement elseBranch = ifStatement.getElse();

			keyword = (element.equals(elseBranch) ? "else" : "if");
		}
		else
		{
			final PsiElement firstChild = parent.getFirstChild();

			assert (firstChild != null);
			keyword = firstChild.getText();
		}

		return JSIntentionBundle.message("braces.add-braces.display-name", keyword);
  }

	@Override
	@RequiredReadAction
	protected void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		if (!(element instanceof JSStatement statement))
		{
			return;
		}
		final JSElement parent = (JSElement) element.getParent();
		final String text = element.getText();
		String newText = parent.getLastChild() instanceof PsiComment
			? '{' + text + "\n}"
			: '{' + text + '}';
		JSElementFactory.replaceStatement(statement, newText);
	}

	private static class AddBracesPredicate implements JSElementPredicate
	{
		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
			if (!(element instanceof JSStatement)
				|| element instanceof JSBlockStatement
				|| !(element.getParent() instanceof JSElement parent)) {
				return false;
			}

			if (parent instanceof JSIfStatement ifStatement)
			{
				return !(element instanceof JSIfStatement && element.equals(ifStatement.getElse()));
			}

			if (parent instanceof JSForStatement || parent instanceof JSForInStatement)
			{
				return element.equals(((JSLoopStatement) parent).getBody());
			}

			return parent instanceof JSWhileStatement || parent instanceof JSDoWhileStatement;
		}
	}
}