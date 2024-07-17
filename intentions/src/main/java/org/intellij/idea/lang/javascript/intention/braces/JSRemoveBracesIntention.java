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
	ignoreId = "JSRemoveBracesIntention",
	categories = {"JavaScript", "Control Flow"},
	fileExtensions = "js"
)
public class JSRemoveBracesIntention extends JSMutablyNamedIntention
{
	@Override
	protected String getBasicText() {
		return JSIntentionBundle.message("braces.remove-braces.family-name");
	}

	@Override
	@Nonnull
	protected JSElementPredicate getElementPredicate()
	{
		return new RemoveBracesPredicate();
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

			keyword = element.equals(elseBranch) ? "else" : "if";
		}
		else
		{
			final PsiElement keywordChild = parent.getFirstChild();

			assert (keywordChild != null);
			keyword = keywordChild.getText();
		}

		return JSIntentionBundle.message("braces.remove-braces.display-name", keyword);
  }

	@Override
	@RequiredReadAction
	protected void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		final JSBlockStatement blockStatement = (JSBlockStatement) element;
		final JSStatement[] statements = blockStatement.getStatements();
		final JSStatement statement = statements[0];

		// handle comments
		final JSElement parent = (JSElement) blockStatement.getParent();

		assert (parent != null);

		final JSElement grandParent = (JSElement) parent.getParent();

		assert (grandParent != null);

		PsiElement sibling = statement.getFirstChild();

		assert (sibling != null);

		sibling = sibling.getNextSibling();
		while (sibling != null && !sibling.equals(statement))
		{
			if (sibling instanceof PsiComment)
			{
				grandParent.addBefore(sibling, parent);
			}
			sibling = sibling.getNextSibling();
		}

		final PsiElement lastChild = blockStatement.getLastChild();

		if (lastChild instanceof PsiComment)
		{
			final JSElement nextSibling = (JSElement) parent.getNextSibling();

			grandParent.addAfter(lastChild, nextSibling);
		}

		String text = statement.getText();
		JSElementFactory.replaceStatement(blockStatement, text);
	}

	public static class RemoveBracesPredicate implements JSElementPredicate
	{
		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			if (!(element instanceof JSBlockStatement blockStatement))
			{
				return false;
			}

			final PsiElement parent = blockStatement.getParent();

			return (parent instanceof JSIfStatement || parent instanceof JSWhileStatement || parent instanceof JSDoWhileStatement
				|| parent instanceof JSForStatement || parent instanceof JSForInStatement)
				&& isSingleNonVarStatement(blockStatement.getStatements());
			}

		private boolean isSingleNonVarStatement(JSStatement[] statements)
		{
			return statements.length == 1 && !(statements[0] instanceof JSVarStatement);
		}
	}
}