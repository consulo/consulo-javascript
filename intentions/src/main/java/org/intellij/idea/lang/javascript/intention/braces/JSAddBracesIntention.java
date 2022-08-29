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
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiComment;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

@ExtensionImpl
@IntentionMetaData(ignoreId = "JSAddBracesIntention", categories = {
		"JavaScript",
		"Control Flow"
}, fileExtensions = "js")
public class JSAddBracesIntention extends JSMutablyNamedIntention
{

	@NonNls
	private static final String IF_KEYWORD = "if";
	@NonNls
	private static final String ELSE_KEYWORD = "else";

	@Override
	@Nonnull
	protected JSElementPredicate getElementPredicate()
	{
		return new AddBracesPredicate();
	}

	@Override
	protected String getTextForElement(PsiElement element)
	{
		final JSElement parent = (JSElement) element.getParent();
		final String keyword;

		assert (parent != null);

		if(parent instanceof JSIfStatement)
		{
			final JSIfStatement ifStatement = (JSIfStatement) parent;
			final JSStatement elseBranch = ifStatement.getElse();

			keyword = (element.equals(elseBranch) ? ELSE_KEYWORD : IF_KEYWORD);
		}
		else
		{
			final PsiElement firstChild = parent.getFirstChild();

			assert (firstChild != null);
			keyword = firstChild.getText();
		}

		return this.getText(keyword);
	}

	@Override
	protected void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		if(!(element instanceof JSStatement))
		{
			return;
		}
		final JSStatement statement = (JSStatement) element;
		final JSElement parent = (JSElement) element.getParent();
		final String text = element.getText();
		String newText;

		if(parent.getLastChild() instanceof PsiComment)
		{
			newText = '{' + text + "\n}";
		}
		else
		{
			newText = '{' + text + '}';
		}
		JSElementFactory.replaceStatement(statement, newText);
	}

	private static class AddBracesPredicate implements JSElementPredicate
	{
		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			if(!(element instanceof JSStatement))
			{
				return false;
			}
			if(element instanceof JSBlockStatement)
			{
				return false;
			}

			final PsiElement parentElement = element.getParent();
			if(!(parentElement instanceof JSElement))
			{
				return false;
			}
			final JSElement parent = (JSElement) parentElement;

			if(parent instanceof JSIfStatement)
			{
				final JSIfStatement ifStatement = (JSIfStatement) parent;

				return (!(element instanceof JSIfStatement &&
						element.equals(ifStatement.getElse())));
			}

			if(parent instanceof JSForStatement ||
					parent instanceof JSForInStatement
			)
			{
				return element.equals(((JSLoopStatement) parent).getBody());
			}

			return (parent instanceof JSWhileStatement ||
					parent instanceof JSDoWhileStatement);
		}
	}
}