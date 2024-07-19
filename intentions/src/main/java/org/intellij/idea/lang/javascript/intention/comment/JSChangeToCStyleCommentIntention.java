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
package org.intellij.idea.lang.javascript.intention.comment;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiComment;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.util.lang.StringUtil;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.intention.JSIntentionBundle;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

import jakarta.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSChangeToCStyleCommentIntention",
	categories = {"JavaScript", "Comments"},
	fileExtensions = "js"
)
public class JSChangeToCStyleCommentIntention extends JSIntention
{
	@Override
	@Nonnull
	public String getText()
	{
		return JSIntentionBundle.message("comment.change.to.cstyle.comment");
	}

	@Override
	@Nonnull
	protected JSElementPredicate getElementPredicate()
	{
		return new EndOfLineCommentPredicate();
	}

	@Override
	@RequiredReadAction
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		PsiComment firstComment = (PsiComment) element;

		while (true)
		{
			final PsiElement prevComment = JSElementFactory.getNonWhiteSpaceSibling(firstComment, false);

			if (!isEndOfLineComment(prevComment))
			{
				break;
			}
			assert (prevComment != null);
			firstComment = (PsiComment) prevComment;
		}

		final StringBuilder buffer = new StringBuilder(getCommentContents(firstComment));
		final List<PsiElement> elementsToDelete = new ArrayList<>();
		PsiElement nextComment = firstComment;

		while (true)
		{
			elementsToDelete.add(nextComment);

			nextComment = JSElementFactory.getNonWhiteSpaceSibling(nextComment, true);
			if (!isEndOfLineComment(nextComment))
			{
				break;
			}
			assert (nextComment != null);

			final PsiElement prevSibling = nextComment.getPrevSibling();

			assert (prevSibling != null);
			elementsToDelete.add(prevSibling);

			buffer.append(prevSibling.getText())  // White space
				.append(getCommentContents((PsiComment) nextComment));
		}

		final String text = StringUtil.replace(buffer.toString(), "*/", "* /");
		final String newCommentString = text.indexOf('\n') >= 0
			? "/*\n" + text + "\n*/"
			: "/*" + text + "*/";

		JSElementFactory.addElementBefore(firstComment, newCommentString);
		for (final PsiElement elementToDelete : elementsToDelete)
		{
			JSElementFactory.removeElement(elementToDelete);
		}
	}

	private static boolean isEndOfLineComment(PsiElement element)
	{
		return element instanceof PsiComment comment && JSTokenTypes.END_OF_LINE_COMMENT.equals(comment.getTokenType());
	}

	@RequiredReadAction
	private static String getCommentContents(PsiComment comment)
	{
		return comment.getText().substring(2);
	}

	private static class EndOfLineCommentPredicate implements JSElementPredicate
	{

		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			return element instanceof PsiComment comment && JSTokenTypes.END_OF_LINE_COMMENT.equals(comment.getTokenType());
		}
	}
}