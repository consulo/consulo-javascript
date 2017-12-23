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
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class JSChangeToCStyleCommentIntention extends JSIntention {

    @Override
	@NotNull
    protected JSElementPredicate getElementPredicate() {
        return new EndOfLineCommentPredicate();
    }

    @Override
	public void processIntention(@NotNull PsiElement element) throws IncorrectOperationException {
        PsiComment firstComment = (PsiComment) element;

        while (true) {
            final PsiElement prevComment = JSElementFactory.getNonWhiteSpaceSibling(firstComment, false);

            if (!isEndOfLineComment(prevComment)) {
                break;
            }
            assert (prevComment != null);
            firstComment = (PsiComment) prevComment;
        }

        final StringBuilder     buffer           = new StringBuilder(getCommentContents(firstComment));
        final List<PsiElement>  elementsToDelete = new ArrayList<PsiElement>();
        PsiElement              nextComment      = firstComment;

        while (true) {
            elementsToDelete.add(nextComment);

            nextComment = JSElementFactory.getNonWhiteSpaceSibling(nextComment, true);
            if (!isEndOfLineComment(nextComment)) {
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
        final String newCommentString;

        if (text.indexOf('\n') >= 0) {
            newCommentString = "/*\n" + text + "\n*/";
        } else {
            newCommentString = "/*" + text + "*/";
        }

        JSElementFactory.addElementBefore(firstComment, newCommentString);
        for (final PsiElement elementToDelete : elementsToDelete) {
            JSElementFactory.removeElement(elementToDelete);
        }
    }

    private static boolean isEndOfLineComment(PsiElement element) {
        if (!(element instanceof PsiComment)) {
            return false;
        }

        final PsiComment   comment   = (PsiComment) element;
        final IElementType tokenType = comment.getTokenType();

        return JSTokenTypes.END_OF_LINE_COMMENT.equals(tokenType);
    }

    private static String getCommentContents(PsiComment comment) {
        return comment.getText().substring(2);
    }

    private static class EndOfLineCommentPredicate implements JSElementPredicate {

        @Override
		public boolean satisfiedBy(@NotNull PsiElement element) {
            if (!(element instanceof PsiComment)) {
                return false;
            }

            final IElementType type = ((PsiComment) element).getTokenType();

            return JSTokenTypes.END_OF_LINE_COMMENT.equals(type);
        }
    }
}