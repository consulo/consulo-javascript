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

import javax.annotation.Nonnull;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.TreeUtil;

public class JSMoveCommentToSeparateLineIntention extends JSIntention {
    @Override
	@Nonnull
    protected JSElementPredicate getElementPredicate() {
        return new CommentOnLineWithSourcePredicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        final PsiComment    selectedComment = (PsiComment) element;
        PsiElement          elementToCheck  = selectedComment;
        final PsiWhiteSpace whiteSpace;

        while (true) {
            elementToCheck = TreeUtil.getPrevLeaf(elementToCheck);
            if (elementToCheck == null) {
                return ;
            }
            if (isLineBreakWhiteSpace(elementToCheck)) {
                whiteSpace = (PsiWhiteSpace) elementToCheck;
                break;
            }
        }

        PsiElement  commentElement = JSElementFactory.addElementBefore(whiteSpace, selectedComment.getText());
        JSElementFactory.addElementBefore(commentElement, "\n");

        JSElementFactory.removeElement(selectedComment);
    }

    private static boolean isLineBreakWhiteSpace(PsiElement element) {
        return (element instanceof PsiWhiteSpace &&
                containsLineBreak(element.getText()));
    }

    private static boolean containsLineBreak(String text) {
        return (text.indexOf((int) '\n') >= 0 ||
                text.indexOf((int) '\r') >= 0);
    }

    private static class CommentOnLineWithSourcePredicate implements JSElementPredicate {

        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof PsiComment)) {
                return false;
            }
            final PsiComment   comment = (PsiComment) element;
            final IElementType type    = comment.getTokenType();

            if (!(JSTokenTypes.C_STYLE_COMMENT    .equals(type) ||
                  JSTokenTypes.END_OF_LINE_COMMENT.equals(type))) {
                return false; // can't move JSP comments
            }

            final PsiElement prevSibling = TreeUtil.getPrevLeaf(element);
            if (!(prevSibling instanceof PsiWhiteSpace)) {
                return true;
            }
            final String prevSiblingText = prevSibling.getText();
            if (prevSiblingText.indexOf('\n') < 0 &&
                prevSiblingText.indexOf('\r') < 0) {
                return true;
            }
            final PsiElement nextSibling = TreeUtil.getNextLeaf(element);
            if (!(nextSibling instanceof PsiWhiteSpace)) {
                return true;
            }
            final String nextSiblingText = nextSibling.getText();
            return (nextSiblingText.indexOf('\n') < 0 &&
                    nextSiblingText.indexOf('\r') < 0);
        }
    }
}
