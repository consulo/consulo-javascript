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
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.ast.IElementType;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiComment;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.TreeUtil;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSChangeToEndOfLineCommentIntention",
    categories = {"JavaScript", "Comments"},
    fileExtensions = "js"
)
public class JSChangeToEndOfLineCommentIntention extends JSIntention {
    @Override
    @Nonnull
    public LocalizeValue getText() {
        return JSIntentionLocalize.commentChangeToEndOfLineComment();
    }

    @Override
    @Nonnull
    protected JSElementPredicate getElementPredicate() {
        return new CStyleCommentPredicate();
    }

    @Override
    @RequiredReadAction
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        PsiElement parent = element.getParent();

        assert (parent != null);

        String commentText = element.getText();
        PsiElement whitespace = element.getNextSibling();
        String text = commentText.substring(2, commentText.length() - 2);
        String[] lines = text.split("\n");
        String[] newComments = buildCommentStrings(lines);
        PsiElement currentElement = element;

        for (int index = newComments.length; --index >= 0; ) {
            if (newComments[index].length() > 0) {
                currentElement = JSElementFactory.addElementBefore(currentElement, "\n");
                currentElement = JSElementFactory.addElementBefore(currentElement, newComments[index]);
            }
        }
        if (whitespace != null) {
            JSElementFactory.removeElement(whitespace);
        }
        JSElementFactory.removeElement(element);
    }

    private static String[] buildCommentStrings(String[] lines) {
        int lastNonEmtpyLine = -1;

        for (int i = lines.length - 1; i >= 0 && lastNonEmtpyLine == -1; i--) {
            String line = lines[i].trim();
            if (!line.isEmpty()) {
                lastNonEmtpyLine = i;
            }
        }
        if (lastNonEmtpyLine == -1) {
            return new String[]{"//"};
        }

        StringBuilder buffer = new StringBuilder();
        String[] commentStrings = new String[lastNonEmtpyLine + 1];

        for (int i = 0; i <= lastNonEmtpyLine; i++) {
            String line = lines[i];

            if (line.trim().length() != 0) {
                buffer.replace(0, buffer.length(), "//");

                if (line.startsWith(" *")) {
                    buffer.append(line.substring(2));
                }
                else if (line.startsWith("*")) {
                    buffer.append(line.substring(1));
                }
                else {
                    buffer.append(line);
                }
            }

            commentStrings[i] = buffer.toString();
        }
        return commentStrings;
    }

    private static class CStyleCommentPredicate implements JSElementPredicate {
        @Override
        @RequiredReadAction
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof PsiComment)) {
                return false;
            }

            PsiComment comment = (PsiComment)element;
            IElementType type = comment.getTokenType();

            if (!(JSTokenTypes.C_STYLE_COMMENT.equals(type) || JSTokenTypes.C_STYLE_COMMENT.equals(type))) {
                return false;
            }
            PsiElement sibling = TreeUtil.getNextLeaf(comment);
            if (!(sibling instanceof PsiWhiteSpace)) {
                return false;
            }
            String whitespaceText = sibling.getText();
            return whitespaceText.indexOf((int)'\n') >= 0 || whitespaceText.indexOf((int)'\r') >= 0;
        }
    }
}