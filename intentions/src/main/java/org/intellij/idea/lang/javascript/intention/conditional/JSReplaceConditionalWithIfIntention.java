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
package org.intellij.idea.lang.javascript.intention.conditional;

import com.intellij.lang.javascript.psi.JSConditionalExpression;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ConditionalUtils;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSReplaceConditionalWithIfIntention",
    categories = {"JavaScript", "Conditional"},
    fileExtensions = "js"
)
public class JSReplaceConditionalWithIfIntention extends JSIntention {
    @Override
    @Nonnull
    public String getText() {
        return JSIntentionLocalize.conditionalReplaceConditionalWithIf().get();
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new ReplaceConditionalWithIfPredicate();
    }

    @Override
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        assert (element instanceof JSConditionalExpression);
        ConditionalUtils.replaceConditionalWithIf((JSConditionalExpression)element);
    }

    private static class ReplaceConditionalWithIfPredicate implements JSElementPredicate {
        @Override
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            return (element instanceof JSConditionalExpression);
        }
    }
}
