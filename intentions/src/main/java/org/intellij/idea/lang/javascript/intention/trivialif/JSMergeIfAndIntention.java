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
package org.intellij.idea.lang.javascript.intention.trivialif;

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ConditionalUtils;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.ParenthesesUtils;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSMergeIfAndIntention",
    categories = {"JavaScript", "Control Flow"},
    fileExtensions = "js"
)
public class JSMergeIfAndIntention extends JSIntention {
    @Override
    @Nonnull
    public String getText() {
        return JSIntentionLocalize.trivialifMergeIfAnd().get();
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new MergeIfAndPredicate();
    }

    @Override
    @RequiredReadAction
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        assert (element.getParent() != null);
        assert (element.getParent() instanceof JSIfStatement || element instanceof JSIfStatement);

        final JSIfStatement parentStatement = (JSIfStatement)(element.getParent() instanceof JSIfStatement ? element.getParent() : element);
        final JSIfStatement childStatement = (JSIfStatement)ConditionalUtils.stripBraces(parentStatement.getThen());
        final JSExpression childCondition = childStatement.getCondition();
        final JSExpression parentCondition = parentStatement.getCondition();
        final String childConditionText = ParenthesesUtils.getParenthesized(childCondition, ParenthesesUtils.AND_PRECENDENCE);
        final String parentConditionText = ParenthesesUtils.getParenthesized(parentCondition, ParenthesesUtils.AND_PRECENDENCE);
        final JSStatement childThenBranch = childStatement.getThen();
        final String statement = "if (" + parentConditionText + " && " + childConditionText + ')' + childThenBranch.getText();

        JSElementFactory.replaceStatement(parentStatement, statement);
    }

    private static class MergeIfAndPredicate implements JSElementPredicate {

        @Override
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSElement)) {
                return false;
            }

            PsiElement parent = element.getParent();

            if (!(parent instanceof JSIfStatement)) {
                if (element instanceof JSIfStatement) {
                    parent = element;
                }
                else {
                    return false;
                }
            }

            final JSIfStatement ifStatement = (JSIfStatement)parent;

            if (ErrorUtil.containsError(ifStatement)) {
                return false;
            }

            final JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());
            final JSStatement elseBranch = ConditionalUtils.stripBraces(ifStatement.getElse());

            return elseBranch == null && thenBranch instanceof JSIfStatement childIfStatement && childIfStatement.getElse() == null;
        }
    }
}