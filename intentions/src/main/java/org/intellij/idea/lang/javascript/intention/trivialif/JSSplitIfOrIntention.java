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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.ParenthesesUtils;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSSplitIfOrIntention",
    categories = {"JavaScript", "Control Flow"},
    fileExtensions = "js"
)
public class JSSplitIfOrIntention extends JSIntention {
    @Override
    @Nonnull
    public LocalizeValue getText() {
        return JSIntentionLocalize.trivialifSplitIfOr();
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new SplitIfOrPredicate();
    }

    @Override
    @RequiredReadAction
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        PsiElement jsElement = (element.getParent() instanceof JSIfStatement ? element.getParent() : element);

        assert (jsElement != null);
        assert (jsElement instanceof JSIfStatement);

        JSIfStatement ifStatement = (JSIfStatement)jsElement;

        assert (ifStatement.getCondition() instanceof JSBinaryExpression);

        JSBinaryExpression condition = (JSBinaryExpression)ifStatement.getCondition();
        String lhsText = ParenthesesUtils.removeParentheses(condition.getLOperand());
        String rhsText = ParenthesesUtils.removeParentheses(condition.getROperand());
        JSStatement thenBranch = ifStatement.getThen();
        JSStatement elseBranch = ifStatement.getElse();
        String thenText = thenBranch.getText();

        assert JSTokenTypes.OROR.equals(condition.getOperationSign());

        StringBuilder statement = new StringBuilder(ifStatement.getTextLength());

        statement.append("if (").append(lhsText).append(')')
            .append(thenText)
            .append("else if (").append(rhsText).append(')')
            .append(thenText);

        if (elseBranch != null) {
            statement.append("else ").append(elseBranch.getText());
        }

        JSElementFactory.replaceStatement(ifStatement, statement.toString());
    }

    private static class SplitIfOrPredicate implements JSElementPredicate {
        @Override
        @RequiredReadAction
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            PsiElement parent = element.getParent();

            if (!(parent instanceof JSIfStatement)) {
                if (element instanceof JSIfStatement) {
                    parent = element;
                }
                else {
                    return false;
                }
            }

            JSIfStatement ifStatement = (JSIfStatement)parent;
            JSExpression condition = ifStatement.getCondition();

            return condition != null
                && !ErrorUtil.containsError(condition)
                && condition instanceof JSBinaryExpression binaryExpression
                && JSTokenTypes.OROR.equals(binaryExpression.getOperationSign());
        }
    }
}
