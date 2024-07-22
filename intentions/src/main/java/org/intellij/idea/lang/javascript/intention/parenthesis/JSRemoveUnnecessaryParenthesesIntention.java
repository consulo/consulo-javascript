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
package org.intellij.idea.lang.javascript.intention.parenthesis;

import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.lang.javascript.psi.JSParenthesizedExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.ast.IElementType;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.ParenthesesUtils;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSRemoveUnnecessaryParenthesesIntention",
    categories = {"JavaScript", "Other"},
    fileExtensions = "js"
)
public class JSRemoveUnnecessaryParenthesesIntention extends JSIntention {
    @Override
    @Nonnull
    public String getText() {
        return JSIntentionLocalize.parenthesisRemoveUnnecessaryParentheses().get();
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new UnnecessaryParenthesesPredicate();
    }

    @Override
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        JSExpression exp = (JSExpression)element;

        while (exp.getParent() instanceof JSExpression parentExp) {
            exp = parentExp;
        }

        final String newExpression = ParenthesesUtils.removeParentheses(exp);

        JSElementFactory.replaceExpression(exp, newExpression);
    }

    private static class UnnecessaryParenthesesPredicate implements JSElementPredicate {
        @Override
        @RequiredReadAction
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (element instanceof JSParenthesizedExpression expression && !ErrorUtil.containsError(element)) {
                if (!(expression.getParent() instanceof JSExpression parentExpression)) {
                    return true;
                }

                final JSExpression body = expression.getInnerExpression();

                if (body instanceof JSParenthesizedExpression) {
                    return true;
                }

                final int parentPrecendence = ParenthesesUtils.getPrecendence(parentExpression);
                final int childPrecendence = ParenthesesUtils.getPrecendence(body);

                if (parentPrecendence > childPrecendence) {
                    return !(body instanceof JSFunctionExpression);
                }
                else if (parentPrecendence == childPrecendence
                    && parentExpression instanceof JSBinaryExpression parentBinaryExpression
                    && body instanceof JSBinaryExpression bodyBinaryExpression) {
                    final IElementType parentOperator = parentBinaryExpression.getOperationSign();
                    final IElementType childOperator = bodyBinaryExpression.getOperationSign();
                    final JSExpression lhs = parentBinaryExpression.getLOperand();

                    return lhs.equals(expression) && parentOperator.equals(childOperator);
                }
            }
            return false;
        }
    }
}
