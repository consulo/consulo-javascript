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
package org.intellij.idea.lang.javascript.intention.increment;

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.BinaryOperatorUtils;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.ExpressionUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.TreeUtil;
import org.jetbrains.annotations.NotNull;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSPostfixExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.intellij.lang.javascript.psi.JSReturnStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSThrowStatement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;

public class JSExtractIncrementIntention extends JSMutablyNamedIntention {
    public String getTextForElement(PsiElement element) {
        return this.getText(BinaryOperatorUtils.getOperatorText(getOperationSign(element)));
    }

    @NotNull
    public JSElementPredicate getElementPredicate() {
        return new ExtractIncrementPredicate();
    }

    public void processIntention(@NotNull PsiElement element) throws IncorrectOperationException {
        final boolean      isPostfix = (element instanceof JSPostfixExpression);
        final JSExpression operand   = (isPostfix ? ((JSPostfixExpression) element).getExpression()
                                                  : ((JSPrefixExpression)  element).getExpression());
        final JSStatement  statement = TreeUtil.getParentOfType(element, JSStatement.class);

        assert (statement != null);

        if (isPostfix) {
            JSElementFactory.addStatementAfter (statement, element.getText() + ';');
        } else {
            JSElementFactory.addStatementBefore(statement, element.getText() + ';');
        }
        JSElementFactory.replaceExpression((JSExpression) element, operand.getText());
    }

    private static IElementType getOperationSign(PsiElement element) {
        return ((element instanceof JSPostfixExpression)
                     ? ((JSPostfixExpression) element).getOperationSign()
                     : ((JSPrefixExpression)  element).getOperationSign());
    }

    private static class ExtractIncrementPredicate implements JSElementPredicate {
        public boolean satisfiedBy(@NotNull PsiElement element) {
            if (!ExpressionUtil.isIncrementDecrementExpression(element)) {
                return false;
            }
            if (ErrorUtil.containsError(element)) {
                return false;
            }

            final PsiElement parent = element.getParent();

            if (parent instanceof JSExpressionStatement) {
                return false;
            }

            final JSStatement containingStatement = TreeUtil.getParentOfType(element, JSStatement.class);

            if (element instanceof JSPostfixExpression &&
                (containingStatement instanceof JSReturnStatement ||
                 containingStatement instanceof JSThrowStatement)) {
                return false;
            }
            return (containingStatement != null);
        }
    }
}
