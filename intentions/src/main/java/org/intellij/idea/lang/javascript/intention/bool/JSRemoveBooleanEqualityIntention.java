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
package org.intellij.idea.lang.javascript.intention.bool;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.ast.IElementType;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.BinaryOperatorUtils;
import org.intellij.idea.lang.javascript.psiutil.BoolUtils;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSRemoveBooleanEqualityIntention",
    categories = {"JavaScript", "Boolean"},
    fileExtensions = "js"
)
public class JSRemoveBooleanEqualityIntention extends JSMutablyNamedIntention {
    @Nonnull
    @Override
    protected LocalizeValue getBasicText() {
        return JSIntentionLocalize.boolRemoveBooleanEquality();
    }

    @Override
    @RequiredReadAction
    protected LocalizeValue getTextForElement(PsiElement element) {
        final JSBinaryExpression binaryExpression = (JSBinaryExpression)element;

        return JSIntentionLocalize.boolRemoveBooleanEqualityMessage(
            BinaryOperatorUtils.getOperatorText(binaryExpression.getOperationSign())
        );
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new BooleanLiteralEqualityPredicate();
    }

    @Override
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        final JSBinaryExpression exp = (JSBinaryExpression)element;
        final boolean isEquals = exp.getOperationSign().equals(JSTokenTypes.EQEQ);
        final JSExpression lhs = exp.getLOperand();
        final JSExpression rhs = exp.getROperand();

        assert (lhs != null);
        assert (rhs != null);

        final String lhsText = lhs.getText();
        final String rhsText = rhs.getText();

        if (BoolUtils.TRUE.equals(lhsText)) {
            if (isEquals) {
                JSElementFactory.replaceExpression(exp, rhsText);
            }
            else {
                JSElementFactory.replaceExpressionWithNegatedExpression(rhs, exp);
            }
        }
        else if (BoolUtils.FALSE.equals(lhsText)) {
            if (isEquals) {
                JSElementFactory.replaceExpressionWithNegatedExpression(rhs, exp);
            }
            else {
                JSElementFactory.replaceExpression(exp, rhsText);
            }
        }
        else if (BoolUtils.TRUE.equals(rhsText)) {
            if (isEquals) {
                JSElementFactory.replaceExpression(exp, lhsText);
            }
            else {
                JSElementFactory.replaceExpressionWithNegatedExpression(lhs, exp);
            }
        }
        else {
            if (isEquals) {
                JSElementFactory.replaceExpressionWithNegatedExpression(lhs, exp);
            }
            else {
                JSElementFactory.replaceExpression(exp, lhsText);
            }
        }
    }

    private static class BooleanLiteralEqualityPredicate implements JSElementPredicate {
        @Override
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSBinaryExpression) || ErrorUtil.containsError(element)) {
                return false;
            }

            final JSBinaryExpression expression = (JSBinaryExpression)element;
            final IElementType sign = expression.getOperationSign();

            if (!(JSTokenTypes.EQEQ.equals(sign) || JSTokenTypes.NE.equals(sign))) {
                return false;
            }

            final JSExpression lhs = expression.getLOperand();
            final JSExpression rhs = expression.getROperand();

            return lhs != null && rhs != null && (BoolUtils.isBooleanLiteral(lhs) || BoolUtils.isBooleanLiteral(rhs));
        }
    }
}
