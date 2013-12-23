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
package org.intellij.idea.lang.javascript.intention.opassign;

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.BinaryOperatorUtils;
import org.intellij.idea.lang.javascript.psiutil.EquivalenceChecker;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.SideEffectChecker;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NotNull;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;

public class JSReplaceWithOperatorAssignmentIntention extends JSMutablyNamedIntention {
    @Override
	public String getTextForElement(PsiElement element) {
        final JSAssignmentExpression exp = (JSAssignmentExpression) element;
        final JSBinaryExpression     rhs = (JSBinaryExpression) exp.getROperand();
        assert (rhs != null);
        final IElementType           sign = rhs.getOperationSign();

        return this.getText(BinaryOperatorUtils.getOperatorText(sign));
    }

    @Override
	@NotNull
    public JSElementPredicate getElementPredicate() {
        return new Predicate();
    }

    @Override
	public void processIntention(@NotNull PsiElement element) throws IncorrectOperationException {
        final JSAssignmentExpression exp = (JSAssignmentExpression) element;
        final JSBinaryExpression     rhs = (JSBinaryExpression) exp.getROperand();
        final JSExpression           lhs = exp.getLOperand();

        assert (rhs != null);

        final IElementType  sign    = rhs.getOperationSign();
        final String        operand = BinaryOperatorUtils.getOperatorText(sign);
        final JSExpression  rhsrhs  = rhs.getROperand();

        assert (rhsrhs != null);

        JSElementFactory.replaceExpression(exp, lhs.getText() + operand + '=' + rhsrhs.getText());
    }

    private static class Predicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@NotNull PsiElement element) {
            if (!(element instanceof JSAssignmentExpression)) {
                return false;
            }
            if (ErrorUtil.containsError(element)) {
                return false;
            }
            final JSAssignmentExpression assignment = (JSAssignmentExpression) element;
            final IElementType           tokenType  = assignment.getOperationSign();
            if (!JSTokenTypes.EQ.equals(tokenType)) {
                return false;
            }
            JSExpression       lhs = assignment.getLOperand();
            final JSExpression rhs = assignment.getROperand();

            if (lhs instanceof JSDefinitionExpression) {
                lhs = ((JSDefinitionExpression) lhs).getExpression();
            }
            if (lhs == null || rhs == null) {
                return false;
            }
            if (!(rhs instanceof JSBinaryExpression)) {
                return false;
            }
            final JSBinaryExpression binaryRhs = (JSBinaryExpression) rhs;
            final JSExpression       rhsRhs    = binaryRhs.getROperand();
            final JSExpression       rhsLhs    = binaryRhs.getLOperand();

            if (rhsRhs == null) {
                return false;
            }

            final IElementType rhsTokenType = binaryRhs.getOperationSign();

            if (JSTokenTypes.OROR  .equals(rhsTokenType)  ||
                JSTokenTypes.ANDAND.equals(rhsTokenType)) {
                return false;
            }
            if (SideEffectChecker.mayHaveSideEffects(lhs)) {
                return false;
            }
            return EquivalenceChecker.expressionsAreEquivalent(lhs, rhsLhs);
        }
    }
}
