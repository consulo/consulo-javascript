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

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.psiutil.ComparisonUtils;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import javax.annotation.Nonnull;

import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.PsiElement;

class ComparisonPredicate implements JSElementPredicate {
    @Override
	public boolean satisfiedBy(@Nonnull PsiElement element) {
        if (!(element instanceof JSBinaryExpression)) {
            return false;
        }
        if (ErrorUtil.containsError(element)) {
            return false;
        }

        final JSBinaryExpression expression = (JSBinaryExpression) element;
        final JSExpression       rhs        = expression.getROperand();

        return (rhs != null &&
                ComparisonUtils.isComparisonOperator((JSExpression) element));

    }
}
