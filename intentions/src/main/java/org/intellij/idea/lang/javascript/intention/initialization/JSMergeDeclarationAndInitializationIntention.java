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
package org.intellij.idea.lang.javascript.intention.initialization;

import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.FindReferenceUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NonNls;
import javax.annotation.Nonnull;

import java.util.Iterator;

public class JSMergeDeclarationAndInitializationIntention extends JSIntention {
    @NonNls private static final String JS_VAR_PREFIX = "var ";

    @Override
	@Nonnull
    protected JSElementPredicate getElementPredicate() {
        return new Predicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        assert (element instanceof JSVarStatement);

        final JSVarStatement varStatement      = (JSVarStatement) element;
        StringBuilder        declarationBuffer = new StringBuilder();

        for (final JSVariable variable : varStatement.getVariables()) {
            if (variable.hasInitializer()) {
                declarationBuffer.append((declarationBuffer.length() == 0) ? JS_VAR_PREFIX : ", ")
                                 .append(variable.getName())
                                 .append(" = ")
                                 .append(variable.getInitializer().getText());
            } else {
                final Iterator<PsiElement>  referenceIterator = FindReferenceUtil.getReferencesAfter(variable, variable.getTextRange().getEndOffset()).iterator();
                final JSReferenceExpression firstReference    = (JSReferenceExpression) (referenceIterator.hasNext() ? referenceIterator.next() : null);
//                final JSReferenceExpression firstReference = FindReferenceUtil.findFirstReference(variable);

                if (firstReference != null &&
                    firstReference.getParent() instanceof JSDefinitionExpression) {
                    final JSExpressionStatement assignmentStatement = (JSExpressionStatement) firstReference.getParent().getParent().getParent();

                    // Replace assignment statement by var statement.
                    JSElementFactory.replaceStatement(assignmentStatement, JS_VAR_PREFIX + assignmentStatement.getText());
                } else {
                    declarationBuffer.append((declarationBuffer.length() == 0) ? JS_VAR_PREFIX : ", ")
                                     .append(variable.getName());
                }
            }
        }

        // Do replacement.
        if (declarationBuffer.length() == 0) {
            JSElementFactory.removeElement(varStatement);
        } else {
            declarationBuffer.append(';');
            JSElementFactory.replaceStatement(varStatement, declarationBuffer.toString());
        }
    }

    private static class Predicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSVarStatement)) {
                return false;
            }
            final JSVarStatement varStatement = (JSVarStatement) element;
            if (ErrorUtil.containsError(varStatement)) {
                return false;
            }

            for (JSVariable variable : varStatement.getVariables()) {
                if (!variable.hasInitializer()) {
                    final Iterator<PsiElement>  referenceIterator = FindReferenceUtil.getReferencesAfter(variable, variable.getTextRange().getEndOffset()).iterator();
                    final JSReferenceExpression firstReference    = (JSReferenceExpression) (referenceIterator.hasNext() ? referenceIterator.next() : null);

                    if (firstReference != null &&
                        firstReference.getParent()                         instanceof JSDefinitionExpression &&
                        firstReference.getParent().getParent()             instanceof JSAssignmentExpression &&
                        firstReference.getParent().getParent().getParent() instanceof JSExpressionStatement) {
                        return true;
                    }
                }
            }
            return false;
        }
    }
}
