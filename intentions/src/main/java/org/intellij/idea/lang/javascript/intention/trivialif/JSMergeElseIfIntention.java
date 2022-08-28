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

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import javax.annotation.Nonnull;

import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;

public class JSMergeElseIfIntention extends JSIntention {
    @Override
	@Nonnull
    public JSElementPredicate getElementPredicate() {
        return new MergeElseIfPredicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        final JSIfStatement parentStatement = (JSIfStatement) element.getParent();

        assert (parentStatement != null);

        final JSBlockStatement elseBranch         = (JSBlockStatement) parentStatement.getElse();
        final JSStatement      elseBranchContents = elseBranch.getStatements()[0];

        JSElementFactory.replaceStatement(elseBranch, elseBranchContents.getText());
    }

    private static class MergeElseIfPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSElement)) {
                return false;
            }

            final PsiElement  parent = element.getParent();

            if (!(parent instanceof JSIfStatement)) {
                return false;
            }

            final JSIfStatement ifStatement = (JSIfStatement) parent;

            if (ErrorUtil.containsError(ifStatement)) {
                return false;
            }

            final JSStatement thenBranch = ifStatement.getThen();
            final JSStatement elseBranch = ifStatement.getElse();

            if (thenBranch == null) {
                return false;
            }
            if (elseBranch == null) {
                return false;
            }
            if (!(elseBranch instanceof JSBlockStatement)) {
                return false;
            }

            final JSStatement[] statements = ((JSBlockStatement) elseBranch).getStatements();

            if (statements.length != 1) {
                return false;
            }
            return (statements[0] != null && statements[0] instanceof JSIfStatement);
        }
    }
}