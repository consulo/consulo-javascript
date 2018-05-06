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
package org.intellij.idea.lang.javascript.intention.braces;

import javax.annotation.Nonnull;

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NonNls;

import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSDoWhileStatement;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSForInStatement;
import com.intellij.lang.javascript.psi.JSForStatement;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.JSWhileStatement;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;

public class JSRemoveBracesIntention extends JSMutablyNamedIntention {
    @NonNls private static final String IF_KEYWORD   = "if";
    @NonNls private static final String ELSE_KEYWORD = "else";

    @Override
	@Nonnull
    protected JSElementPredicate getElementPredicate() {
        return new RemoveBracesPredicate();
    }

    @Override
	protected String getTextForElement(PsiElement element) {
        final JSElement parent = (JSElement) element.getParent();
        final String    keyword;

        assert (parent != null);

        if (parent instanceof JSIfStatement) {
            final JSIfStatement ifStatement = (JSIfStatement) parent;
            final JSStatement   elseBranch  = ifStatement.getElse();

            keyword = (element.equals(elseBranch) ? ELSE_KEYWORD : IF_KEYWORD);
        } else {
            final PsiElement keywordChild = parent.getFirstChild();

            assert (keywordChild != null);
            keyword = keywordChild.getText();
        }

        return this.getText(keyword);
    }

    @Override
	protected void processIntention(@Nonnull PsiElement element)
        throws IncorrectOperationException {
        final JSBlockStatement blockStatement = (JSBlockStatement) element;
        final JSStatement[]    statements     = blockStatement.getStatements();
        final JSStatement      statement      = statements[0];

        // handle comments
        final JSElement parent = (JSElement) blockStatement.getParent();

        assert (parent != null);

        final JSElement grandParent = (JSElement) parent.getParent();

        assert (grandParent != null);

        PsiElement sibling = statement.getFirstChild();

        assert (sibling != null);

        sibling = sibling.getNextSibling();
        while (sibling != null && !sibling.equals(statement)) {
            if (sibling instanceof PsiComment) {
                grandParent.addBefore(sibling, parent);
            }
            sibling = sibling.getNextSibling();
        }

        final PsiElement lastChild = blockStatement.getLastChild();

        if (lastChild instanceof PsiComment) {
            final JSElement nextSibling = (JSElement) parent.getNextSibling();

            grandParent.addAfter(lastChild, nextSibling);
        }

        String text = statement.getText();
        JSElementFactory.replaceStatement(blockStatement, text);
    }

    public static class RemoveBracesPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSBlockStatement)) {
                return false;
            }

            final JSBlockStatement blockStatement = (JSBlockStatement) element;
            final PsiElement        parent         = blockStatement.getParent();

            if (!(parent instanceof JSIfStatement      ||
                  parent instanceof JSWhileStatement   ||
                  parent instanceof JSDoWhileStatement ||
                  parent instanceof JSForStatement     ||
                  parent instanceof JSForInStatement)) {
                return false;
            }

            final JSStatement[] statements = blockStatement.getStatements();

            return (statements.length == 1 && !(statements[0] instanceof JSVarStatement));
        }
    }
}