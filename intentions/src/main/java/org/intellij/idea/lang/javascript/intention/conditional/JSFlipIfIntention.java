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
package org.intellij.idea.lang.javascript.intention.conditional;

import com.intellij.lang.javascript.psi.JSBlockStatement;
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
import org.intellij.idea.lang.javascript.psiutil.BoolUtils;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSFlipIfIntention",
    categories = {"JavaScript", "Conditional"},
    fileExtensions = "js"
)
public class JSFlipIfIntention extends JSIntention {
    @Override
    @Nonnull
    public LocalizeValue getText() {
        return JSIntentionLocalize.conditionalFlipIf();
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new FlipIfPredicate();
    }

    @Override
    @RequiredReadAction
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        final JSIfStatement exp = (JSIfStatement)element;
        final JSExpression condition = exp.getCondition();
        final JSStatement thenBranch = exp.getThen();
        final JSStatement elseBranch = exp.getElse();
        final String negatedText = BoolUtils.getNegatedExpressionText(condition);
        final boolean emptyThenBranch =
            (thenBranch == null || (thenBranch instanceof JSBlockStatement blockStatement && blockStatement.getStatements().length == 0));
        final String thenText = emptyThenBranch ? "" : "else " + thenBranch.getText();
        final String elseText = elseBranch == null ? "{}" : elseBranch.getText();

        final String newStatement = "if (" + negatedText + ')' + elseText + thenText;

        JSElementFactory.replaceStatement(exp, newStatement);
    }

    private static class FlipIfPredicate implements JSElementPredicate {
        @Override
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            return element instanceof JSIfStatement ifStatement
                && !ErrorUtil.containsError(ifStatement)
                && ifStatement.getCondition() != null;
        }
    }
}
