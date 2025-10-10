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
import org.intellij.idea.lang.javascript.psiutil.ControlFlowUtils;
import org.intellij.idea.lang.javascript.psiutil.EquivalenceChecker;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSMergeParallelIfsIntention",
    categories = {"JavaScript", "Control Flow"},
    fileExtensions = "js"
)
public class JSMergeParallelIfsIntention extends JSIntention {
    @Override
    @Nonnull
    public LocalizeValue getText() {
        return JSIntentionLocalize.trivialifMergeParallelIfs();
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new MergeParallelIfsPredicate();
    }

    @Override
    @RequiredReadAction
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        final PsiElement nextElement = JSElementFactory.getNonWhiteSpaceSibling(element, true);

        assert (nextElement != null);

        final JSIfStatement firstStatement = (JSIfStatement)element;
        final JSIfStatement secondStatement = (JSIfStatement)nextElement;
        final StringBuilder statementBuffer = new StringBuilder();

        this.mergeIfStatements(statementBuffer, firstStatement, secondStatement);
        JSElementFactory.replaceStatement(firstStatement, statementBuffer.toString());
        JSElementFactory.removeElement(secondStatement);
    }

    @RequiredReadAction
    private void mergeIfStatements(StringBuilder statementBuffer, JSIfStatement firstStatement, JSIfStatement secondStatement) {
        final JSExpression condition = firstStatement.getCondition();
        final JSStatement firstThenBranch = firstStatement.getThen();
        final JSStatement secondThenBranch = secondStatement.getThen();
        final JSStatement firstElseBranch = firstStatement.getElse();
        final JSStatement secondElseBranch = secondStatement.getElse();

        statementBuffer.append("if (").append(condition.getText()).append(')');
        ControlFlowUtils.appendStatementsInSequence(statementBuffer, firstThenBranch, secondThenBranch);

        if (firstElseBranch != null || secondElseBranch != null) {
            statementBuffer.append("else ");
            if (firstElseBranch instanceof JSIfStatement firstIfStatement
                && secondElseBranch instanceof JSIfStatement secondIfStatement
                && MergeParallelIfsPredicate.ifStatementsCanBeMerged(firstIfStatement, secondIfStatement)) {
                this.mergeIfStatements(statementBuffer, firstIfStatement, secondIfStatement);
            }
            else {
                ControlFlowUtils.appendStatementsInSequence(statementBuffer, firstElseBranch, secondElseBranch);
            }
        }
    }

    private static class MergeParallelIfsPredicate implements JSElementPredicate {
        @Override
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            return element instanceof JSIfStatement ifStatement
                && !ErrorUtil.containsError(element)
                && JSElementFactory.getNonWhiteSpaceSibling(element, true) instanceof JSIfStatement nextIfStatement
                && !ErrorUtil.containsError(nextIfStatement)
                && ifStatementsCanBeMerged(ifStatement, nextIfStatement);
        }

        public static boolean ifStatementsCanBeMerged(JSIfStatement statement1, JSIfStatement statement2) {
            final JSStatement thenBranch = statement1.getThen();
            final JSStatement elseBranch = statement1.getElse();
            if (thenBranch == null) {
                return false;
            }
            final JSExpression firstCondition = statement1.getCondition();
            final JSExpression secondCondition = statement2.getCondition();
            if (!EquivalenceChecker.expressionsAreEquivalent(firstCondition, secondCondition)) {
                return false;
            }

            final JSStatement nextThenBranch = statement2.getThen();
            if (!ControlFlowUtils.canBeMerged(thenBranch, nextThenBranch)) {
                return false;
            }

            final JSStatement nextElseBranch = statement2.getElse();
            return (elseBranch == null || nextElseBranch == null || ControlFlowUtils.canBeMerged(elseBranch, nextElseBranch));
        }
    }
}