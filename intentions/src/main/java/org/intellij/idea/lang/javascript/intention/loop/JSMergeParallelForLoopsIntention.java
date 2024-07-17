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
package org.intellij.idea.lang.javascript.intention.loop;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSForStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSVarStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ControlFlowUtils;
import org.intellij.idea.lang.javascript.psiutil.EquivalenceChecker;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

import jakarta.annotation.Nonnull;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSMergeParallelForLoopsIntention",
	categories = {"JavaScript", "Control Flow"},
	fileExtensions = "js"
)
public class JSMergeParallelForLoopsIntention extends JSIntention
{
	@Override
	@Nonnull
	public JSElementPredicate getElementPredicate()
	{
		return new MergeParallelForLoopsPredicate();
	}

	@Override
	@RequiredReadAction
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		final PsiElement nextElement = JSElementFactory.getNonWhiteSpaceSibling(element, true);

		assert (nextElement != null);

		final JSForStatement firstStatement = (JSForStatement) element;
		final JSForStatement secondStatement = (JSForStatement) nextElement;
		final StringBuilder statementBuffer = new StringBuilder();

		this.mergeForStatements(statementBuffer, firstStatement, secondStatement);
		JSElementFactory.replaceStatement(firstStatement, statementBuffer.toString());
		JSElementFactory.removeElement(secondStatement);
	}

	@RequiredReadAction
	private void mergeForStatements(StringBuilder statementBuffer, JSForStatement firstStatement, JSForStatement secondStatement)
	{
		final JSExpression initialization = firstStatement.getInitialization();
		final JSVarStatement varStatement = firstStatement.getVarDeclaration();
		final JSExpression condition = firstStatement.getCondition();
		final JSExpression update = firstStatement.getUpdate();
		final JSStatement firstBody = firstStatement.getBody();
		final JSStatement secondBody = secondStatement.getBody();

		statementBuffer.append("for (")
			.append((initialization == null) ? varStatement.getText() : initialization.getText())
			.append(';')
			.append(condition.getText())
			.append(';')
			.append(update.getText())
			.append(')');
		ControlFlowUtils.appendStatementsInSequence(statementBuffer, firstBody, secondBody);
	}

	private static class MergeParallelForLoopsPredicate implements JSElementPredicate
	{
		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			if (!(element instanceof JSForStatement forStatement && !ErrorUtil.containsError(forStatement)))
			{
				return false;
			}

			final PsiElement nextStatement = JSElementFactory.getNonWhiteSpaceSibling(element, true);

			return nextStatement instanceof JSForStatement nextForStatement
				&& !ErrorUtil.containsError(nextStatement)
				&& forStatementsCanBeMerged(forStatement, nextForStatement);
		}

		public static boolean forStatementsCanBeMerged(JSForStatement statement1, JSForStatement statement2)
		{
			final JSExpression firstInitialization = statement1.getInitialization();
			final JSExpression secondInitialization = statement2.getInitialization();
			if (!EquivalenceChecker.expressionsAreEquivalent(firstInitialization, secondInitialization))
			{
				return false;
			}

			final JSVarStatement firstVarStatement = statement1.getVarDeclaration();
			final JSVarStatement secondVarStatement = statement2.getVarDeclaration();
			if (!EquivalenceChecker.statementsAreEquivalent(firstVarStatement, secondVarStatement))
			{
				return false;
			}

			final JSExpression firstCondition = statement1.getCondition();
			final JSExpression secondCondition = statement2.getCondition();
			if (!EquivalenceChecker.expressionsAreEquivalent(firstCondition, secondCondition))
			{
				return false;
			}

			final JSExpression firstUpdate = statement1.getUpdate();
			final JSExpression secondUpdate = statement2.getUpdate();
			if (!EquivalenceChecker.expressionsAreEquivalent(firstUpdate, secondUpdate))
			{
				return false;
			}

			final JSStatement firstBody = statement1.getBody();
			final JSStatement secondBody = statement2.getBody();
			return (firstBody == null || secondBody == null || ControlFlowUtils.canBeMerged(firstBody, secondBody));
		}
	}
}
