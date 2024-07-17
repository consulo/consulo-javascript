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

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.intention.JSIntentionBundle;
import org.intellij.idea.lang.javascript.psiutil.*;

import jakarta.annotation.Nonnull;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSMergeIfOrIntention",
	categories = {"JavaScript", "Control Flow"},
	fileExtensions = "js"
)
public class JSMergeIfOrIntention extends JSIntention
{
	@Override
	protected String getBasicText() {
		return JSIntentionBundle.message("trivialif.merge-if-or.display-name");
	}

	@Override
	@Nonnull
	public JSElementPredicate getElementPredicate()
	{
		return new MergeIfOrPredicate();
	}

	@Override
	@RequiredReadAction
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		assert (element instanceof JSElement);
		JSElement jsElement = (JSElement) element;
		if (MergeIfOrPredicate.isMergableExplicitIf(jsElement))
		{
			replaceMergeableExplicitIf(jsElement);
		}
		else
		{
			replaceMergeableImplicitIf(jsElement);
		}
	}

	@RequiredReadAction
	private static void replaceMergeableExplicitIf(JSElement token) throws IncorrectOperationException
	{
		final JSIfStatement parentStatement = (JSIfStatement) (token.getParent() instanceof JSIfStatement ? token.getParent() : token);

		assert (parentStatement != null);

		final JSIfStatement childStatement = (JSIfStatement) parentStatement.getElse();
		final JSExpression childCondition = childStatement.getCondition();
		final JSExpression condition = parentStatement.getCondition();
		final String childConditionText = ParenthesesUtils.getParenthesized(childCondition, ParenthesesUtils.OR_PRECENDENCE);
		final String parentConditionText = ParenthesesUtils.getParenthesized(condition, ParenthesesUtils.OR_PRECENDENCE);
		final JSStatement parentThenBranch = parentStatement.getThen();
		final StringBuilder statement = new StringBuilder("if (")
			.append(parentConditionText)
			.append(" || ")
			.append(childConditionText)
			.append(')')
			.append(parentThenBranch.getText());
		final JSStatement childElseBranch = childStatement.getElse();

		if (childElseBranch != null)
		{
			statement.append("else ").append(childElseBranch.getText());
		}

		JSElementFactory.replaceStatement(parentStatement, statement.toString());
	}

	@RequiredReadAction
	private static void replaceMergeableImplicitIf(JSElement token) throws IncorrectOperationException
	{
		final JSIfStatement parentStatement = (JSIfStatement) (token.getParent() instanceof JSIfStatement ? token.getParent() : token);
		final JSIfStatement childStatement = (JSIfStatement) JSElementFactory.getNonWhiteSpaceSibling(parentStatement, true);

		assert (childStatement != null);
		assert (parentStatement != null);

		final JSExpression childCondition = childStatement.getCondition();
		final JSExpression condition = parentStatement.getCondition();
		final String childConditionText = ParenthesesUtils.getParenthesized(childCondition, ParenthesesUtils.OR_PRECENDENCE);
		final String parentConditionText = ParenthesesUtils.getParenthesized(condition, ParenthesesUtils.OR_PRECENDENCE);
		final JSStatement parentThenBranch = parentStatement.getThen();
		final JSStatement childElseBranch = childStatement.getElse();
		StringBuilder statement = new StringBuilder("if (")
			.append(parentConditionText)
			.append(" || ")
			.append(childConditionText)
			.append(')')
			.append(parentThenBranch.getText());

		if (childElseBranch != null)
		{
			statement.append("else ").append(childElseBranch.getText());
		}

		JSElementFactory.replaceStatement(parentStatement, statement.toString());
		JSElementFactory.removeElement(childStatement);
	}

	private static class MergeIfOrPredicate implements JSElementPredicate
	{
		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			return element instanceof JSElement jsElement
				&& (isMergableExplicitIf(jsElement) || isMergableImplicitIf(jsElement));
		}

		public static boolean isMergableExplicitIf(JSElement element)
		{
			PsiElement parent = element.getParent();

			if (!(parent instanceof JSIfStatement))
			{
				if (element instanceof JSIfStatement)
				{
					parent = element;
				}
				else
				{
					return false;
				}
			}

			final JSIfStatement ifStatement = (JSIfStatement) parent;

			if (ErrorUtil.containsError(ifStatement))
			{
				return false;
			}

			final JSStatement thenBranch = ifStatement.getThen(), elseBranch = ifStatement.getElse();

			return thenBranch != null
				&& elseBranch instanceof JSIfStatement childIfStatement
				&& EquivalenceChecker.statementsAreEquivalent(thenBranch, childIfStatement.getThen());
		}

		private static boolean isMergableImplicitIf(JSElement element)
		{
			PsiElement parent = element.getParent();

			if (!(parent instanceof JSIfStatement))
			{
				if (element instanceof JSIfStatement)
				{
					parent = element;
				}
				else
				{
					return false;
				}
			}

			final JSIfStatement ifStatement = (JSIfStatement) parent;
			final JSStatement thenBranch = ifStatement.getThen(), elseBranch = ifStatement.getElse();

			if (thenBranch == null || elseBranch != null || ControlFlowUtils.statementMayCompleteNormally(thenBranch)) {
				return false;
			}

			final PsiElement nextStatement = JSElementFactory.getNonWhiteSpaceSibling(ifStatement, true);

			return nextStatement instanceof JSIfStatement childIfStatement
				&& EquivalenceChecker.statementsAreEquivalent(thenBranch, childIfStatement.getThen());
		}
	}
}