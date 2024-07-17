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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSForInStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSVarStatement;
import consulo.annotation.access.RequiredReadAction;import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.ASTNode;
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
	ignoreId = "JSMergeParallelForInLoopsIntention",
	categories = {"JavaScript", "Control Flow"},
	fileExtensions = "js"
)
public class JSMergeParallelForInLoopsIntention extends JSIntention
{
	@Override
	@Nonnull
	public JSElementPredicate getElementPredicate()
	{
		return new MergeParallelForInLoopsPredicate();
	}

	@Override
	@RequiredReadAction
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		final PsiElement nextElement = JSElementFactory.getNonWhiteSpaceSibling(element, true);

		assert (nextElement != null);

		final JSForInStatement firstStatement = (JSForInStatement) element;
		final JSForInStatement secondStatement = (JSForInStatement) nextElement;
		final StringBuilder statementBuffer = new StringBuilder();

		this.mergeForInStatements(statementBuffer, firstStatement, secondStatement);
		JSElementFactory.replaceStatement(firstStatement, statementBuffer.toString());
		JSElementFactory.removeElement(secondStatement);
	}

	@RequiredReadAction
	private void mergeForInStatements(StringBuilder statementBuffer, JSForInStatement firstStatement, JSForInStatement secondStatement)
	{
		final JSExpression variableExpression = getVariableExpression(firstStatement);
		final JSVarStatement declaration = firstStatement.getDeclarationStatement();
		final JSExpression collectionExpression = getCollectionExpression(firstStatement);
		final JSStatement firstBody = firstStatement.getBody();
		final JSStatement secondBody = secondStatement.getBody();

		statementBuffer.append("for (")
			.append((declaration == null) ? variableExpression.getText() : declaration.getText())
			.append(" in ")
			.append(collectionExpression.getText())
			.append(')');
		ControlFlowUtils.appendStatementsInSequence(statementBuffer, firstBody, secondBody);
	}

	private static class MergeParallelForInLoopsPredicate implements JSElementPredicate
	{
		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			return element instanceof JSForInStatement forInStatement
				&& !ErrorUtil.containsError(element)
				&& JSElementFactory.getNonWhiteSpaceSibling(element, true) instanceof JSForInStatement nextForInStatement
				&& !ErrorUtil.containsError(nextForInStatement)
				&& forInStatementsCanBeMerged(forInStatement, nextForInStatement);
		}

		public static boolean forInStatementsCanBeMerged(JSForInStatement statement1, JSForInStatement statement2)
		{
			final JSExpression firstVarExpression = getVariableExpression(statement1);
			final JSExpression secondVarExpression = getVariableExpression(statement2);
			if (!EquivalenceChecker.expressionsAreEquivalent(firstVarExpression, secondVarExpression))
			{
				return false;
			}

			final JSVarStatement firstDeclaration = statement1.getDeclarationStatement();
			final JSVarStatement secondDeclaration = statement2.getDeclarationStatement();
			if (!EquivalenceChecker.statementsAreEquivalent(firstDeclaration, secondDeclaration))
			{
				return false;
			}

			final JSExpression firstCollection = getCollectionExpression(statement1);
			final JSExpression secondCollection = getCollectionExpression(statement2);
			if (!EquivalenceChecker.expressionsAreEquivalent(firstCollection, secondCollection))
			{
				return false;
			}

			final JSStatement firstBody = statement1.getBody();
			final JSStatement secondBody = statement2.getBody();
			return firstBody == null || secondBody == null || ControlFlowUtils.canBeMerged(firstBody, secondBody);
		}
	}

	/**
	 * Method provided as a workaround of a bug in the JavaScript language IDEA plugin.
	 *
	 * @param forInStatement the for-in statement
	 * @return the for-in statement collection expression
	 */
	private static JSExpression getCollectionExpression(JSForInStatement forInStatement)
	{
		final ASTNode statementNode = forInStatement.getNode();
		ASTNode child = (statementNode == null) ? null : statementNode.getFirstChildNode();
		boolean inPassed = false;

		while (child != null)
		{
			if (child.getElementType() == JSTokenTypes.IN_KEYWORD)
			{
				inPassed = true;
			}
			if (inPassed && child.getPsi() instanceof JSExpression childExpression)
			{
				return childExpression;
			}
			child = child.getTreeNext();
		}

		return null;
	}

	/**
	 * Method provided as a workaround of a bug in the JavaScript language IDEA plugin.
	 *
	 * @param forInStatement the for-in statement
	 * @return the for-in statement collection expression
	 */
	private static JSExpression getVariableExpression(JSForInStatement forInStatement)
	{
		final ASTNode statementNode = forInStatement.getNode();
		ASTNode child = (statementNode == null) ? null : statementNode.getFirstChildNode();

		while (child != null)
		{
			if (child.getElementType() == JSTokenTypes.IN_KEYWORD)
			{
				return null;
			}

			if (child.getPsi() instanceof JSExpression childExpression)
			{
				return childExpression;
			}
			child = child.getTreeNext();
		}

		return null;
	}
}
