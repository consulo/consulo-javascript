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

import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSReturnStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

@ExtensionImpl
@IntentionMetaData(ignoreId = "JSRemoveRedundantElseIntention", categories = {
		"JavaScript",
		"Control Flow"
}, fileExtensions = "js")
public class JSRemoveRedundantElseIntention extends JSIntention
{
	@NonNls
	private static final String IF_STATEMENT_PREFIX = "if (";

	@Override
	@Nonnull
	public JSElementPredicate getElementPredicate()
	{
		return new RemoveRedundantElsePredicate();
	}

	@Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		final JSIfStatement ifStatement = (JSIfStatement) element;
		final JSStatement thenBranch = ifStatement.getThen();
		final JSStatement elseBranch = ifStatement.getElse();

		assert (thenBranch != null);
		assert (elseBranch != null);

		final String newIfText = IF_STATEMENT_PREFIX + ifStatement.getCondition().getText() + ')' + thenBranch.getText();
		final String elseText = elseBranch.getText();
		final String newStatement = (elseBranch instanceof JSBlockStatement
				? elseText.substring(elseText.indexOf('{') + 1, elseText.lastIndexOf('}') - 1).trim()
				: elseText);

		JSElementFactory.addStatementAfter(ifStatement, newStatement);
		JSElementFactory.replaceStatement(ifStatement, newIfText);
	}

	private static class RemoveRedundantElsePredicate implements JSElementPredicate
	{
		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			if(!(element instanceof JSIfStatement))
			{
				return false;
			}

			final JSIfStatement ifStatement = (JSIfStatement) element;

			if(ErrorUtil.containsError(ifStatement))
			{
				return false;
			}

			final JSStatement elseBranch = ifStatement.getElse();
			JSStatement thenBranch = ifStatement.getThen();

			if(elseBranch == null)
			{
				return false;
			}

			while(thenBranch != null && thenBranch instanceof JSBlockStatement)
			{
				JSStatement[] thenStatements = ((JSBlockStatement) thenBranch).getStatements();

				if(thenStatements.length == 0)
				{
					return true;
				}
				else if(thenStatements.length > 1)
				{
					return false;
				}
				thenBranch = thenStatements[0];
			}

			return (thenBranch != null && thenBranch instanceof JSReturnStatement);
		}
	}
}
