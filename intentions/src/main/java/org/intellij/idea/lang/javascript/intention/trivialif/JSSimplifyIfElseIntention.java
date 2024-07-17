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
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.intention.JSIntentionBundle;
import org.intellij.idea.lang.javascript.psiutil.ConditionalUtils;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;

import jakarta.annotation.Nonnull;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSSimplifyIfElseIntention",
	categories = {"JavaScript", "Control Flow"},
	fileExtensions = "js"
)
public class JSSimplifyIfElseIntention extends JSIntention
{
	@Override
	protected String getBasicText() {
		return JSIntentionBundle.message("trivialif.simplify-if-else.display-name");
	}

	@Override
	@Nonnull
	public JSElementPredicate getElementPredicate()
	{
		return new SimplifyIfElsePredicate();
	}

	@Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		final PsiElement statement = element.getParent() instanceof JSIfStatement ? element.getParent() : element;

		ConditionalUtils.replaceAssignmentOrReturnIfSimplifiable((JSIfStatement) statement);
	}

	private static class SimplifyIfElsePredicate implements JSElementPredicate
	{
		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			if (!(element instanceof JSElement))
			{
				return false;
			}

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
			if (ErrorUtil.containsError(parent))
			{
				return false;
			}

			final JSIfStatement ifStatement = (JSIfStatement) parent;
			final JSExpression condition = ifStatement.getCondition();

			if (condition == null || !condition.isValid())
			{
				return false;
			}

			return ConditionalUtils.isSimplifiableAssignment(ifStatement, false)
				|| ConditionalUtils.isSimplifiableAssignment(ifStatement, true)
				|| ConditionalUtils.isSimplifiableReturn(ifStatement, false)
				|| ConditionalUtils.isSimplifiableReturn(ifStatement, true)
				|| ConditionalUtils.isSimplifiableImplicitReturn(ifStatement, false)
				|| ConditionalUtils.isSimplifiableImplicitReturn(ifStatement, true)
				|| ConditionalUtils.isSimplifiableImplicitAssignment(ifStatement, false)
				|| ConditionalUtils.isSimplifiableImplicitAssignment(ifStatement, true);
		}
	}
}
