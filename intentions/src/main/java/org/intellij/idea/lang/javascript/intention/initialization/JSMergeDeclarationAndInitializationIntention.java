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
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.FindReferenceUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

import java.util.Iterator;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSMergeDeclarationAndInitializationIntention",
	categories = {"JavaScript", "Declaration"},
	fileExtensions = "js"
)
public class JSMergeDeclarationAndInitializationIntention extends JSIntention
{
	@Override
	@Nonnull
	public String getText()
	{
		return JSIntentionLocalize.initializationMergeDeclarationAndInitialization().get();
	}

	@Override
	@Nonnull
	protected JSElementPredicate getElementPredicate()
	{
		return new Predicate();
	}

	@Override
	@RequiredReadAction
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		assert (element instanceof JSVarStatement);

		final JSVarStatement varStatement = (JSVarStatement) element;
		StringBuilder declarationBuffer = new StringBuilder();

		for (final JSVariable variable : varStatement.getVariables())
		{
			if (variable.hasInitializer())
			{
				declarationBuffer.append(declarationBuffer.isEmpty() ? "var " : ", ")
					.append(variable.getName()).append(" = ").append(variable.getInitializer().getText());
			}
			else
			{
				final Iterator<PsiElement> referenceIterator =
					FindReferenceUtil.getReferencesAfter(variable, variable.getTextRange().getEndOffset()).iterator();
				final JSReferenceExpression firstReference = (JSReferenceExpression) (referenceIterator.hasNext() ? referenceIterator.next() : null);
				//final JSReferenceExpression firstReference = FindReferenceUtil.findFirstReference(variable);

				if (firstReference != null && firstReference.getParent() instanceof JSDefinitionExpression definitionExpression)
				{
					final JSExpressionStatement assignmentStatement = (JSExpressionStatement) definitionExpression.getParent().getParent();

					// Replace assignment statement by var statement.
					JSElementFactory.replaceStatement(assignmentStatement, "var " + assignmentStatement.getText());
				}
				else
				{
					declarationBuffer.append((declarationBuffer.length() == 0) ? "var " : ", ")
							.append(variable.getName());
				}
			}
		}

		// Do replacement.
		if (declarationBuffer.length() == 0)
		{
			JSElementFactory.removeElement(varStatement);
		}
		else
		{
			declarationBuffer.append(';');
			JSElementFactory.replaceStatement(varStatement, declarationBuffer.toString());
		}
	}

	private static class Predicate implements JSElementPredicate
	{
		@Override
		@RequiredReadAction
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			if (element instanceof JSVarStatement varStatement && !ErrorUtil.containsError(varStatement))
			{
				for (JSVariable variable : varStatement.getVariables())
				{
					if (variable.hasInitializer()) {
						continue;
					}

					final Iterator<PsiElement> referenceIterator =
						FindReferenceUtil.getReferencesAfter(variable, variable.getTextRange().getEndOffset()).iterator();
					final JSReferenceExpression firstReference =
						(JSReferenceExpression)(referenceIterator.hasNext() ? referenceIterator.next() : null);

					if (firstReference != null
						&& firstReference.getParent() instanceof JSDefinitionExpression definitionExpression
						&& definitionExpression.getParent() instanceof JSAssignmentExpression assignmentExpression
						&& assignmentExpression.getParent() instanceof JSExpressionStatement) {
						return true;
					}
				}
			}
			return false;
		}
	}
}
