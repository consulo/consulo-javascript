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

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSPsiImplUtils;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.intention.JSIntentionBundle;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NonNls;

import java.util.ArrayList;
import java.util.List;

@ExtensionImpl
@IntentionMetaData(
	ignoreId = "JSSplitDeclarationAndInitializationIntention",
	categories = {"JavaScript", "Declaration"},
	fileExtensions = "js"
)
public class JSSplitDeclarationAndInitializationIntention extends JSIntention
{
	@NonNls
	private static final String VAR_KEYWORD = "var ";

	@Override
	protected String getBasicText() {
		return JSIntentionBundle.message("initialization.split-declaration-and-initialization.display-name");
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
		List<String> initializations = new ArrayList<>();

		for (JSVariable variable : varStatement.getVariables())
		{
			declarationBuffer.append(declarationBuffer.isEmpty() ? VAR_KEYWORD : ",")
					.append(variable.getName());

			String s = JSPsiImplUtils.getTypeFromDeclaration(variable);
			final PsiFile containingFile = element.getContainingFile();

			if (s == null && containingFile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4)
			{
				s = JSResolveUtil.getExpressionType(variable.getInitializer(), containingFile);
			}

			if (s != null)
			{
				declarationBuffer.append(":").append(s);
			}
			if (variable.hasInitializer())
			{
				initializations.add(variable.getName() + '=' + variable.getInitializer().getText() + ';');
			}
		}
		declarationBuffer.append(';');

		// Do replacement.
		JSStatement newStatement = JSElementFactory.replaceStatement(varStatement, declarationBuffer.toString());

		for (final String initialization : initializations)
		{
			newStatement = JSElementFactory.addStatementAfter(newStatement, initialization);
		}
	}

	private static class Predicate implements JSElementPredicate
	{
		@Override
		public boolean satisfiedBy(@Nonnull PsiElement element)
		{
			PsiElement elementParent;

			if (!(element instanceof JSVarStatement)
				|| (elementParent = element.getParent()) instanceof JSForStatement ||
					elementParent instanceof JSClass
			)
			{
				return false;
			}

			final JSVarStatement varStatement = (JSVarStatement) element;
			if (ErrorUtil.containsError(varStatement))
			{
				return false;
			}

			for (JSVariable variable : varStatement.getVariables())
			{
				if (variable.hasInitializer())
				{
					return true;
				}
			}
			return false;
		}
	}
}
