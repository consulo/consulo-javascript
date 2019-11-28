/*
 * Copyright 2013-2015 must-be.org
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

package consulo.javascript.ide.completion;

import javax.annotation.Nonnull;
import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.completion.util.ParenthesesInsertHandler;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.patterns.StandardPatterns;
import com.intellij.psi.PsiElement;
import com.intellij.util.ProcessingContext;
import consulo.annotation.access.RequiredReadAction;
import consulo.codeInsight.completion.CompletionProvider;

/**
 * @author VISTALL
 * @since 20.12.2015
 */
public class JavaScriptKeywordCompletionContributor extends CompletionContributor
{
	public JavaScriptKeywordCompletionContributor()
	{
		extend(CompletionType.BASIC, StandardPatterns.psiElement().withParent(JSReferenceExpression.class), new CompletionProvider()
		{
			@RequiredReadAction
			@Override
			public void addCompletions(@Nonnull CompletionParameters parameters, ProcessingContext context, @Nonnull CompletionResultSet result)
			{
				PsiElement position = parameters.getPosition();
				JSReferenceExpression parent = (JSReferenceExpression) position.getParent();
				if(parent.getQualifier() != null)
				{
					return;
				}

				boolean parentIsStatement = parent.getParent() instanceof JSExpressionStatement;

				LookupElementBuilder functionKeyword = LookupElementBuilder.create("function");
				functionKeyword = functionKeyword.bold();
				if(parentIsStatement)
				{
					functionKeyword = functionKeyword.withInsertHandler(SpaceInsertHandler.INSTANCE);
				}
				else
				{
					functionKeyword = functionKeyword.withInsertHandler(ParenthesesInsertHandler.getInstance(false));
					functionKeyword = functionKeyword.withPresentableText("function()");
				}

				result.addElement(functionKeyword);

				result.addElement(LookupElementBuilder.create("var").withInsertHandler(SpaceInsertHandler.INSTANCE).bold());
			}
		});
	}
}
