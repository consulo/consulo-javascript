/*
 * Copyright 2000-2005 JetBrains s.r.o.
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
package com.intellij.javascript;

import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionResult;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.completion.CompletionService;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.lang.Language;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.util.Computable;
import com.intellij.psi.util.PsiUtilBase;
import com.intellij.util.Consumer;

/**
 * @author peter
 */
public class JSCompletionContributor extends CompletionContributor
{
	@RequiredReadAction
	@Override
	public void fillCompletionVariants(final CompletionParameters parameters, final CompletionResultSet result)
	{
		if(parameters.getCompletionType() == CompletionType.SMART && getElementLanguage(parameters).isKindOf(JavaScriptLanguage.INSTANCE))
		{
			final CompletionParameters newParams = parameters.withType(CompletionType.BASIC);
			CompletionService.getCompletionService().getVariantsFromContributors(newParams, this, new Consumer<CompletionResult>()
			{
				@Override
				public void consume(final CompletionResult lookupElement)
				{
					result.addElement(lookupElement.getLookupElement());
				}
			});
		}
	}

	private static Language getElementLanguage(final CompletionParameters parameters)
	{
		return ApplicationManager.getApplication().runReadAction(new Computable<Language>()
		{
			@Override
			public Language compute()
			{
				return PsiUtilBase.getLanguageAtOffset(parameters.getPosition().getContainingFile(), parameters.getOffset());
			}
		});
	}
}
