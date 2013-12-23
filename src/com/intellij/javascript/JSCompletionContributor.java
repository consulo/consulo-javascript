/*
 * Copyright (c) 2000-2005 by JetBrains s.r.o. All Rights Reserved.
 * Use is subject to license terms.
 */
package com.intellij.javascript;

import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionResult;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.completion.CompletionService;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.util.Computable;
import com.intellij.psi.util.PsiUtilBase;
import com.intellij.util.Consumer;

/**
 * @author peter
 */
public class JSCompletionContributor extends CompletionContributor
{
	private static boolean ourDoingSmartCodeCompleteAction;

	public static boolean isDoingSmartCodeCompleteAction()
	{
		return ourDoingSmartCodeCompleteAction;
	}

	@Override
	public void fillCompletionVariants(final CompletionParameters parameters, final CompletionResultSet result)
	{
		ourDoingSmartCodeCompleteAction = parameters.getCompletionType() == CompletionType.SMART && getElementLanguage(parameters).isKindOf
				(JavaScriptSupportLoader.JAVASCRIPT.getLanguage());
		if(ourDoingSmartCodeCompleteAction)
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
