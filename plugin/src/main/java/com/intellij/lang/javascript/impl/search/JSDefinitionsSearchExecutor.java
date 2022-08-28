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

package com.intellij.lang.javascript.impl.search;

import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.search.JSClassSearch;
import consulo.application.util.function.Processor;
import consulo.application.util.query.QueryExecutor;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.PsiReference;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.language.psi.search.DefinitionsScopedSearch;
import consulo.language.psi.search.ReferencesSearch;

import javax.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 *         Date: Apr 28, 2008
 *         Time: 8:34:30 PM
 */
class JSDefinitionsSearchExecutor implements QueryExecutor<PsiElement, DefinitionsScopedSearch.SearchParameters>
{
	@Override
	public boolean execute(@Nonnull final DefinitionsScopedSearch.SearchParameters parameters, @Nonnull final Processor<? super PsiElement> consumer)
	{
		final PsiElement sourceElement = parameters.getElement();
		if(sourceElement instanceof PsiNamedElement && sourceElement.getLanguage().isKindOf(JavaScriptLanguage.INSTANCE))
		{
			ReferencesSearch.search(sourceElement, GlobalSearchScope.projectScope(sourceElement.getProject())).forEach(new Processor<PsiReference>()
			{
				@Override
				public boolean process(final PsiReference t)
				{
					if(t instanceof JSReferenceExpression)
					{
						final PsiElement parent = ((JSReferenceExpression) t).getParent();
						final ResolveResult[] resolveResults = ((JSReferenceExpression) t).multiResolve(true);

						for(ResolveResult r : resolveResults)
						{
							PsiElement psiElement = r.getElement();

							if(psiElement != null &&
									!JavaScriptIndex.isFromPredefinedFile(psiElement.getContainingFile()) &&
									sourceElement != psiElement)
							{
								if((psiElement instanceof JSFunction) && sourceElement instanceof JSFunction)
								{
									JSFunction fun = (JSFunction) psiElement;
									JSFunction sourceFun = (JSFunction) sourceElement;

									if((sourceFun.isGetProperty() && fun.isSetProperty()) || (sourceFun.isSetProperty() && fun.isGetProperty()))
									{
										return true;
									}
								}

								if((psiElement != sourceElement || !(psiElement instanceof JSClass)) && !consumer.process(psiElement))
								{
									return false;
								}
							}
						}

						if(!(parent instanceof JSDefinitionExpression))
						{
							return false;
						}
					}
					return true;
				}
			});

			if(sourceElement instanceof JSClass)
			{
				final JSClass clazz = (JSClass) sourceElement;

				final Processor<JSClass> delegatingProcessor = new Processor<JSClass>()
				{
					@Override
					public boolean process(final JSClass jsClass)
					{
						return consumer.process(jsClass);
					}
				};
				JSClassSearch.searchClassInheritors(clazz, true).forEach(delegatingProcessor);

				if(clazz.isInterface())
				{
					JSClassSearch.searchInterfaceImplementations(clazz, true).forEach(delegatingProcessor);
				}
			}
			else if(sourceElement instanceof JSFunction)
			{
				final JSFunction baseFunction = (JSFunction) sourceElement;
				final Processor<JSFunction> delegatingProcessor = new Processor<JSFunction>()
				{
					@Override
					public boolean process(final JSFunction jsFunction)
					{
						return consumer.process(jsFunction);
					}
				};
				JSFunctionsSearch.searchOverridingFunctions(baseFunction, true).forEach(delegatingProcessor);

				final PsiElement parent = baseFunction.getParent();
				if(parent instanceof JSClass && ((JSClass) parent).isInterface())
				{
					JSFunctionsSearch.searchImplementingFunctions(baseFunction, true).forEach(delegatingProcessor);
				}
			}
		}
		return true;
	}
}
