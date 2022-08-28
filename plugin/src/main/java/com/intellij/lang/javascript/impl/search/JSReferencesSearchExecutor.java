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

import consulo.javascript.language.JavaScriptFileType;
import consulo.application.util.query.QueryExecutor;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.language.psi.scope.LocalSearchScope;
import consulo.language.psi.search.ReferencesSearch;
import consulo.application.util.function.Processor;
import consulo.application.ReadAction;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.psi.PsiReference;
import consulo.language.psi.search.PsiSearchHelper;

/**
 * @author Maxim.Mossienko
 *         Date: Apr 28, 2008
 *         Time: 8:34:20 PM
 */
public class JSReferencesSearchExecutor implements QueryExecutor<PsiReference, ReferencesSearch.SearchParameters>
{
	@Override
	public boolean execute(final ReferencesSearch.SearchParameters queryParameters, final Processor<? super PsiReference> consumer)
	{
		final PsiElement sourceElement = queryParameters.getElementToSearch();

		if(sourceElement instanceof PsiNamedElement &&
				sourceElement.getLanguage().isKindOf(JavaScriptLanguage.INSTANCE) &&
				queryParameters.getScope() instanceof GlobalSearchScope &&
				!(sourceElement.getUseScope() instanceof LocalSearchScope))
		{
			final String s = ReadAction.compute(() -> ((PsiNamedElement) sourceElement).getName());
			if(s == null)
			{
				return true;
			}
			PsiSearchHelper.SERVICE.getInstance(sourceElement.getProject()).processAllFilesWithWordInLiterals(s, GlobalSearchScope.getScopeRestrictedByFileTypes(GlobalSearchScope.projectScope
					(sourceElement.getProject()), JavaScriptFileType.INSTANCE), new Processor<PsiFile>()
			{
				@Override
				public boolean process(final PsiFile psiFile)
				{
					/*if(psiFile.getLanguage() == JavaScriptSupportLoader.JSON)
					{
						ReferencesSearch.search(sourceElement, new LocalSearchScope(psiFile)).forEach(consumer);
					}         */
					return true;
				}
			});
		}
		return true;
	}
}
