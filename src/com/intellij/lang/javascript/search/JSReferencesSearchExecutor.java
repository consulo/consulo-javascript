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

package com.intellij.lang.javascript.search;

import com.intellij.lang.Language;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.JavascriptLanguage;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.PsiSearchHelper;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Processor;
import com.intellij.util.QueryExecutor;

/**
 * @author Maxim.Mossienko
 *         Date: Apr 28, 2008
 *         Time: 8:34:20 PM
 */
class JSReferencesSearchExecutor implements QueryExecutor<PsiReference, ReferencesSearch.SearchParameters>
{
	@Override
	public boolean execute(final ReferencesSearch.SearchParameters queryParameters, final Processor<PsiReference> consumer)
	{
		final PsiElement sourceElement = queryParameters.getElementToSearch();

		if(sourceElement instanceof PsiNamedElement &&
				sourceElement.getLanguage().isKindOf(Language.findInstance(JavascriptLanguage.class)) &&
				queryParameters.getScope() instanceof GlobalSearchScope &&
				!(sourceElement.getUseScope() instanceof LocalSearchScope))
		{
			final String s = ((PsiNamedElement) sourceElement).getName();
			if(s == null)
			{
				return true;
			}
			PsiSearchHelper.SERVICE.getInstance(sourceElement.getProject()).processAllFilesWithWordInLiterals(s,
					GlobalSearchScope.getScopeRestrictedByFileTypes(GlobalSearchScope.projectScope(sourceElement.getProject()),
							JavaScriptSupportLoader.JAVASCRIPT), new Processor<PsiFile>()
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
