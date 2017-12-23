/*
 * Copyright 2013-2016 must-be.org
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

package consulo.javascript.ide.codeInsight;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.inspections.qucikFixes.CreateJSFunctionOrMethodFix;
import com.intellij.openapi.util.KeyedExtensionCollector;
import com.intellij.psi.PsiElement;
import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.lang.LanguageVersion;

/**
 * @author VISTALL
 * @since 23.02.2016
 */
public class JavaScriptQuickFixFactory
{
	private static final JavaScriptQuickFixFactory ourDefaultImpl = new JavaScriptQuickFixFactory();

	@NotNull
	public static JavaScriptQuickFixFactory byElement(PsiElement element)
	{
		LanguageVersion languageVersion = element.getLanguageVersion();
		if(languageVersion instanceof BaseJavaScriptLanguageVersion)
		{
			JavaScriptQuickFixFactory quickFixFactory = EP.findSingle(languageVersion.getName());
			if(quickFixFactory != null)
			{
				return quickFixFactory;
			}
		}
		return ourDefaultImpl;
	}

	public static final KeyedExtensionCollector<JavaScriptQuickFixFactory, String> EP = new KeyedExtensionCollector<>("consulo.javascript.quickFixFactory");

	public CreateJSFunctionOrMethodFix createFunctionOrMethodFix(String referenceName, boolean isMethod)
	{
		return new CreateJSFunctionOrMethodFix(referenceName, isMethod);
	}
}
