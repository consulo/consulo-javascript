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

package com.intellij.lang.javascript;

import org.jetbrains.annotations.NotNull;
import com.intellij.codeInsight.template.TemplateContextType;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.xml.XMLLanguage;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiLanguageInjectionHost;
import com.intellij.psi.impl.source.tree.injected.InjectedLanguageUtil;
import com.intellij.psi.util.PsiTreeUtil;

/**
 * @author Maxim.Mossienko
 *         Date: Apr 14, 2008
 *         Time: 6:01:09 PM
 */
public class JavaScriptCodeContextType extends TemplateContextType
{
	private static final String JAVA_SCRIPT = "JAVA_SCRIPT";

	public JavaScriptCodeContextType()
	{
		super(JAVA_SCRIPT, JSBundle.message("javascript.template.context.type"));
	}

	@Override
	public boolean isInContext(@NotNull final PsiFile file, final int offset)
	{
		PsiElement at = file.findElementAt(offset);
		if(at == null && offset == file.getTextLength())
		{
			at = file.findElementAt(offset - 1);
		}
		Language language = at != null ? at.getParent().getLanguage() : null;

		if(language instanceof XMLLanguage)
		{
			final PsiLanguageInjectionHost host = PsiTreeUtil.getParentOfType(at, PsiLanguageInjectionHost.class, false);

			if(host != null)
			{
				final Ref<Boolean> hasJsInjection = new Ref<Boolean>(Boolean.FALSE);

				InjectedLanguageUtil.enumerate(host, new JSResolveUtil.JSInjectedFilesVisitor()
				{
					@Override
					protected void process(final JSFile file)
					{
						hasJsInjection.set(Boolean.TRUE);
					}
				});

				if(hasJsInjection.get())
				{
					language = JavaScriptSupportLoader.JAVASCRIPT.getLanguage();
				}
			}
		}
		return language != null && language.isKindOf(JavaScriptSupportLoader.JAVASCRIPT.getLanguage());
	}

	public boolean isInContext(@NotNull final FileType fileType)
	{
		return fileType instanceof JavaScriptFileType;
	}

	@Override
	public SyntaxHighlighter createHighlighter()
	{
		return SyntaxHighlighterFactory.getSyntaxHighlighter(JavaScriptSupportLoader.ECMA_SCRIPT_L4, null, null);
	}
}
