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

import javax.annotation.Nonnull;

import consulo.language.editor.completion.lookup.LookupElement;
import consulo.language.editor.template.Result;
import consulo.language.editor.template.TextResult;
import org.jetbrains.annotations.NonNls;
import consulo.language.editor.template.Expression;
import consulo.language.editor.template.ExpressionContext;
import consulo.language.editor.template.macro.Macro;
import consulo.javascript.language.JavaScriptBundle;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.project.Project;
import consulo.language.psi.PsiDocumentManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;

public class JSClassNameMacro extends Macro
{
	@Override
	@NonNls
	public String getName()
	{
		return "jsClassName";
	}

	@Override
	public String getPresentableName()
	{
		return JavaScriptBundle.message("js.classname.macro.description");
	}

	@Override
	@NonNls
	public String getDefaultValue()
	{
		return "";
	}

	@Override
	public Result calculateResult(@Nonnull final Expression[] params, final ExpressionContext context)
	{
		final PsiElement elementAtCaret = findElementAtCaret(context);
		final JSResolveUtil.ContextResolver resolver = new JSResolveUtil.ContextResolver(elementAtCaret);

		String text = resolver.getQualifierAsString();
		if(text == null)
		{
			final JSFunction previousFunction = PsiTreeUtil.getPrevSiblingOfType(elementAtCaret, JSFunction.class);

			if(previousFunction != null)
			{
				text = previousFunction.getName();
			}
		}

		if(text != null)
		{
			return new TextResult(text);
		}

		return null;
	}

	public static PsiElement findElementAtCaret(final ExpressionContext context)
	{
		Project project = context.getProject();
		int templateStartOffset = context.getTemplateStartOffset();
		int offset = templateStartOffset > 0 ? context.getTemplateStartOffset() - 1 : context.getTemplateStartOffset();

		PsiDocumentManager.getInstance(project).commitAllDocuments();

		PsiFile file = PsiDocumentManager.getInstance(project).getPsiFile(context.getEditor().getDocument());
		return file.findElementAt(offset);
	}

	@Override
	public Result calculateQuickResult(@Nonnull final Expression[] params, final ExpressionContext context)
	{
		return null;
	}

	@Override
	public LookupElement[] calculateLookupItems(@Nonnull final Expression[] params, final ExpressionContext context)
	{
		return null;
	}
}
