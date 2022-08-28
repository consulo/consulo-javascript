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

package com.intellij.lang.javascript.impl.inspections;

import com.intellij.javascript.documentation.JSDocumentationUtils;
import consulo.javascript.language.JavaScriptBundle;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.editor.inspection.ProblemsHolder;
import consulo.language.psi.PsiElement;
import consulo.language.psi.ResolveResult;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

/**
 * @by Maxim.Mossienko
 */
@ExtensionImpl
public class JSDeprecatedSymbolsInspection extends JSInspection
{
	@NonNls
	private static final String SHORT_NAME = "JSDeprecatedSymbols";

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return "General";
	}

	@Override
	@Nonnull
	public String getDisplayName()
	{
		return JavaScriptBundle.message("js.deprecated.symbols.inspection.name");
	}

	@Override
	@Nonnull
	@NonNls
	public String getShortName()
	{
		return SHORT_NAME;
	}

	@Override
	protected JSElementVisitor createVisitor(final ProblemsHolder holder)
	{
		return new JSElementVisitor()
		{
			@Override
			public void visitJSReferenceExpression(final JSReferenceExpression node)
			{
				for(ResolveResult r : node.multiResolve(false))
				{
					final PsiElement element = r.getElement();
					if((element instanceof JSDefinitionExpression && element.getParent() instanceof JSAssignmentExpression) || element == node.getParent())
					{
						continue;
					}
					if(JSDocumentationUtils.isDeprecated(element))
					{
						holder.registerProblem(node.getReferenceNameElement(), JavaScriptBundle.message("javascript.deprecated.symbol.used.name.message"),
								ProblemHighlightType.LIKE_DEPRECATED);
						break;
					}
				}
			}
		};
	}
}