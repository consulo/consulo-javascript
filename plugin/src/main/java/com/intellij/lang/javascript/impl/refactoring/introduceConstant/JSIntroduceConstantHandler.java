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

package com.intellij.lang.javascript.impl.refactoring.introduceConstant;

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.impl.refactoring.JSBaseIntroduceHandler;
import com.intellij.lang.javascript.psi.*;
import consulo.codeEditor.Editor;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.refactoring.util.CommonRefactoringUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import consulo.util.lang.ref.Ref;
import org.jetbrains.annotations.NonNls;

/**
 * @author Maxim.Mossienko
 *         Date: May 29, 2008
 *         Time: 8:20:03 PM
 */
public class JSIntroduceConstantHandler extends JSBaseIntroduceHandler<JSElement, JSIntroduceConstantSettings, JSIntroduceConstantDialog>
{
	@Override
	protected String getRefactoringName()
	{
		return JavaScriptLocalize.javascriptIntroduceConstantTitle().get();
	}

	@Override
	protected LocalizeValue getCannotIntroduceMessage()
	{
		return JavaScriptLocalize.javascriptIntroduceConstantErrorNoExpressionSelected();
	}

	@Override
	protected JSIntroduceConstantDialog createDialog(final Project project, final JSExpression expression, final JSExpression[] occurrences)
	{
		return new JSIntroduceConstantDialog(project, occurrences, expression);
	}

	@Override
	protected String getDeclText(final JSIntroduceConstantSettings settings)
	{
		@NonNls String baseDeclText = "static const " + settings.getVariableName();
		final JSAttributeList.AccessType type = settings.getAccessType();
		if (type != JSAttributeList.AccessType.PACKAGE_LOCAL)
		{
			baseDeclText = type.toString().toLowerCase() + " " + baseDeclText;
		}

		return baseDeclText;
	}

	@Override
	protected JSElement findAnchor(final BaseIntroduceContext<JSIntroduceConstantSettings> context, final boolean replaceAllOccurences)
	{
		return findClassAnchor(context.expression);
	}

	@Override
	protected JSElement addStatementBefore(final JSElement anchorStatement, final JSVarStatement declaration) throws IncorrectOperationException
	{
		return addToClassAnchor(anchorStatement, declaration);
	}

	@Override
	protected JSExpression findIntroducedExpression(final PsiFile file, final int start, final int end, Editor editor)
	{
		if (file.getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4)
		{
			CommonRefactoringUtil.showErrorHint(
				file.getProject(),
				editor,
				JavaScriptLocalize.javascriptIntroduceConstantErrorNotAvailableInJavascriptCode().get(),
				getRefactoringName(),
				null
			);
			return null;
		}

		final JSExpression expression = super.findIntroducedExpression(file, start, end, editor);
		if(expression == null)
		{
			return null;
		}

		final Ref<Boolean> hasAccesibilityProblem = new Ref<Boolean>();
		expression.accept(new JSElementVisitor()
		{
			@Override
			public void visitJSReferenceExpression(final JSReferenceExpression node)
			{
				if(node.getQualifier() == null)
				{
					final PsiElement element = node.resolve();

					if(element instanceof JSAttributeListOwner && !(element instanceof JSClass))
					{
						final JSAttributeList attributeList = ((JSAttributeListOwner) element).getAttributeList();
						if(attributeList == null || !attributeList.hasModifier(JSAttributeList.ModifierType.STATIC))
						{
							hasAccesibilityProblem.set(Boolean.TRUE);
						}
					}
					else if(element == null)
					{
						hasAccesibilityProblem.set(Boolean.TRUE);
					}
				}
				super.visitJSReferenceExpression(node);
			}

			@Override
			public void visitJSElement(final JSElement node)
			{
				node.acceptChildren(this);
			}
		});

		if(Boolean.TRUE.equals(hasAccesibilityProblem.get()))
		{
			CommonRefactoringUtil.showErrorHint(
				file.getProject(),
				editor,
				JavaScriptLocalize.javascriptIntroduceConstantErrorNotConstantExpressionSelected().get(),
				getRefactoringName(),
				null
			);
			return null;
		}
		return expression;
	}
}
