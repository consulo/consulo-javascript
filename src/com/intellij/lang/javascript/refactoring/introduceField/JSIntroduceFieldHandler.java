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

package com.intellij.lang.javascript.refactoring.introduceField;

import org.jetbrains.annotations.NonNls;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JSLanguageDialect;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.refactoring.JSBaseIntroduceHandler;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlFile;
import com.intellij.refactoring.util.CommonRefactoringUtil;
import com.intellij.util.IncorrectOperationException;

/**
 * @author Maxim.Mossienko
 *         Date: May 29, 2008
 *         Time: 8:20:03 PM
 */
public class JSIntroduceFieldHandler extends JSBaseIntroduceHandler<JSElement, JSIntroduceFieldSettings, JSIntroduceFieldDialog>
{
	@Override
	protected String getRefactoringName()
	{
		return JSBundle.message("javascript.introduce.field.title");
	}

	@Override
	protected String getCannotIntroduceMessagePropertyKey()
	{
		return "javascript.introduce.field.error.no.expression.selected";
	}

	@Override
	protected JSIntroduceFieldDialog createDialog(final Project project, final JSExpression expression, final JSExpression[] occurrences)
	{
		return new JSIntroduceFieldDialog(project, occurrences, expression);
	}

	@Override
	protected JSElement findAnchor(final BaseIntroduceContext<JSIntroduceFieldSettings> context, final boolean replaceAllOccurences)
	{
		return findClassAnchor(context.expression);
	}

	@Override
	protected JSElement addStatementBefore(final JSElement anchorStatement, final JSVarStatement declaration) throws IncorrectOperationException
	{
		return addToClassAnchor(anchorStatement, declaration);
	}

	@Override
	protected String getDeclText(final JSIntroduceFieldSettings settings)
	{
		@NonNls String baseDeclText = super.getDeclText(settings);
		final JSAttributeList.AccessType type = settings.getAccessType();
		if(type != JSAttributeList.AccessType.PACKAGE_LOCAL)
		{
			baseDeclText = type.toString().toLowerCase() + " " + baseDeclText;
		}

		return baseDeclText;
	}

	@Override
	protected JSExpression findIntroducedExpression(final PsiFile file, final int start, final int end, Editor editor)
	{
		if(file.getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4)
		{
			CommonRefactoringUtil.showErrorHint(file.getProject(), editor, JSBundle.message("javascript.introduce.field.error.not.available.in.javascript" +
					".code"), getRefactoringName(), null);
			return null;
		}

		return super.findIntroducedExpression(file, start, end, editor);
	}

	@Override
	protected JSVarStatement prepareDeclaration(final String varDeclText, BaseIntroduceContext<JSIntroduceFieldSettings> context,
			final Project project, final JSLanguageDialect languageDialect) throws IncorrectOperationException
	{
		final JSIntroduceFieldSettings.InitializationPlace place = context.settings.getInitializationPlace();

		if(place == JSIntroduceFieldSettings.InitializationPlace.FieldDeclaration)
		{
			return super.prepareDeclaration(varDeclText, context, project, languageDialect);
		}
		else
		{
			final String assignmentText = context.settings.getVariableName() + "=" + context.expression.getText() + JSChangeUtil.getSemicolon(project);
			final PsiElement psiToInsert = JSChangeUtil.createStatementFromText(project, assignmentText, languageDialect).getPsi();

			if(place == JSIntroduceFieldSettings.InitializationPlace.CurrentMethod)
			{
				final JSElement element = super.findAnchor(context, context.settings.isReplaceAllOccurences());
				final PsiElement parent = element.getParent();
				final PsiElement addedElement = parent.addBefore(psiToInsert, element);
				CodeStyleManager.getInstance(project).reformatNewlyAddedElement(parent.getNode(), addedElement.getNode());
			}
			else
			{
				PsiElement parent = PsiTreeUtil.getParentOfType(context.expression, JSClass.class, JSFile.class);
				if(parent instanceof JSFile)
				{
					final PsiFile containingFile = parent.getContext().getContainingFile();
					assert containingFile instanceof XmlFile;
					parent = XmlBackedJSClassImpl.getXmlBackedClass((XmlFile) containingFile);
				}

				assert parent instanceof JSClass;
				final JSClass clazz = (JSClass) parent;
				JSFunction fun = clazz.findFunctionByName(clazz.getName());

				if(fun == null)
				{
					@NonNls String constr = "function " + clazz.getName() + "() {}";
					if(clazz.getAttributeList() != null && clazz.getAttributeList().getAccessType() == JSAttributeList.AccessType.PUBLIC)
					{
						constr = "public " + constr;
					}

					fun = (JSFunction) clazz.add(JSChangeUtil.createJSTreeFromText(project, constr, languageDialect).getPsi());
				}

				fun.getBody()[0].add(psiToInsert);
			}

			return (JSVarStatement) JSChangeUtil.createStatementFromText(project, varDeclText + JSChangeUtil.getSemicolon(project), languageDialect).getPsi();
		}
	}
}