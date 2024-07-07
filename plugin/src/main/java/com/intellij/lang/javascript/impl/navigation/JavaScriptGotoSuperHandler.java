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

/*
 * @author max
 */
package com.intellij.lang.javascript.impl.navigation;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.component.ExtensionImpl;
import consulo.codeEditor.Editor;
import consulo.codeEditor.EditorPopupHelper;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.action.GotoSuperActionHander;
import consulo.language.editor.ui.PopupNavigationUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.navigation.NavigationItem;
import consulo.project.Project;
import consulo.ui.ex.popup.JBPopup;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class JavaScriptGotoSuperHandler implements GotoSuperActionHander
{
	@Override
	public void invoke(final Project project, final Editor editor, final PsiFile file)
	{
		final PsiElement at = file.findElementAt(editor.getCaretModel().getOffset());
		if(at == null)
		{
			return;
		}
		JSNamedElement namedElement = PsiTreeUtil.getParentOfType(at, JSNamedElement.class);
		PsiElement parent = namedElement != null ? namedElement.getParent() : null;

		if(namedElement instanceof JSDefinitionExpression)
		{
			if(parent instanceof JSAssignmentExpression)
			{
				PsiElement rOperand = ((JSAssignmentExpression) parent).getROperand();
				if(rOperand instanceof JSFunctionExpression)
				{
					namedElement = (JSNamedElement) rOperand;
				}
			}
		}

		if(namedElement instanceof JSFunction)
		{
			final JSFunction function = (JSFunction) namedElement;
			final String qName = JSResolveUtil.getQNameToStartHierarchySearch(function);

			if(qName != null)
			{
				if(parent instanceof JSFile)
				{
					JSClass xmlBackedClass = JSResolveUtil.getXmlBackedClass((JSFile) parent);
					if(xmlBackedClass != null)
					{
						parent = xmlBackedClass;
					}
				}
				boolean result = JSResolveUtil.iterateType(function, parent instanceof JSClass ? parent : parent.getContainingFile(), qName,
						new JSResolveUtil.OverrideHandler()
				{
					@Override
					public boolean process(final ResolveProcessor processor, final PsiElement scope, final String className)
					{
						((NavigationItem) processor.getResult()).navigate(true);
						return false;
					}
				});

				if(!result)
				{
					return;
				}
			}

			if(parent instanceof JSClass)
			{
				JSResolveUtil.processInterfaceMethods((JSClass) parent, new JSResolveUtil.CollectMethodsToImplementProcessor(function.getName(), function)
				{
					@Override
					protected boolean process(final ResolveProcessor processor)
					{
						((NavigationItem) processor.getResult()).navigate(true);
						return true;
					}
				});
			}
		}
		else if(namedElement instanceof JSClass)
		{
			final JSClass clazz = (JSClass) namedElement;
			final JSClass[] classes = clazz.getSupers();

			if(classes.length == 0)
			{
				return;
			}
			if(classes.length == 1)
			{
				classes[0].navigate(true);
			}
			else
			{
				JBPopup psiElementPopup = PopupNavigationUtil.getPsiElementPopup(classes, "Choose super class or interface");

				EditorPopupHelper.getInstance().showPopupInBestPositionFor(editor, psiElementPopup);
			}
		}
	}

	@Override
	public boolean startInWriteAction()
	{
		return false;
	}

	@Override
	public boolean isValidFor(final Editor editor, final PsiFile file)
	{
		return true;
	}

	@Nonnull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}
}
