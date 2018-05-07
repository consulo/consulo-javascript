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

package com.intellij.lang.javascript.index;

import javax.swing.Icon;

import javax.annotation.Nullable;
import consulo.annotations.RequiredDispatchThread;
import consulo.awt.TargetAWT;
import consulo.ide.IconDescriptorUpdaters;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.util.Iconable;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;

/**
 * @author Maxim.Mossienko
 * @since 9:55:49 PM Apr 7, 2006
 */
public class JSItemPresentation implements ItemPresentation
{
	private JSNamedElement myElement;

	public JSItemPresentation(final JSNamedElement elementProxy)
	{
		this.myElement = elementProxy;
	}

	@Override
	public String getPresentableText()
	{
		return myElement.getName();
	}

	@Override
	@Nullable
	public String getLocationString()
	{
		final PsiFile psiFile = myElement.getContainingFile();
		if(myElement instanceof JSVariable || myElement instanceof JSFunction)
		{
			PsiElement possibleClazz = JSResolveUtil.findParent(myElement);

			if(possibleClazz instanceof JSClass)
			{
				final StringBuilder presentation = new StringBuilder();

				presentation.append(((JSClass) possibleClazz).getQualifiedName());
				presentation.append('(').append(getFileName(psiFile)).append(')');
				return presentation.toString();
			}
		}
		else if(myElement instanceof JSClass)
		{
			final String s = ((JSClass) myElement).getQualifiedName();
			final int i = s.lastIndexOf('.');

			if(i != -1)
			{
				final StringBuilder presentation = new StringBuilder();

				presentation.append(s.substring(0, i));
				presentation.append('(').append(getFileName(psiFile)).append(')');
				return presentation.toString();
			}
		}
		return getFileName(psiFile);
	}

	private static String getFileName(final PsiFile psiFile)
	{
		final String s = psiFile.getName();
		if(JSResolveUtil.isPredefinedFile(psiFile))
		{
			return s.substring(s.lastIndexOf('/') + 1);
		}
		return s;
	}

	@Override
	@Nullable
	@RequiredDispatchThread
	public Icon getIcon(boolean open)
	{
		return TargetAWT.to(IconDescriptorUpdaters.getIcon(myElement, Iconable.ICON_FLAG_VISIBILITY));
	}
}
