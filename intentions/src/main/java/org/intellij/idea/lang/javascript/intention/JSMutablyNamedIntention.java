/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.intention;

import consulo.codeEditor.Editor;
import consulo.language.psi.PsiElement;
import consulo.project.Project;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public abstract class JSMutablyNamedIntention extends JSIntention
{
	private String myText;

	protected abstract String getTextForElement(PsiElement element);

	@Override
	@Nonnull
	public String getText()
	{
		if(myText != null)
		{
			return myText;
		}
		return super.getText();
	}

	@Override
	public boolean isAvailable(@Nonnull Project project, Editor editor, @Nullable PsiElement node)
	{
		final PsiElement element = findMatchingElement(node);
		if(element != null)
		{
			myText = getTextForElement(element);
			return true;
		}
		else
		{
			return false;
		}
	}
}
