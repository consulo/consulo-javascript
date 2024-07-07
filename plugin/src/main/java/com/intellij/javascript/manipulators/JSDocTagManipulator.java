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
package com.intellij.javascript.manipulators;

import com.intellij.lang.javascript.psi.JSDocComment;
import com.intellij.lang.javascript.psi.JSDocTag;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class JSDocTagManipulator extends JSAbstractElementManipulator<JSDocTag>
{
	@Override
	protected JSDocTag createTree(final String newText, final Project project)
	{
		String ToCreate = "/** " + newText + " */";
		final PsiElement element = JSChangeUtil.createJSTreeFromText(project, ToCreate).getPsi();
		return ((JSDocComment) element).getTags()[0];
	}

	@Nonnull
	@Override
	public Class<JSDocTag> getElementClass()
	{
		return JSDocTag.class;
	}
}