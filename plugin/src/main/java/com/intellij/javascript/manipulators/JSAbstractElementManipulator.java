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

import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.AbstractElementManipulator;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;

/**
 * @author peter
 */
abstract class JSAbstractElementManipulator<T extends PsiElement> extends AbstractElementManipulator<T>
{
	@Override
	public T handleContentChange(final T element, final TextRange range, String newContent) throws IncorrectOperationException
	{
		String oldText = element.getText();
		newContent = StringUtil.escapeStringCharacters(newContent);
		String newText = oldText.substring(0, range.getStartOffset()) + newContent + oldText.substring(range.getEndOffset());

		return (T) element.replace(createTree(newText, element.getProject()));
	}

	protected abstract T createTree(final String newText, final Project project);
}
