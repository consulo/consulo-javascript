/*
 * Copyright (c) 2000-2005 by JetBrains s.r.o. All Rights Reserved.
 * Use is subject to license terms.
 */
package com.intellij.javascript.manipulators;

import com.intellij.lang.javascript.JSLanguageDialect;
import com.intellij.lang.javascript.psi.util.JSUtils;
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
	public T handleContentChange(final T element, final TextRange range, String newContent) throws IncorrectOperationException
	{
		String oldText = element.getText();
		newContent = StringUtil.escapeStringCharacters(newContent);
		String newText = oldText.substring(0, range.getStartOffset()) + newContent + oldText.substring(range.getEndOffset());
		final JSLanguageDialect languageDialect = JSUtils.getDialect(element.getContainingFile());

		return (T) element.replace(createTree(newText, languageDialect, element.getProject()));
	}

	protected abstract T createTree(final String newText, final JSLanguageDialect languageDialect, final Project project);
}
