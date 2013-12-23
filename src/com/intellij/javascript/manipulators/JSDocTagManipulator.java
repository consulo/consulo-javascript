/*
 * Copyright (c) 2000-2005 by JetBrains s.r.o. All Rights Reserved.
 * Use is subject to license terms.
 */
package com.intellij.javascript.manipulators;

import org.jetbrains.annotations.NonNls;
import com.intellij.lang.javascript.JSLanguageDialect;
import com.intellij.lang.javascript.psi.JSDocComment;
import com.intellij.lang.javascript.psi.JSDocTag;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;

public class JSDocTagManipulator extends JSAbstractElementManipulator<JSDocTag>
{
	@Override
	protected JSDocTag createTree(final String newText, final JSLanguageDialect languageDialect, final Project project)
	{
		@NonNls String ToCreate = "/** " + newText + " */";
		final PsiElement element = JSChangeUtil.createJSTreeFromText(project, ToCreate, languageDialect).getPsi();
		return ((JSDocComment) element).getTags()[0];
	}
}