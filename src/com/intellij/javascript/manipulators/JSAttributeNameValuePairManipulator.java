/*
 * Copyright (c) 2000-2005 by JetBrains s.r.o. All Rights Reserved.
 * Use is subject to license terms.
 */
package com.intellij.javascript.manipulators;

import org.jetbrains.annotations.NonNls;
import com.intellij.lang.javascript.JSLanguageDialect;
import com.intellij.lang.javascript.psi.JSAttributeNameValuePair;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;

/**
 * @author peter
 */
public class JSAttributeNameValuePairManipulator extends JSAbstractElementManipulator<JSAttributeNameValuePair>
{
	@Override
	protected JSAttributeNameValuePair createTree(final String newText, final JSLanguageDialect languageDialect, final Project project)
	{
		@NonNls String ToCreate = "[A(" + newText + ")] class C {}";
		final PsiElement element = JSChangeUtil.createStatementFromText(project, ToCreate, languageDialect).getPsi();
		return ((JSClass) element).getAttributeList().getAttributes()[0].getValues()[0];
	}
}
