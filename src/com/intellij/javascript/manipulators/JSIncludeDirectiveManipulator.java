/*
 * Copyright (c) 2000-2005 by JetBrains s.r.o. All Rights Reserved.
 * Use is subject to license terms.
 */
package com.intellij.javascript.manipulators;

import com.intellij.lang.javascript.JSLanguageDialect;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.impl.JSIncludeDirectiveImpl;
import com.intellij.openapi.project.Project;

/**
 * @author peter
 */
public class JSIncludeDirectiveManipulator extends JSAbstractElementManipulator<JSIncludeDirectiveImpl>
{
	protected JSIncludeDirectiveImpl createTree(final String newText, final JSLanguageDialect languageDialect, final Project project)
	{
		return (JSIncludeDirectiveImpl) JSChangeUtil.createStatementFromText(project, newText, languageDialect).getPsi();
	}
}
