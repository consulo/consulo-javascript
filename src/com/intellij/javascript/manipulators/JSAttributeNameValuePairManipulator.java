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

import org.jetbrains.annotations.NonNls;
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
	protected JSAttributeNameValuePair createTree(final String newText, final Project project)
	{
		@NonNls String ToCreate = "[A(" + newText + ")] class C {}";
		final PsiElement element = JSChangeUtil.createStatementFromText(project, ToCreate).getPsi();
		return ((JSClass) element).getAttributeList().getAttributes()[0].getValues()[0];
	}
}
