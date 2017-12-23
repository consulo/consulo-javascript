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

package com.intellij.lang.javascript.surroundWith;

import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;

/**
 * Created by IntelliJ IDEA.
 * User: yole
 * Date: 12.07.2005
 * Time: 19:29:10
 * To change this template use File | Settings | File Templates.
 */
public class JSWithTryCatchSurrounder extends JSWithTryCatchFinallySurrounder
{
	@Override
	public String getTemplateDescription()
	{
		return JavaScriptBundle.message("javascript.surround.with.try.catch");
	}

	@Override
	protected String getStatementTemplate(final Project project, PsiElement context)
	{
		return "try { } catch(e" + getExceptionVarTypeBasedOnContext(context) + ") { }";
	}
}
