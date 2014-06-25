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

package com.intellij.lang.javascript.psi;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.impl.JSExpressionCodeFragmentImpl;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;

/**
 * @author nik
 */
public class JSElementFactory
{
	private JSElementFactory()
	{
	}

	@NotNull
	public static JSFile createExpressionCodeFragment(@NotNull Project project, CharSequence text, PsiElement context, boolean isPhysical)
	{
		@NonNls String name = "fragment." + JavaScriptSupportLoader.JAVASCRIPT.getDefaultExtension();
		JSExpressionCodeFragmentImpl codeFragment = new JSExpressionCodeFragmentImpl(project, name, text, isPhysical);
		codeFragment.setContext(context);
		return codeFragment;
	}

}
