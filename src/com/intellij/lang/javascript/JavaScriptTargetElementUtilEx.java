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

package com.intellij.lang.javascript;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.psi.PsiElement;
import consulo.codeInsight.TargetElementUtilEx;
import consulo.extensions.CompositeExtensionPointName;

/**
 * @author Maxim.Mossienko
 *         Date: Oct 13, 2008
 *         Time: 3:53:42 PM
 */
public class JavaScriptTargetElementUtilEx extends TargetElementUtilEx.Adapter
{
	@Override
	@CompositeExtensionPointName.BooleanBreakResult(breakValue = false)
	public boolean includeSelfInGotoImplementation(@NotNull final PsiElement element)
	{
		if(element instanceof JSFunction)
		{
			final PsiElement parent = JSResolveUtil.findParent(element);
			if(parent instanceof JSClass && ((JSClass) parent).isInterface())
			{
				return false;
			}
		}
		else if(element instanceof JSClass)
		{
			return false;
		}
		return true;
	}
}
