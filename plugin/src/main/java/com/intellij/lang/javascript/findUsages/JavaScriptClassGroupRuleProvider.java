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

package com.intellij.lang.javascript.findUsages;

import org.jetbrains.annotations.NotNull;
import com.intellij.icons.AllIcons;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.usages.UsageGroup;

/**
 * @author Maxim.Mossienko
 */
public class JavaScriptClassGroupRuleProvider extends JavaScriptGroupRuleProviderBase<JSClass>
{
	@Override
	protected Class<? extends JSNamedElement> getUsageClass()
	{
		return JSClass.class;
	}

	@Override
	protected UsageGroup createUsageGroup(final JSClass clazz)
	{
		return new ClassUsageGroup(clazz);
	}

	@Override
	protected boolean isAcceptableElement(JSNamedElement element)
	{
		return super.isAcceptableElement(element) && !JSResolveUtil.isArtificialClassUsedForReferenceList((JSClass) element);
	}

	private static class ClassUsageGroup extends JavaScriptGroupRuleProviderBase.PsiNamedElementUsageGroupBase<JSClass>
	{
		public ClassUsageGroup(@NotNull JSClass clazz)
		{
			super(clazz, AllIcons.Nodes.Class);
		}
	}
}