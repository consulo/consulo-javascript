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

package com.intellij.lang.javascript.psi.util;

import org.jetbrains.annotations.Nullable;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.ide.IconDescriptorUpdaters;
import com.intellij.psi.PsiElement;

/**
 * Created by IntelliJ IDEA.
 * User: maxim.mossienko
 * Date: Dec 6, 2005
 * Time: 8:35:58 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSLookupUtil
{

	@Nullable
	public static Object createPrioritizedLookupItem(PsiElement value, String name, int priority)
	{
		LookupElementBuilder builder = LookupElementBuilder.create(name);
		builder = builder.withTypeText(value.getContainingFile().getName(), true);
		builder = builder.withIcon(IconDescriptorUpdaters.getIcon(value, 0));
		return builder;
	}
}
