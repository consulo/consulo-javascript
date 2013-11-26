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
