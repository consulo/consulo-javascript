/*
 * Copyright (c) 2000-2005 by JetBrains s.r.o. All Rights Reserved.
 * Use is subject to license terms.
 */
package com.intellij.lang.javascript.types;

import com.intellij.lang.Language;
import com.intellij.psi.tree.IStubFileElementType;

/**
 * @author peter
 */
public class JSFileElementType extends IStubFileElementType
{
	public static final int VERSION = 22;

	public JSFileElementType(final Language language)
	{
		super(language);
	}

	@Override
	public String getExternalId()
	{
		return getLanguage() + ":" + toString();
	}

	@Override
	public int getStubVersion()
	{
		return VERSION;
	}
}
