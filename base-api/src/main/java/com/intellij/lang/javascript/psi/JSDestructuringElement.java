package com.intellij.lang.javascript.psi;

import consulo.annotation.access.RequiredReadAction;
import jakarta.annotation.Nonnull;

import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public interface JSDestructuringElement extends JSElement
{
	@Nonnull
	@RequiredReadAction
	default JSVariable[] getVariables()
	{
		JSDestructuringObject object = getDestructuringObject();
		return object == null ? JSVariable.EMPTY_ARRAY : object.getVariables();
	}

	@Nullable
	@RequiredReadAction
	JSDestructuringObject getDestructuringObject();
}
