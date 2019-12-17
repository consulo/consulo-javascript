package com.intellij.lang.javascript.psi;

import consulo.annotation.access.RequiredReadAction;

import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
public interface JSDestructuringParameter extends JSParameter
{
	@RequiredReadAction
	@Nullable
	JSDestructuringObject getDestructuringObject();
}
