package com.intellij.lang.javascript.psi;

import consulo.annotation.access.RequiredReadAction;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public interface JSDestructuringShorthandedProperty extends JSElement
{
	@RequiredReadAction
	@Nonnull
	JSVariable getVarialbe();
}
