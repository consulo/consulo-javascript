package com.intellij.lang.javascript.psi;

import consulo.annotation.access.RequiredReadAction;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 11/12/2021
 */
public interface JSClassExpression extends JSExpression
{
	@Nonnull
	@RequiredReadAction
	JSClass getClassElement();
}
