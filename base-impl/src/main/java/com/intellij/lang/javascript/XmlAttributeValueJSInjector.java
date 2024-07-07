package com.intellij.lang.javascript;

import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import consulo.xml.psi.xml.XmlAttributeValue;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
@ExtensionImpl
public class XmlAttributeValueJSInjector extends JSLanguageInjector
{
	@Nonnull
	@Override
	public Class<? extends PsiElement> getElementClass()
	{
		return XmlAttributeValue.class;
	}
}
