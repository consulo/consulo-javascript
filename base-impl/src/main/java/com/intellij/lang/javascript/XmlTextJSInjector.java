package com.intellij.lang.javascript;

import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import consulo.xml.psi.xml.XmlText;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
@ExtensionImpl
public class XmlTextJSInjector extends JSLanguageInjector {
    @Nonnull
    @Override
    public Class<? extends PsiElement> getElementClass() {
        return XmlText.class;
    }
}
