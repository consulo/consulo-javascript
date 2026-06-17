package com.intellij.lang.javascript;

import consulo.annotation.component.ExtensionImpl;
import consulo.language.LanguageRegistry;
import consulo.language.psi.PsiElement;
import consulo.xml.language.psi.XmlText;
import jakarta.inject.Inject;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
@ExtensionImpl
public class XmlTextJSInjector extends JSLanguageInjector {
    @Inject
    public XmlTextJSInjector(LanguageRegistry languageRegistry) {
        super(languageRegistry);
    }

    @Override
    public Class<? extends PsiElement> getElementClass() {
        return XmlText.class;
    }
}
