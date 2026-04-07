package com.intellij.lang.javascript;

import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import consulo.xml.language.psi.XmlAttributeValue;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
@ExtensionImpl
public class XmlAttributeValueJSInjector extends JSLanguageInjector {
    @Override
    public Class<? extends PsiElement> getElementClass() {
        return XmlAttributeValue.class;
    }
}
