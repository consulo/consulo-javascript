package consulo.javascript.jsx.language.psi.impl;

import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReferenceBase;

import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
public class JSXmlTagReference extends PsiReferenceBase<PsiElement> {
    public JSXmlTagReference(PsiElement element) {
        super(element);
    }

    @RequiredReadAction
    @Nullable
    @Override
    public PsiElement resolve() {
        return null;
    }

    @RequiredReadAction
    @Override
    public Object[] getVariants() {
        return new Object[0];
    }
}
