package consulo.javascript.impl.ide.navigationToolbar;

import consulo.annotation.component.ExtensionImpl;
import consulo.ide.navigationToolbar.StructureAwareNavBarModelExtension;
import consulo.language.Language;
import com.intellij.lang.javascript.impl.structureView.JSStructureItemPresentation;
import consulo.language.psi.PsiElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.JavaScriptLanguage;
import jakarta.annotation.Nonnull;

import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 17/01/2021
 */
@ExtensionImpl
public class JavaScriptNavBarModelExtension extends StructureAwareNavBarModelExtension {
    @Nonnull
    @Override
    protected Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public String getPresentableText(Object object) {
        return object instanceof PsiElement ? JSStructureItemPresentation.getName((PsiElement)object) : null;
    }
}
