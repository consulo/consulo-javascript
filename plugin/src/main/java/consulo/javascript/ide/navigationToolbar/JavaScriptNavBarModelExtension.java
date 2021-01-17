package consulo.javascript.ide.navigationToolbar;

import com.intellij.ide.navigationToolbar.StructureAwareNavBarModelExtension;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.structureView.JSStructureItemPresentation;
import com.intellij.psi.PsiElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.JavaScriptLanguage;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 17/01/2021
 */
public class JavaScriptNavBarModelExtension extends StructureAwareNavBarModelExtension
{
	@Nonnull
	@Override
	protected Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}

	@Nullable
	@Override
	@RequiredReadAction
	public String getPresentableText(Object object)
	{
		return object instanceof PsiElement ? JSStructureItemPresentation.getName((PsiElement) object) : null;
	}
}
