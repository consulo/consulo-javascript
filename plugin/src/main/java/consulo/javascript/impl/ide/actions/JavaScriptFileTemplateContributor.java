package consulo.javascript.impl.ide.actions;

import consulo.annotation.component.ExtensionImpl;
import consulo.fileTemplate.FileTemplateContributor;
import consulo.fileTemplate.FileTemplateRegistrator;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
@ExtensionImpl
public class JavaScriptFileTemplateContributor implements FileTemplateContributor
{
	@Override
	public void register(@Nonnull FileTemplateRegistrator fileTemplateRegistrator)
	{
		fileTemplateRegistrator.registerInternalTemplate("JavaScriptFile");
	}
}
