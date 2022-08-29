package consulo.javascript.impl.ide.actions;

import consulo.fileTemplate.FileTemplateContributor;
import consulo.fileTemplate.FileTemplateRegistrator;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
public class JavaScriptFileTemplateContributor implements FileTemplateContributor
{
	@Override
	public void register(@Nonnull FileTemplateRegistrator fileTemplateRegistrator)
	{
		fileTemplateRegistrator.registerInternalTemplate("JavaScriptFile");
	}
}
