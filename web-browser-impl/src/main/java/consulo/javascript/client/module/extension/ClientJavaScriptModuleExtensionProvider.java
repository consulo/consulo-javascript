package consulo.javascript.client.module.extension;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.localize.LocalizeValue;
import consulo.module.content.layer.ModuleExtensionProvider;
import consulo.module.content.layer.ModuleRootLayer;
import consulo.module.extension.ModuleExtension;
import consulo.module.extension.MutableModuleExtension;
import consulo.ui.image.Image;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
@ExtensionImpl
public class ClientJavaScriptModuleExtensionProvider implements ModuleExtensionProvider<ClientJavaScriptModuleExtension>
{
	@Nonnull
	@Override
	public String getId()
	{
		return "client-javascript";
	}

	@Override
	public boolean isAllowMixin()
	{
		return true;
	}

	@Nonnull
	@Override
	public LocalizeValue getName()
	{
		return LocalizeValue.localizeTODO("JavaScript (Client)");
	}

	@Nonnull
	@Override
	public Image getIcon()
	{
		return JavaScriptIconGroup.javascriptmodule();
	}

	@Nonnull
	@Override
	public ModuleExtension<ClientJavaScriptModuleExtension> createImmutableExtension(@Nonnull ModuleRootLayer moduleRootLayer)
	{
		return new ClientJavaScriptModuleExtension(getId(), moduleRootLayer);
	}

	@Nonnull
	@Override
	public MutableModuleExtension<ClientJavaScriptModuleExtension> createMutableExtension(@Nonnull ModuleRootLayer moduleRootLayer)
	{
		return new ClientJavaScriptMutableModuleExtension(getId(), moduleRootLayer);
	}
}
