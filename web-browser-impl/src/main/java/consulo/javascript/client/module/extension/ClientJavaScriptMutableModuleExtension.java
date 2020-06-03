package consulo.javascript.client.module.extension;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import javax.swing.*;

import com.intellij.openapi.projectRoots.Sdk;
import consulo.disposer.Disposable;
import consulo.javascript.module.extension.JavaScriptMutableModuleExtension;
import consulo.lang.LanguageVersion;
import consulo.module.extension.MutableModuleInheritableNamedPointer;
import consulo.module.extension.swing.SwingMutableModuleExtension;
import consulo.roots.ModuleRootLayer;
import consulo.ui.Component;
import consulo.ui.Label;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.layout.VerticalLayout;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public class ClientJavaScriptMutableModuleExtension extends ClientJavaScriptModuleExtension implements JavaScriptMutableModuleExtension<ClientJavaScriptModuleExtension>, SwingMutableModuleExtension
{
	public ClientJavaScriptMutableModuleExtension(@Nonnull String id, @Nonnull ModuleRootLayer rootModel)
	{
		super(id, rootModel);
	}

	@Nonnull
	@Override
	public MutableModuleInheritableNamedPointer<Sdk> getInheritableSdk()
	{
		return (MutableModuleInheritableNamedPointer<Sdk>) super.getInheritableSdk();
	}

	@RequiredUIAccess
	@Nullable
	@Override
	public Component createConfigurationComponent(@Nonnull Disposable disposable, @Nonnull Runnable runnable)
	{
		return VerticalLayout.create().add(Label.create("Unsupported UI"));
	}

	@RequiredUIAccess
	@Nullable
	@Override
	public JComponent createConfigurablePanel(@Nonnull Disposable disposable, @Nonnull Runnable runnable)
	{
		return new ClientJavaScriptModuleExtensionPanel(this);
	}

	@Override
	public void setEnabled(boolean val)
	{
		myIsEnabled = val;
	}

	@Override
	public boolean isModified(@Nonnull ClientJavaScriptModuleExtension originalExtension)
	{
		return myIsEnabled != originalExtension.isEnabled() || myLanguageVersion != originalExtension.getLanguageVersion();
	}

	@Override
	public void setLanguageVersion(@Nonnull LanguageVersion languageVersion)
	{
		myLanguageVersion = languageVersion;
	}
}
