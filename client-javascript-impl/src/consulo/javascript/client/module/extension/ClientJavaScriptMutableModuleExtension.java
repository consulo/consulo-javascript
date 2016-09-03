package consulo.javascript.client.module.extension;

import javax.swing.JComponent;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.openapi.projectRoots.Sdk;
import consulo.annotations.RequiredDispatchThread;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.javascript.module.extension.JavaScriptMutableModuleExtension;
import consulo.lang.LanguageVersion;
import consulo.module.extension.MutableModuleInheritableNamedPointer;
import consulo.roots.ModuleRootLayer;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public class ClientJavaScriptMutableModuleExtension extends ClientJavaScriptModuleExtension implements JavaScriptMutableModuleExtension<ClientJavaScriptModuleExtension>
{
	public ClientJavaScriptMutableModuleExtension(@NotNull String id, @NotNull ModuleRootLayer rootModel)
	{
		super(id, rootModel);
	}

	@NotNull
	@Override
	public MutableModuleInheritableNamedPointer<Sdk> getInheritableSdk()
	{
		return (MutableModuleInheritableNamedPointer<Sdk>) super.getInheritableSdk();
	}

	@RequiredDispatchThread
	@Nullable
	@Override
	public JComponent createConfigurablePanel(@NotNull Runnable updateOnCheck)
	{
		return new ClientJavaScriptModuleExtensionPanel(this);
	}

	@Override
	public void setEnabled(boolean val)
	{
		myIsEnabled = val;
	}

	@Override
	public boolean isModified(@NotNull ClientJavaScriptModuleExtension originalExtension)
	{
		return myIsEnabled != originalExtension.isEnabled() || myLanguageVersion != originalExtension.getLanguageVersion();
	}

	@Override
	public void setLanguageVersion(@NotNull LanguageVersion<JavaScriptLanguage> languageVersion)
	{
		myLanguageVersion = languageVersion;
	}
}
