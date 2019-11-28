package consulo.javascript.client.module.extension;

import javax.annotation.Nonnull;

import org.jdom.Element;

import javax.annotation.Nullable;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkTable;
import com.intellij.openapi.projectRoots.SdkType;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.client.module.sdk.ClientJavaScriptSdkType;
import consulo.javascript.lang.StandardJavaScriptVersions;
import consulo.javascript.module.extension.JavaScriptModuleExtension;
import consulo.lang.LanguageVersion;
import consulo.module.extension.ModuleInheritableNamedPointer;
import consulo.module.extension.impl.ModuleExtensionImpl;
import consulo.module.extension.impl.ModuleInheritableNamedPointerImpl;
import consulo.roots.ModuleRootLayer;
import consulo.roots.impl.ModuleRootLayerImpl;
import consulo.util.pointers.NamedPointer;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public class ClientJavaScriptModuleExtension extends ModuleExtensionImpl<ClientJavaScriptModuleExtension> implements JavaScriptModuleExtension<ClientJavaScriptModuleExtension>
{
	private ModuleInheritableNamedPointerImpl<Sdk> myPointer;

	protected LanguageVersion myLanguageVersion = StandardJavaScriptVersions.getInstance().getDefaultVersion();

	public ClientJavaScriptModuleExtension(@Nonnull String id, @Nonnull ModuleRootLayer rootLayer)
	{
		super(id, rootLayer);
		myPointer = new ModuleInheritableNamedPointerImpl<Sdk>(rootLayer, id)
		{
			@Nullable
			@Override
			public String getItemNameFromModule(@Nonnull Module module)
			{
				ClientJavaScriptModuleExtension extension = ModuleUtilCore.getExtension(module, ClientJavaScriptModuleExtension.class);
				if(extension == null)
				{
					return null;
				}
				return extension.getSdkName();
			}

			@Nullable
			@Override
			public Sdk getItemFromModule(@Nonnull Module module)
			{
				ClientJavaScriptModuleExtension extension = ModuleUtilCore.getExtension(module, ClientJavaScriptModuleExtension.class);
				if(extension == null)
				{
					return null;
				}
				return extension.getSdk();
			}

			@Nonnull
			@Override
			public NamedPointer<Sdk> getPointer(@Nonnull ModuleRootLayer moduleRootLayer, @Nonnull String name)
			{
				return ((ModuleRootLayerImpl)moduleRootLayer).getRootModel().getConfigurationAccessor().getSdkPointer(name);
			}
		};

		Sdk sdkByType = SdkTable.getInstance().findPredefinedSdkByType(ClientJavaScriptSdkType.getInstance());
		myPointer.set(null, sdkByType);
	}

	@RequiredReadAction
	@Override
	protected void loadStateImpl(@Nonnull Element element)
	{
		super.loadStateImpl(element);
		myLanguageVersion = StandardJavaScriptVersions.getInstance().findVersionById(element.getAttributeValue("language-version"));
	}

	@Override
	protected void getStateImpl(@Nonnull Element element)
	{
		super.getStateImpl(element);
		if(myLanguageVersion != StandardJavaScriptVersions.getInstance().getDefaultVersion())
		{
			element.setAttribute("language-version", myLanguageVersion.getId());
		}
	}

	@Nonnull
	@Override
	public ModuleInheritableNamedPointer<Sdk> getInheritableSdk()
	{
		return myPointer;
	}

	@Nullable
	@Override
	public Sdk getSdk()
	{
		return myPointer.get();
	}

	@Nullable
	@Override
	public String getSdkName()
	{
		return myPointer.getName();
	}

	@Nonnull
	@Override
	public Class<? extends SdkType> getSdkTypeClass()
	{
		throw new IllegalArgumentException();
	}

	@Nonnull
	@Override
	public LanguageVersion getLanguageVersion()
	{
		return myLanguageVersion;
	}

	@RequiredReadAction
	@Override
	public void commit(@Nonnull ClientJavaScriptModuleExtension mutableModuleExtension)
	{
		super.commit(mutableModuleExtension);
		myLanguageVersion = mutableModuleExtension.getLanguageVersion();
	}
}
