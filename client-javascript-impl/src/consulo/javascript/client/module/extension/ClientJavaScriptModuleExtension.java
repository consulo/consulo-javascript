package consulo.javascript.client.module.extension;

import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import consulo.javascript.client.module.sdk.ClientJavaScriptSdkType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkTable;
import com.intellij.openapi.projectRoots.SdkType;
import consulo.annotations.RequiredReadAction;
import consulo.bundle.SdkUtil;
import consulo.extension.impl.ModuleExtensionImpl;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.javascript.lang.StandardJavaScriptVersions;
import consulo.javascript.module.extension.JavaScriptModuleExtension;
import consulo.lang.LanguageVersion;
import consulo.module.extension.ModuleInheritableNamedPointer;
import consulo.module.extension.impl.ModuleInheritableNamedPointerImpl;
import consulo.roots.ModuleRootLayer;
import consulo.util.pointers.NamedPointer;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public class ClientJavaScriptModuleExtension extends ModuleExtensionImpl<ClientJavaScriptModuleExtension> implements JavaScriptModuleExtension<ClientJavaScriptModuleExtension>
{
	private ModuleInheritableNamedPointerImpl<Sdk> myPointer;

	protected LanguageVersion<JavaScriptLanguage> myLanguageVersion = StandardJavaScriptVersions.getDefaultVersion();

	public ClientJavaScriptModuleExtension(@NotNull String id, @NotNull ModuleRootLayer rootModel)
	{
		super(id, rootModel);
		myPointer = new ModuleInheritableNamedPointerImpl<Sdk>(getProject(), id)
		{
			@Nullable
			@Override
			public String getItemNameFromModule(@NotNull Module module)
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
			public Sdk getItemFromModule(@NotNull Module module)
			{
				ClientJavaScriptModuleExtension extension = ModuleUtilCore.getExtension(module, ClientJavaScriptModuleExtension.class);
				if(extension == null)
				{
					return null;
				}
				return extension.getSdk();
			}

			@NotNull
			@Override
			public NamedPointer<Sdk> getPointer(@NotNull Project project, @NotNull String name)
			{
				return SdkUtil.createPointer(name);
			}
		};

		Sdk sdkByType = SdkTable.getInstance().findPredefinedSdkByType(ClientJavaScriptSdkType.getInstance());
		myPointer.set(null, sdkByType);
	}

	@RequiredReadAction
	@Override
	protected void loadStateImpl(@NotNull Element element)
	{
		super.loadStateImpl(element);
		myLanguageVersion = StandardJavaScriptVersions.findVersionById(element.getAttributeValue("language-version"));
	}

	@Override
	protected void getStateImpl(@NotNull Element element)
	{
		super.getStateImpl(element);
		if(myLanguageVersion != StandardJavaScriptVersions.getDefaultVersion())
		{
			element.setAttribute("language-version", myLanguageVersion.getName());
		}
	}

	@NotNull
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

	@NotNull
	@Override
	public Class<? extends SdkType> getSdkTypeClass()
	{
		throw new IllegalArgumentException();
	}

	@NotNull
	@Override
	public LanguageVersion<JavaScriptLanguage> getLanguageVersion()
	{
		return myLanguageVersion;
	}

	@Override
	public void commit(@NotNull ClientJavaScriptModuleExtension mutableModuleExtension)
	{
		super.commit(mutableModuleExtension);
		myLanguageVersion = mutableModuleExtension.getLanguageVersion();
	}
}
