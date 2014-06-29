package org.mustbe.consulo.javascript.client.module.extension;

import org.consulo.module.extension.ModuleInheritableNamedPointer;
import org.consulo.module.extension.impl.ModuleExtensionImpl;
import org.consulo.module.extension.impl.ModuleInheritableNamedPointerImpl;
import org.consulo.sdk.SdkUtil;
import org.consulo.util.pointers.NamedPointer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.client.module.sdk.ClientJavaScriptBundledSdkProvider;
import org.mustbe.consulo.javascript.module.extension.JavaScriptModuleExtension;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkType;
import com.intellij.openapi.roots.ModifiableRootModel;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public class ClientJavaScriptModuleExtension extends ModuleExtensionImpl<ClientJavaScriptModuleExtension> implements
		JavaScriptModuleExtension<ClientJavaScriptModuleExtension>
{
	private ModuleInheritableNamedPointerImpl<Sdk> myPointer;

	public ClientJavaScriptModuleExtension(@NotNull String id, @NotNull ModifiableRootModel rootModel)
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
		myPointer.set(null, ClientJavaScriptBundledSdkProvider.ANY_JAVASCRIPT_SDK);
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
}
