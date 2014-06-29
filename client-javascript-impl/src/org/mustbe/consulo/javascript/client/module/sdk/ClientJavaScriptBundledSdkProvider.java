package org.mustbe.consulo.javascript.client.module.sdk;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jetbrains.annotations.NotNull;
import com.intellij.ide.plugins.IdeaPluginDescriptor;
import com.intellij.ide.plugins.PluginManager;
import com.intellij.ide.plugins.cl.PluginClassLoader;
import com.intellij.openapi.projectRoots.BundledSdkProvider;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.projectRoots.impl.SdkImpl;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.SmartList;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public class ClientJavaScriptBundledSdkProvider implements BundledSdkProvider
{
	public static final String ANY_JAVASCRIPT_SDK = "JavaScript SDK (Any Browser)";

	private Map<String, String> ourMapping = new HashMap<String, String>()
	{
		{
			put("clientAny", ANY_JAVASCRIPT_SDK);
		}
	};

	@NotNull
	@Override
	public Sdk[] createBundledSdks()
	{
		PluginClassLoader classLoader = (PluginClassLoader) getClass().getClassLoader();
		IdeaPluginDescriptor plugin = PluginManager.getPlugin(classLoader.getPluginId());
		assert plugin != null;
		File path = plugin.getPath();

		File sdkDir = new File(path, "sdk");
		if(!sdkDir.exists())
		{
			return Sdk.EMPTY_ARRAY;
		}

		List<Sdk> list = new SmartList<Sdk>();
		for(File file : sdkDir.listFiles())
		{
			String s = ourMapping.get(file.getName());
			if(s == null)
			{
				continue;
			}

			VirtualFile fileByIoFile = LocalFileSystem.getInstance().findFileByIoFile(file);
			if(fileByIoFile == null)
			{
				continue;
			}
			SdkImpl sdk = new SdkImpl(s, ClientJavaScriptSdkType.INSTANCE);
			sdk.setHomePath(fileByIoFile.getPath());
			sdk.setVersionString("1");

			SdkModificator sdkModificator = sdk.getSdkModificator();
			for(VirtualFile child : fileByIoFile.getChildren())
			{
				sdkModificator.addRoot(child, OrderRootType.CLASSES);
				sdkModificator.addRoot(child, OrderRootType.SOURCES);
			}
			sdkModificator.commitChanges();
			list.add(sdk);
		}
		return list.toArray(new Sdk[list.size()]);
	}
}
