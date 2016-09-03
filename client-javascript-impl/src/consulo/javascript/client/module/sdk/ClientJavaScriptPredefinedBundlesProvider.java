package consulo.javascript.client.module.sdk;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.jetbrains.annotations.NotNull;
import com.intellij.ide.plugins.IdeaPluginDescriptor;
import com.intellij.ide.plugins.PluginManager;
import com.intellij.ide.plugins.cl.PluginClassLoader;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.projectRoots.impl.SdkImpl;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.Consumer;
import consulo.bundle.PredefinedBundlesProvider;
import consulo.roots.types.BinariesOrderRootType;
import consulo.roots.types.SourcesOrderRootType;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public class ClientJavaScriptPredefinedBundlesProvider extends PredefinedBundlesProvider
{
	public static final String ANY_JAVASCRIPT_SDK = "JavaScript SDK (Any Browser)";

	private Map<String, String> ourMapping = new HashMap<String, String>()
	{
		{
			put("clientAny", ANY_JAVASCRIPT_SDK);
		}
	};

	@Override
	public void createBundles(@NotNull Consumer<SdkImpl> consumer)
	{
		PluginClassLoader classLoader = (PluginClassLoader) getClass().getClassLoader();
		IdeaPluginDescriptor plugin = PluginManager.getPlugin(classLoader.getPluginId());
		assert plugin != null;
		File path = plugin.getPath();

		File sdkDir = new File(path, "sdk");
		if(!sdkDir.exists())
		{
			return;
		}

		for(File file : sdkDir.listFiles())
		{
			String name = ourMapping.get(file.getName());
			if(name == null)
			{
				continue;
			}

			VirtualFile fileByIoFile = LocalFileSystem.getInstance().findFileByIoFile(file);
			if(fileByIoFile == null)
			{
				continue;
			}
			SdkImpl sdk = createSdkWithName(ClientJavaScriptSdkType.getInstance(), name);
			sdk.setHomePath(fileByIoFile.getPath());
			sdk.setVersionString("1");

			SdkModificator sdkModificator = sdk.getSdkModificator();
			for(VirtualFile child : fileByIoFile.getChildren())
			{
				sdkModificator.addRoot(child, BinariesOrderRootType.getInstance());
				sdkModificator.addRoot(child, SourcesOrderRootType.getInstance());
			}
			sdkModificator.commitChanges();
			consumer.consume(sdk);
		}
	}
}
