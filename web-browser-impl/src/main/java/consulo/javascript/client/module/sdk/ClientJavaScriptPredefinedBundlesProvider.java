package consulo.javascript.client.module.sdk;

import com.intellij.ide.plugins.PluginManager;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkModificator;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import consulo.bundle.PredefinedBundlesProvider;
import consulo.roots.types.BinariesOrderRootType;
import consulo.roots.types.SourcesOrderRootType;

import javax.annotation.Nonnull;
import java.io.File;
import java.util.HashMap;
import java.util.Map;

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
	public void createBundles(@Nonnull Context context)
	{
		File sdkDir = new File(PluginManager.getPluginPath(ClientJavaScriptPredefinedBundlesProvider.class), "sdk");
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
			Sdk sdk = context.createSdkWithName(ClientJavaScriptSdkType.getInstance(), name);

			SdkModificator modificator = sdk.getSdkModificator();
			modificator.setHomePath(fileByIoFile.getPath());
			modificator.setVersionString("1");
			for(VirtualFile child : fileByIoFile.getChildren())
			{
				modificator.addRoot(child, BinariesOrderRootType.getInstance());
				modificator.addRoot(child, SourcesOrderRootType.getInstance());
			}
			modificator.commitChanges();
		}
	}
}
