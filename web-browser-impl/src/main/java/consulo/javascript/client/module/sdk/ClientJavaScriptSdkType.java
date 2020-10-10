package consulo.javascript.client.module.sdk;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import com.intellij.openapi.projectRoots.SdkType;
import com.intellij.openapi.roots.OrderRootType;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.roots.types.BinariesOrderRootType;
import consulo.roots.types.SourcesOrderRootType;
import consulo.ui.image.Image;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public class ClientJavaScriptSdkType extends SdkType
{
	@Nonnull
	public static ClientJavaScriptSdkType getInstance()
	{
		return EP_NAME.findExtension(ClientJavaScriptSdkType.class);
	}

	public ClientJavaScriptSdkType()
	{
		super("CLIENT_JAVASCRIPT_SDK_TYPE");
	}

	@Override
	public boolean supportsUserAdd()
	{
		return false;
	}

	@Override
	public boolean isRootTypeApplicable(OrderRootType type)
	{
		return type == BinariesOrderRootType.getInstance() || type == SourcesOrderRootType.getInstance();
	}

	@Nullable
	@Override
	public Image getIcon()
	{
		return JavaScriptIconGroup.javaScript();
	}

	@Override
	public boolean isValidSdkHome(String path)
	{
		return false;
	}

	@Nullable
	@Override
	public String getVersionString(String sdkHome)
	{
		return "1";
	}

	@Override
	public String suggestSdkName(String currentSdkName, String sdkHome)
	{
		return currentSdkName;
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "Client JavaScript";
	}
}
