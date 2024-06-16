package consulo.javascript.client.module.sdk;

import consulo.annotation.component.ExtensionImpl;
import consulo.content.OrderRootType;
import consulo.content.base.BinariesOrderRootType;
import consulo.content.base.SourcesOrderRootType;
import consulo.content.bundle.SdkType;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.ui.image.Image;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 29.06.14
 */
@ExtensionImpl
public class ClientJavaScriptSdkType extends SdkType
{
	@Nonnull
	public static ClientJavaScriptSdkType getInstance()
	{
		return EP_NAME.findExtensionOrFail(ClientJavaScriptSdkType.class);
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

	@Nonnull
	@Override
	public Image getIcon()
	{
		return JavaScriptIconGroup.javascriptmodule();
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
