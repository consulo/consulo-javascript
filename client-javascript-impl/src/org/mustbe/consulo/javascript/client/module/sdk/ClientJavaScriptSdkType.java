package org.mustbe.consulo.javascript.client.module.sdk;

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.JavaScriptIcons;
import com.intellij.openapi.projectRoots.SdkType;
import com.intellij.openapi.roots.OrderRootType;
import consulo.lombok.annotations.Lazy;
import consulo.roots.types.BinariesOrderRootType;
import consulo.roots.types.SourcesOrderRootType;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public class ClientJavaScriptSdkType extends SdkType
{
	@NotNull
	@Lazy
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
	public Icon getIcon()
	{
		return JavaScriptIcons.JavaScript;
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

	@NotNull
	@Override
	public String getPresentableName()
	{
		return "Client JavaScript";
	}
}
