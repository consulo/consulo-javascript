package consulo.javascript.client.module.sdk;

import consulo.annotation.component.ExtensionImpl;
import consulo.application.Application;
import consulo.content.OrderRootType;
import consulo.content.base.BinariesOrderRootType;
import consulo.content.base.SourcesOrderRootType;
import consulo.content.bundle.SdkType;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2014-06-29
 */
@ExtensionImpl
public class ClientJavaScriptSdkType extends SdkType {
    @Nonnull
    public static ClientJavaScriptSdkType getInstance() {
        return Application.get().getExtensionPoint(SdkType.class).findExtensionOrFail(ClientJavaScriptSdkType.class);
    }

    public ClientJavaScriptSdkType() {
        super("CLIENT_JAVASCRIPT_SDK_TYPE", LocalizeValue.localizeTODO("Client JavaScript"), JavaScriptIconGroup.javascriptmodule());
    }

    @Override
    public boolean supportsUserAdd() {
        return false;
    }

    @Override
    public boolean isRootTypeApplicable(OrderRootType type) {
        return type == BinariesOrderRootType.getInstance() || type == SourcesOrderRootType.getInstance();
    }

    @Override
    public boolean isValidSdkHome(String path) {
        return false;
    }

    @Nullable
    @Override
    public String getVersionString(String sdkHome) {
        return "1";
    }

    @Nonnull
    @Override
    public String suggestSdkName(String currentSdkName, String sdkHome) {
        return currentSdkName;
    }
}
