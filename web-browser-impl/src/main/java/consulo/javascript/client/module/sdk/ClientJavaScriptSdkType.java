package consulo.javascript.client.module.sdk;

import consulo.annotation.component.ExtensionImpl;
import consulo.application.Application;
import consulo.content.OrderRootType;
import consulo.content.base.BinariesOrderRootType;
import consulo.content.base.SourcesOrderRootType;
import consulo.content.bundle.SdkType;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.localize.LocalizeValue;
import jakarta.inject.Inject;
import org.jspecify.annotations.Nullable;

import java.util.Set;

/**
 * @author VISTALL
 * @since 2014-06-29
 */
@ExtensionImpl
public class ClientJavaScriptSdkType extends SdkType {

    public static ClientJavaScriptSdkType getInstance() {
        return Application.get().getExtensionPoint(SdkType.class).findExtensionOrFail(ClientJavaScriptSdkType.class);
    }

    private static final Set<String> ourAcceptedRootTypeIds = Set.of(BinariesOrderRootType.ID, SourcesOrderRootType.ID);

    private final Application myApplication;

    @Inject
    public ClientJavaScriptSdkType(Application application) {
        super("CLIENT_JAVASCRIPT_SDK_TYPE", LocalizeValue.localizeTODO("Client JavaScript"), JavaScriptIconGroup.javascriptmodule());
        myApplication = application;
    }

    @Override
    public boolean supportsUserAdd() {
        return false;
    }

    @Override
    public boolean isRootTypeApplicable(String type) {
        return ourAcceptedRootTypeIds.contains(type);
    }

    @Override
    public boolean isValidSdkHome(String path) {
        return false;
    }

    @Nullable
    @Override
    public String getVersionString(String sdkHome) {
        return myApplication.getBuildNumber().asString();
    }

    @Override
    public String suggestSdkName(String currentSdkName, String sdkHome) {
        return currentSdkName;
    }
}
