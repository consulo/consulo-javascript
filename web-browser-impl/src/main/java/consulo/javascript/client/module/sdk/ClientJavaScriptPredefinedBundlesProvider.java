package consulo.javascript.client.module.sdk;

import consulo.annotation.component.ExtensionImpl;
import consulo.content.bundle.PredefinedBundlesProvider;
import consulo.content.bundle.Sdk;
import consulo.javascript.library.JavaScriptStandardLibrary;
import consulo.virtualFileSystem.VirtualFile;

/**
 * The predefined Web SDK: the standard library declarations bundled with the plugin (TypeScript's lib.*.d.ts), the
 * default SDK of the Web extension. Recreated on every start, so its roots follow the plugin.
 *
 * @since 2026-10-31
 */
@ExtensionImpl
public class ClientJavaScriptPredefinedBundlesProvider extends PredefinedBundlesProvider {
    @Override
    public void createBundles(Context context) {
        VirtualFile directory = JavaScriptStandardLibrary.getInstance().getLibDirectory();
        if (directory == null) {
            return;
        }
        Sdk sdk = context.createSdkWithName(ClientJavaScriptSdkType.getInstance(), ClientJavaScriptSdks.STANDARD_LIBRARY_SDK_NAME);
        ClientJavaScriptSdks.setupRoots(sdk, directory);
    }
}
