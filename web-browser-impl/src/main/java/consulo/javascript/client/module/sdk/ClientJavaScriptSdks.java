package consulo.javascript.client.module.sdk;

import consulo.application.WriteAction;
import consulo.content.base.BinariesOrderRootType;
import consulo.content.base.SourcesOrderRootType;
import consulo.content.bundle.Sdk;
import consulo.content.bundle.SdkModificator;
import consulo.content.bundle.SdkTable;
import consulo.javascript.client.module.extension.ClientJavaScriptMutableModuleExtension;
import consulo.javascript.library.JavaScriptStandardLibrary;
import consulo.module.Module;
import consulo.module.content.ModuleRootManager;
import consulo.module.content.layer.ModifiableModuleRootLayer;
import consulo.module.content.layer.ModifiableRootModel;
import consulo.module.content.layer.orderEntry.ModuleExtensionWithSdkOrderEntry;
import consulo.module.content.layer.orderEntry.OrderEntry;
import consulo.virtualFileSystem.VirtualFile;
import org.jspecify.annotations.Nullable;

import java.util.Set;

/**
 * The Web SDK of the standard library, and enabling Web on a module with it: for the code that sets up modules (project
 * generators, headless tests - where the predefined bundles aren't loaded).
 *
 * @since 2026-10-31
 */
public final class ClientJavaScriptSdks {
    public static final String STANDARD_LIBRARY_SDK_NAME = "JavaScript Standard Library";

    /**
     * The id of the Web extension.
     */
    public static final String WEB_EXTENSION_ID = "client-javascript";

    private ClientJavaScriptSdks() {
    }

    /**
     * The Web SDK of the bundled standard library: the predefined one, else created and added to the SDK table (where
     * the predefined bundles aren't loaded). Null when the bundled libraries aren't found.
     */
    @Nullable
    public static Sdk findOrCreateStandardLibrary() {
        SdkTable sdkTable = SdkTable.getInstance();
        Sdk predefined = sdkTable.findPredefinedSdkByType(ClientJavaScriptSdkType.getInstance());
        if (predefined != null) {
            return predefined;
        }
        Sdk existing = sdkTable.findSdk(STANDARD_LIBRARY_SDK_NAME);
        if (existing != null) {
            return existing;
        }
        VirtualFile directory = JavaScriptStandardLibrary.getInstance().getLibDirectory();
        if (directory == null) {
            return null;
        }
        return WriteAction.compute(() -> {
            Sdk sdk = sdkTable.createSdk(STANDARD_LIBRARY_SDK_NAME, ClientJavaScriptSdkType.getInstance());
            setupRoots(sdk, directory);
            sdkTable.addSdk(sdk);
            return sdk;
        });
    }

    /**
     * Enables Web on the module with the standard library SDK and the environment libraries
     * ({@link consulo.javascript.client.module.extension.ClientJavaScriptModuleExtension#DEFAULT_LIBRARIES}: the DOM).
     * Runs a write action: not from inside a read action.
     */
    public static void enableWeb(Module module, Set<String> libraries) {
        Sdk sdk = findOrCreateStandardLibrary();
        WriteAction.run(() -> {
            ModifiableRootModel model = ModuleRootManager.getInstance(module).getModifiableModel();
            enableWeb(model, libraries, sdk);
            model.commit();
        });
    }

    /**
     * Enables Web in the model (not committed) with the SDK and its order entry (the settings add the entry when the
     * extension is enabled there): the SDK roots are indexed and in the resolve scope of the module. Keeps an SDK the
     * extension already has.
     */
    static void enableWeb(ModifiableRootModel model, Set<String> libraries, @Nullable Sdk sdk) {
        ClientJavaScriptMutableModuleExtension extension = model.getExtensionWithoutCheck(WEB_EXTENSION_ID);
        extension.setEnabled(true);
        extension.setStandardLibraries(libraries);
        if (extension.getSdk() == null) {
            extension.getInheritableSdk().set(null, sdk);
        }
        if (extension.getSdk() != null && !hasSdkEntry(model, extension)) {
            ((ModifiableModuleRootLayer) extension.getModuleRootLayer()).addModuleExtensionSdkEntry(extension);
        }
    }

    /**
     * Disables Web on the module. Runs a write action: not from inside a read action.
     */
    public static void disableWeb(Module module) {
        WriteAction.run(() -> {
            ModifiableRootModel model = ModuleRootManager.getInstance(module).getModifiableModel();
            ClientJavaScriptMutableModuleExtension extension = model.getExtensionWithoutCheck(WEB_EXTENSION_ID);
            extension.setEnabled(false);
            model.commit();
        });
    }

    private static boolean hasSdkEntry(ModifiableRootModel model, ClientJavaScriptMutableModuleExtension extension) {
        for (OrderEntry entry : model.getOrderEntries()) {
            if (entry instanceof ModuleExtensionWithSdkOrderEntry sdkEntry && extension.getId().equals(sdkEntry.getModuleExtensionId())) {
                return true;
            }
        }
        return false;
    }

    static void setupRoots(Sdk sdk, VirtualFile libDirectory) {
        SdkModificator modificator = sdk.getSdkModificator();
        modificator.setHomePath(libDirectory.getPath());
        modificator.setVersionString("TypeScript 5.9.3");
        modificator.addRoot(libDirectory, BinariesOrderRootType.ID);
        modificator.addRoot(libDirectory, SourcesOrderRootType.ID);
        modificator.commitChanges();
    }
}
