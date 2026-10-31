package consulo.typescript.it;

import consulo.javascript.client.module.extension.ClientJavaScriptModuleExtension;
import consulo.javascript.client.module.sdk.ClientJavaScriptSdks;
import consulo.application.WriteAction;
import consulo.it.HeadlessProjects;
import consulo.javascript.module.declarations.JavaScriptDeclarations;
import consulo.module.Module;
import consulo.module.content.ModuleRootManager;
import consulo.module.content.layer.ModifiableRootModel;

import java.util.List;
import java.util.Set;

/**
 * The Web module extension (JavaScript in a browser) of the test module, with the standard library SDK (created here:
 * the predefined bundles aren't loaded headless).
 */
public final class WebModules {
    private WebModules() {
    }

    public static void enableWeb(Module module) throws Exception {
        enableWeb(module, ClientJavaScriptModuleExtension.DEFAULT_LIBRARIES);
    }

    /**
     * Enables Web with the SDK and waits for the indexing of the SDK roots (the global resolve finds nothing while
     * indexing).
     */
    static void enableWeb(Module module, Set<String> libraries) throws Exception {
        ClientJavaScriptSdks.enableWeb(module, libraries);
        HeadlessProjects.awaitIdle(module.getProject());
    }

    /**
     * Sets the JavaScript declarations entry of an owner (no urls: removes it) and waits for the indexing of its roots.
     */
    static void setDeclarations(Module module, String name, List<String> urls) throws Exception {
        WriteAction.run(() -> {
            ModifiableRootModel model = ModuleRootManager.getInstance(module).getModifiableModel();
            JavaScriptDeclarations.set(model, name, urls);
            model.commit();
        });
        HeadlessProjects.awaitIdle(module.getProject());
    }

    public static void disableWeb(Module module) throws Exception {
        ClientJavaScriptSdks.disableWeb(module);
        HeadlessProjects.awaitIdle(module.getProject());
    }
}
