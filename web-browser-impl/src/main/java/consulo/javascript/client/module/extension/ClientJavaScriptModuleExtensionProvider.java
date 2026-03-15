package consulo.javascript.client.module.extension;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.localize.LocalizeValue;
import consulo.module.content.layer.ModuleExtensionProvider;
import consulo.module.content.layer.ModuleRootLayer;
import consulo.module.extension.ModuleExtension;
import consulo.module.extension.MutableModuleExtension;
import consulo.ui.image.Image;


/**
 * @author VISTALL
 * @since 2022-08-29
 */
@ExtensionImpl
public class ClientJavaScriptModuleExtensionProvider implements ModuleExtensionProvider<ClientJavaScriptModuleExtension> {
    @Override
    public String getId() {
        return "client-javascript";
    }

    @Override
    public boolean isAllowMixin() {
        return true;
    }

    @Override
    public LocalizeValue getName() {
        return LocalizeValue.localizeTODO("JavaScript (Client)");
    }

    @Override
    public Image getIcon() {
        return JavaScriptIconGroup.javascriptmodule();
    }

    @Override
    public ModuleExtension<ClientJavaScriptModuleExtension> createImmutableExtension(ModuleRootLayer moduleRootLayer) {
        return new ClientJavaScriptModuleExtension(getId(), moduleRootLayer);
    }

    @Override
    public MutableModuleExtension<ClientJavaScriptModuleExtension> createMutableExtension(ModuleRootLayer moduleRootLayer) {
        return new ClientJavaScriptMutableModuleExtension(getId(), moduleRootLayer);
    }
}
