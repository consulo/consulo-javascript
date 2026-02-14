package consulo.javascript.client.module.extension;

import consulo.content.bundle.Sdk;
import consulo.disposer.Disposable;
import consulo.javascript.module.extension.JavaScriptMutableModuleExtension;
import consulo.language.version.LanguageVersion;
import consulo.module.content.layer.ModuleRootLayer;
import consulo.module.extension.MutableModuleInheritableNamedPointer;
import consulo.module.extension.swing.SwingMutableModuleExtension;
import consulo.ui.Component;
import consulo.ui.Label;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.layout.VerticalLayout;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import javax.swing.*;

/**
 * @author VISTALL
 * @since 2014-06-29
 */
public class ClientJavaScriptMutableModuleExtension extends ClientJavaScriptModuleExtension implements JavaScriptMutableModuleExtension<ClientJavaScriptModuleExtension>, SwingMutableModuleExtension {
    public ClientJavaScriptMutableModuleExtension(@Nonnull String id, @Nonnull ModuleRootLayer rootModel) {
        super(id, rootModel);
    }

    @Nonnull
    @Override
    public MutableModuleInheritableNamedPointer<Sdk> getInheritableSdk() {
        return (MutableModuleInheritableNamedPointer<Sdk>)super.getInheritableSdk();
    }

    @RequiredUIAccess
    @Nullable
    @Override
    public Component createConfigurationComponent(@Nonnull Disposable disposable, @Nonnull Runnable runnable) {
        return VerticalLayout.create().add(Label.create("Unsupported UI"));
    }

    @RequiredUIAccess
    @Nullable
    @Override
    public JComponent createConfigurablePanel(@Nonnull Disposable disposable, @Nonnull Runnable runnable) {
        return new ClientJavaScriptModuleExtensionPanel(this);
    }

    @Override
    public void setEnabled(boolean val) {
        myIsEnabled = val;
    }

    @Override
    public boolean isModified(@Nonnull ClientJavaScriptModuleExtension originalExtension) {
        return myIsEnabled != originalExtension.isEnabled() || myLanguageVersion != originalExtension.getLanguageVersion();
    }

    @Override
    public void setLanguageVersion(@Nonnull LanguageVersion languageVersion) {
        myLanguageVersion = languageVersion;
    }
}
