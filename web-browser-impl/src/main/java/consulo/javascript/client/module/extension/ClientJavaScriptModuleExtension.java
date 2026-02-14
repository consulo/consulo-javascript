package consulo.javascript.client.module.extension;

import consulo.annotation.access.RequiredReadAction;
import consulo.content.bundle.Sdk;
import consulo.content.bundle.SdkTable;
import consulo.content.bundle.SdkType;
import consulo.javascript.client.module.sdk.ClientJavaScriptSdkType;
import consulo.javascript.language.StandardJavaScriptVersions;
import consulo.javascript.module.extension.JavaScriptModuleExtension;
import consulo.language.util.ModuleUtilCore;
import consulo.language.version.LanguageVersion;
import consulo.module.Module;
import consulo.module.content.layer.ModuleRootLayer;
import consulo.module.content.layer.extension.ModuleExtensionBase;
import consulo.module.content.layer.extension.SdkModuleInheritableNamedPointerImpl;
import consulo.module.extension.ModuleInheritableNamedPointer;
import org.jdom.Element;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2014-06-29
 */
public class ClientJavaScriptModuleExtension extends ModuleExtensionBase<ClientJavaScriptModuleExtension> implements JavaScriptModuleExtension<ClientJavaScriptModuleExtension> {
    private SdkModuleInheritableNamedPointerImpl myPointer;

    protected LanguageVersion myLanguageVersion = StandardJavaScriptVersions.getInstance().getDefaultVersion();

    public ClientJavaScriptModuleExtension(@Nonnull String id, @Nonnull ModuleRootLayer rootLayer) {
        super(id, rootLayer);
        myPointer = new SdkModuleInheritableNamedPointerImpl(rootLayer, id) {
            @Nullable
            @Override
            public String getItemNameFromModule(@Nonnull Module module) {
                ClientJavaScriptModuleExtension extension = ModuleUtilCore.getExtension(module, ClientJavaScriptModuleExtension.class);
                if (extension == null) {
                    return null;
                }
                return extension.getSdkName();
            }

            @Nullable
            @Override
            public Sdk getItemFromModule(@Nonnull Module module) {
                ClientJavaScriptModuleExtension extension = ModuleUtilCore.getExtension(module, ClientJavaScriptModuleExtension.class);
                if (extension == null) {
                    return null;
                }
                return extension.getSdk();
            }
        };

        Sdk sdkByType = SdkTable.getInstance().findPredefinedSdkByType(ClientJavaScriptSdkType.getInstance());
        myPointer.set(null, sdkByType);
    }

    @RequiredReadAction
    @Override
    protected void loadStateImpl(@Nonnull Element element) {
        super.loadStateImpl(element);
        myLanguageVersion = StandardJavaScriptVersions.getInstance().findVersionById(element.getAttributeValue("language-version"));
    }

    @Override
    protected void getStateImpl(@Nonnull Element element) {
        super.getStateImpl(element);
        if (myLanguageVersion != StandardJavaScriptVersions.getInstance().getDefaultVersion()) {
            element.setAttribute("language-version", myLanguageVersion.getId());
        }
    }

    @Nonnull
    @Override
    public ModuleInheritableNamedPointer<Sdk> getInheritableSdk() {
        return myPointer;
    }

    @Nullable
    @Override
    public Sdk getSdk() {
        return myPointer.get();
    }

    @Nullable
    @Override
    public String getSdkName() {
        return myPointer.getName();
    }

    @Nonnull
    @Override
    public Class<? extends SdkType> getSdkTypeClass() {
        throw new IllegalArgumentException();
    }

    @Nonnull
    @Override
    public LanguageVersion getLanguageVersion() {
        return myLanguageVersion;
    }

    @RequiredReadAction
    @Override
    public void commit(@Nonnull ClientJavaScriptModuleExtension mutableModuleExtension) {
        super.commit(mutableModuleExtension);
        myLanguageVersion = mutableModuleExtension.getLanguageVersion();
    }
}
