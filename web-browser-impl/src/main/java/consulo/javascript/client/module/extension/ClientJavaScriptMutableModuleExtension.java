package consulo.javascript.client.module.extension;

import consulo.content.bundle.Sdk;
import consulo.disposer.Disposable;
import consulo.javascript.language.JavaScriptLanguageVersion;
import consulo.javascript.language.StandardJavaScriptVersions;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.javascript.module.extension.JavaScriptMutableModuleExtension;
import consulo.language.version.LanguageVersion;
import consulo.localize.LocalizeValue;
import consulo.module.content.layer.ModuleRootLayer;
import consulo.module.extension.MutableModuleInheritableNamedPointer;
import consulo.ui.CheckBox;
import consulo.ui.ComboBox;
import consulo.ui.Component;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.layout.VerticalLayout;
import consulo.ui.util.FormBuilder;

import org.jspecify.annotations.Nullable;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * @author VISTALL
 * @since 2014-06-29
 */
public class ClientJavaScriptMutableModuleExtension extends ClientJavaScriptModuleExtension implements JavaScriptMutableModuleExtension<ClientJavaScriptModuleExtension> {
    public ClientJavaScriptMutableModuleExtension(String id, ModuleRootLayer rootModel) {
        super(id, rootModel);
    }

    @Override
    public MutableModuleInheritableNamedPointer<Sdk> getInheritableSdk() {
        return (MutableModuleInheritableNamedPointer<Sdk>)super.getInheritableSdk();
    }

    @RequiredUIAccess
    @Nullable
    @Override
    public Component createConfigurationComponent(Disposable disposable, Runnable runnable) {
        List<JavaScriptLanguageVersion> validLanguageVersions = StandardJavaScriptVersions.getInstance().getValidLanguageVersions();

        ComboBox<JavaScriptLanguageVersion> languageVersionBox = ComboBox.create(validLanguageVersions);
        languageVersionBox.setTextRenderer(
            version -> version == null ? LocalizeValue.empty() : LocalizeValue.of(version.getPresentableName())
        );
        languageVersionBox.setValue((JavaScriptLanguageVersion)getLanguageVersion());
        languageVersionBox.addValueListener(event -> {
            JavaScriptLanguageVersion version = event.getValue();
            if (version != null) {
                setLanguageVersion(version);
            }
        });

        // the environment libraries, next to the ECMAScript library of the language level
        CheckBox domBox = CheckBox.create(JavaScriptLocalize.clientModuleExtensionLibraryDom());
        domBox.setValue(myStandardLibraries.containsAll(DOM_LIBRARIES));
        domBox.addValueListener(event -> setLibraries(DOM_LIBRARIES, event.getValue()));

        CheckBox scriptHostBox = CheckBox.create(JavaScriptLocalize.clientModuleExtensionLibraryScripthost());
        scriptHostBox.setValue(myStandardLibraries.contains(SCRIPT_HOST_LIBRARY));
        scriptHostBox.addValueListener(event -> setLibraries(Set.of(SCRIPT_HOST_LIBRARY), event.getValue()));

        VerticalLayout libraries = VerticalLayout.create();
        libraries.add(domBox);
        libraries.add(scriptHostBox);

        FormBuilder formBuilder = FormBuilder.create();
        formBuilder.addLabeled(JavaScriptLocalize.clientModuleExtensionDefaultVersionLabel(), languageVersionBox);
        formBuilder.addLabeled(JavaScriptLocalize.clientModuleExtensionLibrariesLabel(), libraries);
        return formBuilder.build();
    }

    /**
     * The environment libraries ({@link #getStandardLibraries()}).
     */
    public void setStandardLibraries(Set<String> libraries) {
        myStandardLibraries = new LinkedHashSet<>(libraries);
    }

    private void setLibraries(Set<String> libraries, boolean enabled) {
        if (enabled) {
            myStandardLibraries.addAll(libraries);
        }
        else {
            myStandardLibraries.removeAll(libraries);
        }
    }

    @Override
    public void setEnabled(boolean val) {
        myIsEnabled = val;
    }

    @Override
    public boolean isModified(ClientJavaScriptModuleExtension originalExtension) {
        return myIsEnabled != originalExtension.isEnabled() || myLanguageVersion != originalExtension.getLanguageVersion()
            || !myStandardLibraries.equals(originalExtension.getStandardLibraries())
            || !getInheritableSdk().equals(originalExtension.getInheritableSdk());
    }

    @Override
    public void setLanguageVersion(LanguageVersion languageVersion) {
        myLanguageVersion = languageVersion;
    }
}
