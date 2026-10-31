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

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2014-06-29
 */
/**
 * Web: JavaScript running in a browser. It sees the standard library of its language level and of the browser
 * environment ({@link #getStandardLibraries()}).
 */
public class ClientJavaScriptModuleExtension extends ModuleExtensionBase<ClientJavaScriptModuleExtension> implements JavaScriptModuleExtension<ClientJavaScriptModuleExtension> {
    /**
     * The DOM: the libraries of tsc for a browser (lib.esXXXX.full.d.ts), without the ECMAScript level.
     */
    public static final Set<String> DOM_LIBRARIES = Set.of("dom", "dom.iterable", "dom.asynciterable", "webworker.importscripts");

    public static final String SCRIPT_HOST_LIBRARY = "scripthost";

    public static final Set<String> DEFAULT_LIBRARIES = Stream.concat(DOM_LIBRARIES.stream(), Stream.of(SCRIPT_HOST_LIBRARY))
        .collect(Collectors.toUnmodifiableSet());

    private static final String LIBRARY_ELEMENT = "library";
    private static final String NAME_ATTRIBUTE = "name";
    private static final String LIBRARIES_ELEMENT = "libraries";

    private SdkModuleInheritableNamedPointerImpl myPointer;

    protected LanguageVersion myLanguageVersion = StandardJavaScriptVersions.getInstance().getDefaultVersion();

    protected Set<String> myStandardLibraries = new LinkedHashSet<>(DEFAULT_LIBRARIES);

    public ClientJavaScriptModuleExtension(String id, ModuleRootLayer rootLayer) {
        super(id, rootLayer);
        myPointer = new SdkModuleInheritableNamedPointerImpl(rootLayer, id) {
            @Nullable
            @Override
            public String getItemNameFromModule(Module module) {
                ClientJavaScriptModuleExtension extension = ModuleUtilCore.getExtension(module, ClientJavaScriptModuleExtension.class);
                if (extension == null) {
                    return null;
                }
                return extension.getSdkName();
            }

            @Nullable
            @Override
            public Sdk getItemFromModule(Module module) {
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
    protected void loadStateImpl(Element element) {
        super.loadStateImpl(element);
        myLanguageVersion = StandardJavaScriptVersions.getInstance().findVersionById(element.getAttributeValue("language-version"));

        // the SDK of the module, else the predefined standard library
        myPointer.fromXml(element);
        if (myPointer.getName() == null) {
            myPointer.set(null, SdkTable.getInstance().findPredefinedSdkByType(ClientJavaScriptSdkType.getInstance()));
        }

        myStandardLibraries = new LinkedHashSet<>(DEFAULT_LIBRARIES);
        Element libraries = element.getChild(LIBRARIES_ELEMENT);
        if (libraries != null) {
            myStandardLibraries.clear();
            for (Element library : libraries.getChildren(LIBRARY_ELEMENT)) {
                String name = library.getAttributeValue(NAME_ATTRIBUTE);
                if (name != null) {
                    myStandardLibraries.add(name);
                }
            }
        }
    }

    @Override
    protected void getStateImpl(Element element) {
        super.getStateImpl(element);
        if (myLanguageVersion != StandardJavaScriptVersions.getInstance().getDefaultVersion()) {
            element.setAttribute("language-version", myLanguageVersion.getId());
        }
        myPointer.toXml(element);
        if (!myStandardLibraries.equals(DEFAULT_LIBRARIES)) {
            Element libraries = new Element(LIBRARIES_ELEMENT);
            for (String name : myStandardLibraries) {
                libraries.addContent(new Element(LIBRARY_ELEMENT).setAttribute(NAME_ATTRIBUTE, name));
            }
            element.addContent(libraries);
        }
    }

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

    @Override
    public Class<? extends SdkType> getSdkTypeClass() {
        return ClientJavaScriptSdkType.class;
    }

    @Override
    public LanguageVersion getLanguageVersion() {
        return myLanguageVersion;
    }

    /**
     * The browser libraries of the TypeScript standard library: the DOM ({@link #DOM_LIBRARIES}) and the Windows Script
     * Host by default, like tsc.
     */
    @Override
    public Set<String> getStandardLibraries() {
        return Collections.unmodifiableSet(myStandardLibraries);
    }

    @RequiredReadAction
    @Override
    public void commit(ClientJavaScriptModuleExtension mutableModuleExtension) {
        super.commit(mutableModuleExtension);
        myPointer.set(mutableModuleExtension.getInheritableSdk());
        myLanguageVersion = mutableModuleExtension.getLanguageVersion();
        myStandardLibraries = new LinkedHashSet<>(mutableModuleExtension.getStandardLibraries());
    }
}
