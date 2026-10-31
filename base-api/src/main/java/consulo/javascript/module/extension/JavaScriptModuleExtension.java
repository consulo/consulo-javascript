package consulo.javascript.module.extension;

import consulo.language.version.LanguageVersion;
import consulo.module.extension.ModuleExtensionWithSdk;

import java.util.Set;

/**
 * @author VISTALL
 * @since 2014-06-29
 */
public interface JavaScriptModuleExtension<T extends JavaScriptModuleExtension<T>> extends ModuleExtensionWithSdk<T> {
    LanguageVersion getLanguageVersion();

    /**
     * The libraries of the environment the code of the module runs in, next to the ECMAScript library of its language
     * level: names of the TypeScript standard library ({@code dom}, {@code dom.iterable}, {@code webworker}...). Web
     * contributes the browser libraries, a runtime with its own globals (Node.js) none. The libraries of a module are
     * the union of its JavaScript extensions ({@link JavaScriptModuleExtensions}).
     */
    default Set<String> getStandardLibraries() {
        return Set.of();
    }
}
