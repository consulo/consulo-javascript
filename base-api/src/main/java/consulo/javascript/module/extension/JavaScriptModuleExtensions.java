package consulo.javascript.module.extension;

import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.StandardJavaScriptVersion;
import consulo.language.version.LanguageVersion;
import consulo.module.Module;
import consulo.module.content.ModuleRootManager;
import consulo.module.extension.ModuleExtension;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * The JavaScript settings of a module merged from all its enabled JavaScript extensions: a module can mix several (Web
 * and Node.js for a Vue or Nuxt application). Use it instead of looking up a single {@link JavaScriptModuleExtension};
 * code that needs a concrete SDK (a Node.js runner, a browser debugger) asks for its concrete extension class.
 *
 * @since 2026-10-31
 */
public final class JavaScriptModuleExtensions {
    private JavaScriptModuleExtensions() {
    }

    /**
     * The enabled JavaScript extensions of the module; empty for a module without JavaScript.
     */
    @RequiredReadAction
    public static List<JavaScriptModuleExtension<?>> getExtensions(Module module) {
        List<JavaScriptModuleExtension<?>> result = new ArrayList<>();
        for (ModuleExtension extension : ModuleRootManager.getInstance(module).getExtensions()) {
            if (extension.isEnabled() && extension instanceof JavaScriptModuleExtension<?> javaScriptExtension) {
                result.add(javaScriptExtension);
            }
        }
        return result;
    }

    /**
     * The language level of the module: the highest of its extensions, null without a JavaScript extension.
     */
    @Nullable
    @RequiredReadAction
    @SuppressWarnings("deprecation")
    public static LanguageVersion getLanguageVersion(Module module) {
        LanguageVersion result = null;
        int resultWeight = Integer.MIN_VALUE;
        for (JavaScriptModuleExtension<?> extension : getExtensions(module)) {
            LanguageVersion version = extension.getLanguageVersion();
            // the weight orders the standard versions: ECMAScript 6 is 600, ECMAScript 12 is 1200
            int weight = version instanceof StandardJavaScriptVersion standard ? standard.getWeight() : 0;
            if (result == null || weight > resultWeight) {
                result = version;
                resultWeight = weight;
            }
        }
        return result;
    }

    /**
     * The environment libraries of the module ({@link JavaScriptModuleExtension#getStandardLibraries()}): the union of
     * its extensions.
     */
    @RequiredReadAction
    public static Set<String> getStandardLibraries(Module module) {
        Set<String> result = new LinkedHashSet<>();
        for (JavaScriptModuleExtension<?> extension : getExtensions(module)) {
            result.addAll(extension.getStandardLibraries());
        }
        return result;
    }
}
