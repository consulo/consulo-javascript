package consulo.javascript.module.extension;

import consulo.language.version.LanguageVersion;
import consulo.module.extension.ModuleExtensionWithSdk;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2014-06-29
 */
public interface JavaScriptModuleExtension<T extends JavaScriptModuleExtension<T>> extends ModuleExtensionWithSdk<T> {
    @Nonnull
    LanguageVersion getLanguageVersion();
}
