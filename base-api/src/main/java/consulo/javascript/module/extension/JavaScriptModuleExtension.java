package consulo.javascript.module.extension;

import consulo.language.version.LanguageVersion;
import consulo.module.extension.ModuleExtensionWithSdk;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public interface JavaScriptModuleExtension<T extends JavaScriptModuleExtension<T>> extends ModuleExtensionWithSdk<T> {
    @Nonnull
    LanguageVersion getLanguageVersion();
}
