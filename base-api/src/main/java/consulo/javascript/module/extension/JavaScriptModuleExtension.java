package consulo.javascript.module.extension;

import javax.annotation.Nonnull;

import consulo.lang.LanguageVersion;
import consulo.module.extension.ModuleExtensionWithSdk;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public interface JavaScriptModuleExtension<T extends JavaScriptModuleExtension<T>> extends ModuleExtensionWithSdk<T>
{
	@Nonnull
	LanguageVersion getLanguageVersion();
}
