package org.mustbe.consulo.javascript.module.extension;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import consulo.lang.LanguageVersion;
import consulo.module.extension.ModuleExtensionWithSdk;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public interface JavaScriptModuleExtension<T extends JavaScriptModuleExtension<T>> extends ModuleExtensionWithSdk<T>
{
	@NotNull
	LanguageVersion<JavaScriptLanguage> getLanguageVersion();
}
