package org.mustbe.consulo.javascript.module.extension;

import org.consulo.module.extension.ModuleExtensionWithSdk;
import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import com.intellij.lang.LanguageVersion;

/**
 * @author VISTALL
 * @since 29.06.14
 */
public interface JavaScriptModuleExtension<T extends JavaScriptModuleExtension<T>> extends ModuleExtensionWithSdk<T>
{
	@NotNull
	LanguageVersion<JavaScriptLanguage> getLanguageVersion();
}
