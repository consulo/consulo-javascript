package com.intellij.lang.javascript;

import com.intellij.lang.Language;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

/**
 * @by Maxim.Mossienko
*/
public abstract class JSLanguageDialect extends Language {
  public JSLanguageDialect(@NonNls @NotNull String id) {
    super(JavaScriptSupportLoader.JAVASCRIPT.getLanguage(), id);
  }

  public abstract @NonNls String getFileExtension();
}
