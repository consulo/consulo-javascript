/*
 * @author max
 */
package com.intellij.lang.javascript.refactoring;

import com.intellij.lang.javascript.JavascriptLanguage;

public class BasicJavascriptNamesValidator extends JSNamesValidator {
  public BasicJavascriptNamesValidator() {
    super(JavascriptLanguage.DIALECT_OPTION_HOLDER);
  }
}