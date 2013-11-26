/*
 * @author max
 */
package com.intellij.lang.javascript.refactoring;

import com.intellij.lang.javascript.GwtLanguageDialect;

public class GwtNamesValidator extends JSNamesValidator {
  public GwtNamesValidator() {
    super(GwtLanguageDialect.DIALECT_OPTION_HOLDER);
  }
}