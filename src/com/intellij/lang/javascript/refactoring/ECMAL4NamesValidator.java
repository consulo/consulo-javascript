/*
 * @author max
 */
package com.intellij.lang.javascript.refactoring;

import com.intellij.lang.javascript.ECMAL4LanguageDialect;

public class ECMAL4NamesValidator extends JSNamesValidator {
  public ECMAL4NamesValidator() {
    super(ECMAL4LanguageDialect.DIALECT_OPTION_HOLDER);
  }
}