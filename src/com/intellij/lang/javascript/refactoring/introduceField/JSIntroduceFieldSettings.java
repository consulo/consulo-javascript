package com.intellij.lang.javascript.refactoring.introduceField;

import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.refactoring.BaseIntroduceSettings;

/**
 * @author ven
 */
public interface JSIntroduceFieldSettings extends BaseIntroduceSettings {
  JSAttributeList.AccessType getAccessType();

  enum InitializationPlace {
    CurrentMethod, FieldDeclaration, Constructor
  }

  InitializationPlace getInitializationPlace();
}