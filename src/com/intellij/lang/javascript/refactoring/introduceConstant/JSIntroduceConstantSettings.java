package com.intellij.lang.javascript.refactoring.introduceConstant;

import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.refactoring.BaseIntroduceSettings;

/**
 * @author ven
 */
public interface JSIntroduceConstantSettings extends BaseIntroduceSettings {
  JSAttributeList.AccessType getAccessType();
}