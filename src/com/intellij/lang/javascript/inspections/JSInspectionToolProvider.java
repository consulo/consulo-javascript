package com.intellij.lang.javascript.inspections;

import com.intellij.codeInspection.InspectionToolProvider;

/**
 * @author yole
 */
public class JSInspectionToolProvider implements InspectionToolProvider {
  public Class[] getInspectionClasses() {
    return new Class[] {
      JSUnresolvedVariableInspection.class,
      JSUndeclaredVariableInspection.class,
      JSUntypedDeclarationInspection.class,
      JSUnresolvedFunctionInspection.class,
      JSDuplicatedDeclarationInspection.class,
      JSDeprecatedSymbolsInspection.class,
      JSUnusedLocalSymbolsInspection.class
    };
  }
}
