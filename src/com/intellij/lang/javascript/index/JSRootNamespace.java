package com.intellij.lang.javascript.index;

import org.jetbrains.annotations.NotNull;

/**
 * @by maxim
 */
class JSRootNamespace extends JSNamespace {
  final JSIndexEntry myEntry;

  public JSRootNamespace(JSPackage _package, @NotNull JSIndexEntry entry) {
    super(_package);
    myEntry = entry;
  }

  public int getQualifiedNameId(final JavaScriptIndex index) {
    return -1;
  }

  void validate() {
    getPackage().addInstance(this);
  }
}