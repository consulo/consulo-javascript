/*
 * Copyright (c) 2000-2005 by JetBrains s.r.o. All Rights Reserved.
 * Use is subject to license terms.
 */
package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.Language;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.LanguageSubstitutor;
import org.jetbrains.annotations.NotNull;

/**
 * @author peter
 */
public class JSLanguageSubstitor extends LanguageSubstitutor{
  public Language getLanguage(@NotNull final VirtualFile file, @NotNull final Project project) {
    return JavaScriptSupportLoader.getLanguageDialect(file);
  }
}
