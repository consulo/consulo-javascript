/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.intellij.lang.javascript.findUsages;

import com.intellij.lang.cacheBuilder.WordsScanner;
import com.intellij.lang.findUsages.FindUsagesProvider;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSPackageWrapper;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.xml.XmlTag;
import com.intellij.psi.xml.XmlToken;
import org.jetbrains.annotations.NotNull;
             
/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Feb 14, 2005
 * Time: 6:44:02 PM
 * To change this template use File | Settings | File Templates.
 */
public class JavaScriptFindUsagesProvider implements FindUsagesProvider {
  public boolean canFindUsagesFor(@NotNull PsiElement psiElement) {
    return psiElement instanceof PsiNamedElement;
  }

  public String getHelpId(@NotNull PsiElement psiElement) {
    return null;
  }

  @NotNull
  public String getType(@NotNull PsiElement element) {
    if (element instanceof JSNamedElementProxy) element = ((JSNamedElementProxy)element).getElement();
    if (element instanceof JSFunction) return JSBundle.message("javascript.language.term.function");
    if (element instanceof JSClass) return JSBundle.message("javascript.language.term.class");
    if (element instanceof JSNamespaceDeclaration) return JSBundle.message("javascript.language.term.namespace");
    if (element instanceof JSParameter) return JSBundle.message("javascript.language.term.parameter");
    if (element instanceof JSProperty) return JSBundle.message("javascript.language.term.property");
    if (element instanceof JSVariable) return JSBundle.message("javascript.language.term.variable");
    if (element instanceof JSLabeledStatement) return JSBundle.message("javascript.language.term.label");
    if (element instanceof JSDefinitionExpression) return JSBundle.message("javascript.language.term.value");
    if (element instanceof XmlTag) return JSBundle.message("javascript.language.term.tag");
    if (element instanceof XmlToken) return JSBundle.message("javascript.language.term.attribute.value");
    if (element instanceof JSPackageWrapper || element instanceof JSPackageStatement) return JSBundle.message("javascript.language.term.package");
    return "";
  }

  @NotNull
  public String getDescriptiveName(@NotNull PsiElement element) {
    String name = ((PsiNamedElement)element).getName();
    return name != null ? name:"";
  }

  @NotNull
  public String getNodeText(@NotNull PsiElement element, boolean useFullName) {
    return getDescriptiveName(element);
  }

  public WordsScanner getWordsScanner() {
    return new JSWordsScanner();
  }
}
