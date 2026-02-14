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

package com.intellij.lang.javascript.impl.findUsages;

import com.intellij.lang.javascript.psi.*;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.Language;
import consulo.language.cacheBuilder.WordsScanner;
import consulo.language.findUsage.FindUsagesProvider;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNamedElement;
import consulo.xml.psi.xml.XmlTag;
import consulo.xml.psi.xml.XmlToken;

import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-02-14
 */
@ExtensionImpl
public class JavaScriptFindUsagesProvider implements FindUsagesProvider {
    @Override
    public boolean canFindUsagesFor(@Nonnull PsiElement psiElement) {
        return psiElement instanceof PsiNamedElement;
    }

    @Override
    @Nonnull
    public String getType(@Nonnull PsiElement element) {
        if (element instanceof JSFunction) {
            return JavaScriptLocalize.javascriptLanguageTermFunction().get();
        }
        if (element instanceof JSClass) {
            return JavaScriptLocalize.javascriptLanguageTermClass().get();
        }
        if (element instanceof JSNamespaceDeclaration) {
            return JavaScriptLocalize.javascriptLanguageTermNamespace().get();
        }
        if (element instanceof JSParameter) {
            return JavaScriptLocalize.javascriptLanguageTermParameter().get();
        }
        if (element instanceof JSProperty) {
            return JavaScriptLocalize.javascriptLanguageTermProperty().get();
        }
        if (element instanceof JSVariable) {
            return JavaScriptLocalize.javascriptLanguageTermVariable().get();
        }
        if (element instanceof JSLabeledStatement) {
            return JavaScriptLocalize.javascriptLanguageTermLabel().get();
        }
        if (element instanceof JSDefinitionExpression) {
            return JavaScriptLocalize.javascriptLanguageTermValue().get();
        }
        if (element instanceof XmlTag) {
            return JavaScriptLocalize.javascriptLanguageTermTag().get();
        }
        if (element instanceof XmlToken) {
            return JavaScriptLocalize.javascriptLanguageTermAttributeValue().get();
        }
        if (element instanceof JSPackageStatement) {
            return JavaScriptLocalize.javascriptLanguageTermPackage().get();
        }
        return "";
    }

    @Override
    @Nonnull
    public String getDescriptiveName(@Nonnull PsiElement element) {
        String name = ((PsiNamedElement)element).getName();
        return name != null ? name : "";
    }

    @Override
    @Nonnull
    public String getNodeText(@Nonnull PsiElement element, boolean useFullName) {
        return getDescriptiveName(element);
    }

    @Override
    public WordsScanner getWordsScanner() {
        return new JSWordsScanner();
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
