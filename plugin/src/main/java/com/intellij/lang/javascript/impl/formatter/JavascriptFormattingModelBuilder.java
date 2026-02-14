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

/*
 * @author max
 */
package com.intellij.lang.javascript.impl.formatter;

import com.intellij.lang.javascript.impl.formatter.blocks.JSBlock;
import com.intellij.lang.javascript.psi.JSFile;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.codeStyle.*;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;

import jakarta.annotation.Nonnull;

@ExtensionImpl
public class JavascriptFormattingModelBuilder implements FormattingModelBuilder {
    @Nonnull
    @Override
    public FormattingModel createModel(@Nonnull FormattingContext formattingContext) {
        PsiElement element = formattingContext.getPsiElement();

        CodeStyleSettings settings = formattingContext.getCodeStyleSettings();

        PsiFile psiFile = element.getContainingFile();

        CommonCodeStyleSettings commonSettings = settings.getCommonSettings(JavaScriptLanguage.INSTANCE);

        return new JSFormattingModel(
            psiFile,
            settings,
            new JSBlock(
                psiFile instanceof JSFile jsFile ? jsFile.getNode() : element.getNode(),
                null,
                null,
                null,
                commonSettings
            )
        );
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}