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

package com.intellij.lang.javascript.impl;

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.Language;
import consulo.language.editor.highlight.SyntaxHighlighter;
import consulo.language.editor.highlight.SyntaxHighlighterFactory;
import consulo.language.editor.template.context.BaseTemplateContextType;
import consulo.language.editor.template.context.TemplateActionContext;
import consulo.language.inject.InjectedLanguageManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiLanguageInjectionHost;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.util.lang.ref.SimpleReference;
import consulo.xml.lang.xml.XMLLanguage;
import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 * @since 2008-04-14
 */
@ExtensionImpl
public class JavaScriptCodeContextType extends BaseTemplateContextType {
    private static final String JAVA_SCRIPT = "JAVA_SCRIPT";

    public JavaScriptCodeContextType() {
        super(JAVA_SCRIPT, JavaScriptLocalize.javascriptTemplateContextType());
    }

    @Override
    @RequiredReadAction
    public boolean isInContext(@Nonnull TemplateActionContext context) {
        PsiFile file = context.getFile();
        int offset = context.getStartOffset();

        PsiElement at = file.findElementAt(offset);
        if (at == null && offset == file.getTextLength()) {
            at = file.findElementAt(offset - 1);
        }
        Language language = at != null ? at.getParent().getLanguage() : null;

        if (language instanceof XMLLanguage) {
            PsiLanguageInjectionHost host = PsiTreeUtil.getParentOfType(at, PsiLanguageInjectionHost.class, false);

            if (host != null) {
                final SimpleReference<Boolean> hasJsInjection = new SimpleReference<>(Boolean.FALSE);

                InjectedLanguageManager.getInstance(at.getProject()).enumerate(
                    host,
                    new JSResolveUtil.JSInjectedFilesVisitor() {
                        @Override
                        protected void process(JSFile file) {
                            hasJsInjection.set(Boolean.TRUE);
                        }
                    }
                );

                if (hasJsInjection.get()) {
                    language = JavaScriptLanguage.INSTANCE;
                }
            }
        }
        return language != null && language.isKindOf(JavaScriptLanguage.INSTANCE);
    }

    @Override
    public SyntaxHighlighter createHighlighter() {
        return SyntaxHighlighterFactory.getSyntaxHighlighter(JavaScriptSupportLoader.ECMA_SCRIPT_L4, null, null);
    }
}
