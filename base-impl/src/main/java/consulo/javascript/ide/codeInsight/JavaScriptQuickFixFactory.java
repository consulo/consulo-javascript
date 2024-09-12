/*
 * Copyright 2013-2016 must-be.org
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

package consulo.javascript.ide.codeInsight;

import com.intellij.lang.javascript.inspections.qucikFixes.CreateJSFunctionOrMethodFix;
import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ExtensionAPI;
import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.javascript.language.JavaScriptLanguageVersion;
import consulo.language.psi.PsiElement;
import consulo.language.version.LanguageVersion;

import jakarta.annotation.Nonnull;

import java.util.List;

/**
 * @author VISTALL
 * @since 23.02.2016
 */
@ExtensionAPI(ComponentScope.APPLICATION)
public abstract class JavaScriptQuickFixFactory {
    private static final JavaScriptQuickFixFactory ourDefaultImpl = new JavaScriptQuickFixFactory() {
        @Override
        public boolean isMyVersion(@Nonnull JavaScriptLanguageVersion version) {
            return true;
        }
    };

    @Nonnull
    public static JavaScriptQuickFixFactory byElement(PsiElement element) {
        LanguageVersion languageVersion = element.getLanguageVersion();
        if (languageVersion instanceof BaseJavaScriptLanguageVersion jsVersion) {
            List<JavaScriptQuickFixFactory> extensionList = element.getProject()
                .getApplication()
                .getExtensionList(JavaScriptQuickFixFactory.class);
            for (JavaScriptQuickFixFactory factory : extensionList) {
                if (factory.isMyVersion(jsVersion)) {
                    return factory;
                }
            }
        }
        return ourDefaultImpl;
    }

    public CreateJSFunctionOrMethodFix createFunctionOrMethodFix(String referenceName, boolean isMethod) {
        return new CreateJSFunctionOrMethodFix(referenceName, isMethod);
    }

    public abstract boolean isMyVersion(@Nonnull JavaScriptLanguageVersion version);
}
