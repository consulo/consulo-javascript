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

package consulo.javascript.ecmascript.codeInsight;

import com.intellij.lang.javascript.inspections.qucikFixes.CreateJSFunctionOrMethodFix;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.ecmascript.codeInsight.quickFixes.EcmaScript6CreateJSFunctionOrMethodFix;
import consulo.javascript.ecmascript.lang.BaseEcmaScriptJavaScriptVersion;
import consulo.javascript.ide.codeInsight.JavaScriptQuickFixFactory;
import consulo.javascript.language.JavaScriptLanguageVersion;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 24.02.2016
 */
@ExtensionImpl
public class EcmaScript6QuickFixFactory extends JavaScriptQuickFixFactory {
    @Override
    public CreateJSFunctionOrMethodFix createFunctionOrMethodFix(String referenceName, boolean isMethod) {
        return new EcmaScript6CreateJSFunctionOrMethodFix(referenceName, isMethod);
    }

    @Override
    public boolean isMyVersion(@Nonnull JavaScriptLanguageVersion version) {
        return version instanceof BaseEcmaScriptJavaScriptVersion;
    }
}
