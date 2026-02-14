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

package com.intellij.lang.javascript.index;

import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;

/**
 * @author maxim, yole
 */
@Deprecated
public final class JavaScriptIndex {
    public static final String ECMASCRIPT_JS2 = "ECMAScript.js2";

    public static boolean isFromPredefinedFile(PsiFile containingFile) {
        return false;
    }

    public static PsiElement findSymbolByFileAndNameAndOffset(String fileName, String name, int offset) {
        return null;
    }

    public static PsiElement findSymbolWithNameAndOffsetInEntry(String nameId, int offset) {
        return null;
    }
}
