/*
 * Copyright 2013-2015 must-be.org
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

package consulo.javascript.language.psi;

import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2015-12-13
 */
public class JavaScriptPrimitiveType implements JavaScriptType {
    public static final JavaScriptPrimitiveType BOOL = new JavaScriptPrimitiveType("Boolean");
    public static final JavaScriptPrimitiveType STRING = new JavaScriptPrimitiveType("String");
    public static final JavaScriptPrimitiveType NUMBER = new JavaScriptPrimitiveType("Number");
    public static final JavaScriptPrimitiveType REGEXP = new JavaScriptPrimitiveType("Regexp");
    public static final JavaScriptPrimitiveType NULL = new JavaScriptPrimitiveType("null");

    private String myName;

    public JavaScriptPrimitiveType(String name) {
        myName = name;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public String getPresentableText() {
        return myName;
    }

    @Nullable
    @Override
    public PsiElement getTargetElement() {
        return null;
    }

    @Override
    public String toString() {
        return "JavaScriptPrimitiveType: " + myName;
    }
}
