/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.intention;

import java.util.ResourceBundle;

import org.jetbrains.annotations.PropertyKey;
import org.jetbrains.annotations.NonNls;
import org.intellij.idea.lang.javascript.JSAbstractBundle;

import com.intellij.CommonBundle;

public class JSIntentionBundle extends JSAbstractBundle {

    @NonNls
    private static final String         bundleClassName = "org.intellij.idea.lang.javascript.intention.JavaScriptIntentionBundle";
    private static final ResourceBundle ourBundle       = ResourceBundle.getBundle(bundleClassName);

    private JSIntentionBundle() {}

    public static String message(@PropertyKey(resourceBundle = bundleClassName) String key,
                                 Object... params) {
        return CommonBundle.message(ourBundle, key, params);
    }
}
