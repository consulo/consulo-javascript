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
package org.intellij.idea.lang.javascript;

/**
 * Abstract super class for JS inspection and intention bundles
 */
public abstract class JSAbstractBundle {
    protected JSAbstractBundle() {
    }

    public static String getKey(String className, String classNameSuffix, String alternateClassNameSuffix, Object... suffixes) {
        String name = className;
        int dollarIndex = name.indexOf((int)'$');

        if (dollarIndex >= 0) {
            name = name.substring(0, dollarIndex);
        }
        if (name.endsWith(classNameSuffix)) {
            name = name.substring(0, name.length() - classNameSuffix.length());
        }
        if (alternateClassNameSuffix != null && name.endsWith(alternateClassNameSuffix)) {
            name = name.substring(0, name.length() - alternateClassNameSuffix.length());
        }

        int length = name.length();
        StringBuilder buffer = new StringBuilder(length + 10);
        boolean addWordSeparator = false;

        for (int index = 0; index < length; index++) {
            char c = name.charAt(index);

            if (Character.isUpperCase(c)) {
                if (addWordSeparator) {
                    buffer.append('-');
                    addWordSeparator = false;
                }
                buffer.append(Character.toLowerCase(c));
            }
            else {
                addWordSeparator = (c != '.');
                buffer.append(c);
            }
        }

        for (Object suffix : suffixes) {
            buffer.append(suffix);
        }

        return buffer.toString();
    }
}
