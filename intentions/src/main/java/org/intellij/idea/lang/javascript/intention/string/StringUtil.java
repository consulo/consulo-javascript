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
package org.intellij.idea.lang.javascript.intention.string;

import com.intellij.lang.javascript.psi.JSLiteralExpression;

class StringUtil {
    public static final char SIMPLE_QUOTE = '\'';
    public static final char DOUBLE_QUOTE = '"';
    public static final char BACKSLASH = '\\';

    private StringUtil() {
    }

    public static boolean isSimpleQuoteStringLiteral(JSLiteralExpression expression) {
        String value = expression.getText();

        return (value != null &&
            value.charAt(0) == SIMPLE_QUOTE &&
            value.charAt(value.length() - 1) == SIMPLE_QUOTE);
    }

    public static boolean isDoubleQuoteStringLiteral(JSLiteralExpression expression) {
        String value = expression.getText();

        return (value != null &&
            value.charAt(0) == DOUBLE_QUOTE &&
            value.charAt(value.length() - 1) == DOUBLE_QUOTE);
    }

}
