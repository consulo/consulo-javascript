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

package com.intellij.lang.javascript.psi;

import consulo.annotation.access.RequiredReadAction;
import org.jspecify.annotations.Nullable;

/**
 * @author max
 * @since 2005-01-30
 */
public interface JSLiteralExpression extends JSExpression {
    /**
     * The value of a string literal without quotes ({@code 'a'}, {@code "a"}, a template literal without
     * substitutions), escapes resolved; null for other literals.
     */
    @Nullable
    @RequiredReadAction
    default String getStringValue() {
        String text = getText();
        if (text == null || text.length() < 2) {
            return null;
        }
        char quote = text.charAt(0);
        if ((quote != '\'' && quote != '"' && quote != '`') || text.charAt(text.length() - 1) != quote) {
            return null;
        }
        String content = text.substring(1, text.length() - 1);
        if (quote == '`' && content.contains("${")) {
            return null;
        }
        return unescape(content);
    }

    /**
     * Whether the literal is a quoted string ({@code '...'} or {@code "..."}).
     */
    @RequiredReadAction
    default boolean isQuotedLiteral() {
        String text = getText();
        if (text == null || text.length() < 2) {
            return false;
        }
        char quote = text.charAt(0);
        return (quote == '\'' || quote == '"') && text.charAt(text.length() - 1) == quote;
    }

    private static boolean isHex(String text) {
        for (int i = 0; i < text.length(); i++) {
            if (Character.digit(text.charAt(i), 16) < 0) {
                return false;
            }
        }
        return true;
    }

    private static String unescape(String content) {
        if (content.indexOf('\\') < 0) {
            return content;
        }
        StringBuilder result = new StringBuilder(content.length());
        for (int i = 0; i < content.length(); i++) {
            char c = content.charAt(i);
            if (c != '\\' || i + 1 == content.length()) {
                result.append(c);
                continue;
            }
            char next = content.charAt(++i);
            switch (next) {
                case 'n' -> result.append('\n');
                case 't' -> result.append('\t');
                case 'r' -> result.append('\r');
                case 'b' -> result.append('\b');
                case 'f' -> result.append('\f');
                case 'v' -> result.append('\u000B');
                case '0' -> result.append('\0');
                case 'u' -> {
                    if (i + 4 < content.length() && isHex(content.substring(i + 1, i + 5))) {
                        result.append((char) Integer.parseInt(content.substring(i + 1, i + 5), 16));
                        i += 4;
                    }
                    else {
                        result.append(next);
                    }
                }
                case '\n' -> {
                    // line continuation
                }
                default -> result.append(next);
            }
        }
        return result.toString();
    }
}
