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

package com.intellij.lang.javascript.impl.refactoring;

import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JSFlexAdapter;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.language.lexer.Lexer;
import consulo.project.Project;
import consulo.language.editor.refactoring.NamesValidator;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Jun 27, 2006
 * Time: 8:28:35 PM
 * To change this template use File | Settings | File Templates.
 */
abstract class JSNamesValidator implements NamesValidator {
    protected final Lexer myLexer;

    JSNamesValidator(DialectOptionHolder optionHolder) {
        myLexer = createLexer(optionHolder);
    }

    protected Lexer createLexer(final DialectOptionHolder optionHolder) {
        return new JSFlexAdapter(false, optionHolder);
    }

    @Override
    public synchronized boolean isKeyword(String name, Project project) {
        myLexer.start(name, 0, name.length(), 0);
        return JSTokenTypes.KEYWORDS.contains(myLexer.getTokenType()) && myLexer.getTokenEnd() == name.length();
    }

    @Override
    public synchronized boolean isIdentifier(String name, Project project) {
        myLexer.start(name, 0, name.length(), 0);
        return JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(myLexer.getTokenType()) && myLexer.getTokenEnd() == name.length();
    }
}
