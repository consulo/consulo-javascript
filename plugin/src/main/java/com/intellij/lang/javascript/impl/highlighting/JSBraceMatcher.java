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

package com.intellij.lang.javascript.impl.highlighting;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.BracePair;
import consulo.language.Language;
import consulo.language.PairedBraceMatcher;
import consulo.language.ast.IElementType;
import jakarta.annotation.Nonnull;

import jakarta.annotation.Nullable;

/**
 * User: max
 * Date: Feb 2, 2005
 * Time: 12:10:44 PM
 */
@ExtensionImpl
public class JSBraceMatcher implements PairedBraceMatcher {
    private static final BracePair[] PAIRS = new BracePair[]{
        new BracePair(JSTokenTypes.LPAR, JSTokenTypes.RPAR, false),
        new BracePair(JSTokenTypes.LBRACKET, JSTokenTypes.RBRACKET, false),
        new BracePair(JSTokenTypes.LBRACE, JSTokenTypes.RBRACE, true)
    };

    @Override
    public BracePair[] getPairs() {
        return PAIRS;
    }

    @Override
    public boolean isPairedBracesAllowedBeforeType(@Nonnull final IElementType lbraceType, @Nullable final IElementType tokenType) {
        return JSTokenTypes.WHITE_SPACE == tokenType || JSTokenTypes.COMMENTS.contains(tokenType) || tokenType == JSTokenTypes.SEMICOLON || tokenType ==
            JSTokenTypes.COMMA || tokenType == JSTokenTypes.RPAR || tokenType == JSTokenTypes.RBRACKET || tokenType == JSTokenTypes.RBRACE || null ==
            tokenType;
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
