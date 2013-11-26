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
package com.intellij.lang.javascript.highlighting;

import com.intellij.lang.BracePair;
import com.intellij.lang.PairedBraceMatcher;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Feb 2, 2005
 * Time: 12:10:44 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSBraceMatcher implements PairedBraceMatcher {
  private static final BracePair[] PAIRS = new BracePair[] {
    new BracePair(JSTokenTypes.LPAR, JSTokenTypes.RPAR, false),
    new BracePair(JSTokenTypes.LBRACKET, JSTokenTypes.RBRACKET, false),
    new BracePair(JSTokenTypes.LBRACE, JSTokenTypes.RBRACE, true)
  };

  public BracePair[] getPairs() {
    return PAIRS;
  }

  public boolean isPairedBracesAllowedBeforeType(@NotNull final IElementType lbraceType, @Nullable final IElementType tokenType) {
    return JSTokenTypes.WHITE_SPACE  == tokenType
            || JSTokenTypes.COMMENTS.contains(tokenType)
            || tokenType == JSTokenTypes.SEMICOLON
            || tokenType == JSTokenTypes.COMMA
            || tokenType == JSTokenTypes.RPAR
            || tokenType == JSTokenTypes.RBRACKET
            || tokenType == JSTokenTypes.RBRACE
            || null == tokenType;
  }

  public int getCodeConstructStart(final PsiFile file, int openingBraceOffset) {
    return openingBraceOffset;
  }
}
