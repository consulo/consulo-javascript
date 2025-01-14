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

package com.intellij.lang.javascript.impl.surroundWith;

import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.surroundWith.SurroundDescriptor;
import consulo.language.editor.surroundWith.Surrounder;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;

/**
 * @author yole
 * @since 2005-07-12
 */
@ExtensionImpl
public class JSExpressionSurroundDescriptor implements SurroundDescriptor {
    private static final Surrounder[] SURROUNDERS = {
        new JSWithParenthesesSurrounder()
    };

    @Override
    @Nonnull
    @RequiredReadAction
    public PsiElement[] getElementsToSurround(PsiFile file, int startOffset, int endOffset) {
        final JSExpression expr = findExpressionInRange(file, startOffset, endOffset);
        if (expr == null) {
            return PsiElement.EMPTY_ARRAY;
        }
        return new PsiElement[]{expr};
    }

    @Override
    @Nonnull
    public Surrounder[] getSurrounders() {
        return SURROUNDERS;
    }

    @Override
    public boolean isExclusive() {
        return false;
    }

    @RequiredReadAction
    private static JSExpression findExpressionInRange(PsiFile file, int startOffset, int endOffset) {
        PsiElement element1 = file.findElementAt(startOffset);
        PsiElement element2 = file.findElementAt(endOffset - 1);
        if (element1 instanceof PsiWhiteSpace whiteSpace) {
            startOffset = whiteSpace.getTextRange().getEndOffset();
        }
        if (element2 instanceof PsiWhiteSpace whiteSpace) {
            endOffset = whiteSpace.getTextRange().getStartOffset();
        }
        JSExpression expression = PsiTreeUtil.findElementOfClassAtRange(file, startOffset, endOffset, JSExpression.class);
        if (expression == null || expression.getTextRange().getEndOffset() != endOffset) {
            return null;
        }
        if (expression instanceof JSReferenceExpression && expression.getParent() instanceof JSCallExpression) {
            return null;
        }
        return expression;
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
