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
package com.intellij.javascript.manipulators;

import consulo.annotation.access.RequiredWriteAction;
import consulo.document.util.TextRange;
import consulo.language.psi.AbstractElementManipulator;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import consulo.util.lang.StringEscapeUtil;

/**
 * @author peter
 */
abstract class JSAbstractElementManipulator<T extends PsiElement> extends AbstractElementManipulator<T> {
    @Override
    @RequiredWriteAction
    @SuppressWarnings("unchecked")
    public T handleContentChange(T element, TextRange range, String newContent) throws IncorrectOperationException {
        String oldText = element.getText();
        int oldLength = oldText.length();
        StringBuilder newText = new StringBuilder(oldLength);
        newText.append(oldText, 0, range.getStartOffset());
        StringEscapeUtil.escape(newContent, '"', newText);
        newText.append(oldText, range.getEndOffset(), oldLength);

        return (T) element.replace(createTree(newText.toString(), element.getProject()));
    }

    protected abstract T createTree(String newText, Project project);
}
