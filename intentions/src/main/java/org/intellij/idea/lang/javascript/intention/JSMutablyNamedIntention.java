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

import javax.annotation.Nonnull;

import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nullable;

public abstract class JSMutablyNamedIntention extends JSIntention {
    private String text;

    @NonNls protected abstract String getTextForElement(PsiElement element);

    @Override
	@Nonnull
	public String getText() {
      return text;
    }

    @Override
	public boolean isAvailable(@Nonnull Project project, Editor editor, @Nullable PsiElement node) {
      final PsiElement element = findMatchingElement(node);
      if (element != null) {
        text = getTextForElement(element);
        return true;
      } else {
        return false;
      }
    }
}
