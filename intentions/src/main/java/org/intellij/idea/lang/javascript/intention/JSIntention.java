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

import consulo.codeEditor.Editor;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.intention.PsiElementBaseIntentionAction;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import consulo.xml.psi.xml.XmlElement;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public abstract class JSIntention extends PsiElementBaseIntentionAction {
  private static final String INTENTION_SUFFIX = "Intention";
  private static final String DISPLAY_NAME = ".display-name";
  private static final String PACKAGE_NAME = JSIntention.class.getPackage().getName();

  private final JSElementPredicate predicate;

  protected JSIntention() {
    this.predicate = this.getElementPredicate();
  }

  @Override
  public void invoke(@Nonnull Project project, Editor editor, @Nonnull PsiElement element) throws IncorrectOperationException {
    PsiElement matchingElement = findMatchingElement(element);
    if (matchingElement == null) {
      return;
    }
    processIntention(matchingElement);
  }

  protected abstract void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException;

  @Nonnull
  protected abstract JSElementPredicate getElementPredicate();

  @Nullable
  protected PsiElement findMatchingElement(@Nullable PsiElement element) {
    if (element == null || element instanceof PsiFile) {
      return null;
    }

    final Language language = element.getLanguage();
    if (language != Language.ANY && language != JavaScriptLanguage.INSTANCE) {
      return null;
    }

    while (element != null) {
      if (this.predicate.satisfiedBy(element)) {
        return element;
      }
      element = element.getParent();
      if (element instanceof PsiFile || element instanceof XmlElement) {
        break;
      }
    }
    return null;
  }


  @Override
  public boolean isAvailable(@Nonnull Project project, Editor editor, @Nullable PsiElement element) {
    return element != null && findMatchingElement(element) != null;
  }

  @Override
  public boolean startInWriteAction() {
    return true;
  }

  protected String getTextKey(@NonNls Object... suffixes) {
    return JSIntentionBundle.getKey(this.getClass().getName().substring(PACKAGE_NAME.length() + 1).replace("JS", ""),
                                    INTENTION_SUFFIX,
                                    null,
                                    suffixes);
  }

  @Override
  @SuppressWarnings({"UnresolvedPropertyKey"})
  @Nonnull
  public String getText() {
    return JSIntentionBundle.message(this.getTextKey(DISPLAY_NAME));
  }

  @SuppressWarnings({"UnresolvedPropertyKey"})
  public String getText(@NonNls Object... arguments) {
    return JSIntentionBundle.message(this.getTextKey(DISPLAY_NAME), arguments);
  }

  @SuppressWarnings({"UnresolvedPropertyKey"})
  protected String getSuffixedDisplayName(@NonNls String suffix, @NonNls Object... arguments) {
    return JSIntentionBundle.message(this.getTextKey(DISPLAY_NAME, '.', suffix), arguments);
  }
}
