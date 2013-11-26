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

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.codeInsight.intention.PsiElementBaseIntentionAction;
import com.intellij.lang.Language;
import com.intellij.openapi.editor.CaretModel;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.xml.XmlElement;
import com.intellij.util.IncorrectOperationException;

public abstract class JSIntention extends PsiElementBaseIntentionAction {
    private static Language JS_LANGUAGE;

    private static void initJSLanguage() {
        for(Language lang:Language.getRegisteredLanguages()) {
            if ("JavaScript".equals(lang.getID())) {
                JS_LANGUAGE = lang;
                break;
            }
        }
    }

  @NonNls private static final String INTENTION_SUFFIX = "Intention";
    @NonNls private static final String DISPLAY_NAME     = ".display-name";
    @NonNls private static final String FAMILY_NAME      = ".family-name";
    private         static final String PACKAGE_NAME     = JSIntention.class.getPackage().getName();

    private final JSElementPredicate predicate;

    /** @noinspection AbstractMethodCallInConstructor,OverridableMethodCallInConstructor*/
    protected JSIntention() {
        if (JS_LANGUAGE == null) initJSLanguage();
        this.predicate = this.getElementPredicate();
    }

	@Override
	public void invoke(@NotNull Project project, Editor editor, @NotNull PsiElement element) throws IncorrectOperationException
	{
		this.processIntention(element);
	}

    protected abstract void processIntention(@NotNull PsiElement element)
            throws IncorrectOperationException;

    @NotNull
    protected abstract JSElementPredicate getElementPredicate();

    @Nullable
    private PsiElement findMatchingElement(PsiFile file, Editor editor) {
        final CaretModel caretModel = editor.getCaretModel();
        final int        position   = caretModel.getOffset();
        PsiElement       element    = file.findElementAt(position);

        return findMatchingElement(element);
    }

    @Nullable
    protected PsiElement findMatchingElement(@Nullable PsiElement element) {
        if (element == null || element instanceof PsiFile) {
            return null;
        }

        final Language language = element.getLanguage();
        if (language != Language.ANY && language != JS_LANGUAGE) return null;

        while (element != null) {
            if (this.predicate.satisfiedBy(element)) {
                return element;
            }
            element = element.getParent();
            if (element instanceof PsiFile ||
                element instanceof XmlElement
               ) {
              break;
            }
        }
        return null;
    }


    public boolean isAvailable(@NotNull Project project, Editor editor, @Nullable PsiElement element) {
      return element != null && findMatchingElement(element) != null;
    }

    public boolean startInWriteAction() {
        return true;
    }

    protected String getTextKey(@NonNls Object... suffixes) {
        return JSIntentionBundle.getKey(this.getClass().getName().substring(PACKAGE_NAME.length() + 1).replace("JS", ""),
                                        INTENTION_SUFFIX, null, suffixes);
    }

    @SuppressWarnings({"UnresolvedPropertyKey"})
    @NotNull public String getText() {
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

    @SuppressWarnings({"UnresolvedPropertyKey"})
    @NotNull public String getFamilyName() {
        return JSIntentionBundle.message(this.getTextKey(FAMILY_NAME));
    }
}
