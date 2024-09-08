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

/*
 * @author max
 */
package com.intellij.lang.javascript.impl.generation;

import com.intellij.lang.javascript.impl.validation.BaseCreateMethodsFix;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.codeEditor.Editor;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.action.LanguageCodeInsightActionHandler;
import consulo.language.editor.generation.ClassMember;
import consulo.language.editor.generation.MemberChooserBuilder;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.logging.Logger;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.undoRedo.CommandProcessor;
import jakarta.annotation.Nonnull;

import javax.swing.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

abstract class BaseJSGenerateHandler implements LanguageCodeInsightActionHandler {
    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }

    @Override
    @RequiredUIAccess
    public void invoke(@Nonnull final Project project, @Nonnull final Editor editor, @Nonnull final PsiFile file) {
        JSClass clazz = findClass(file, editor);

        if (clazz == null) {
            return;
        }

        final Collection<JSNamedElementNode> candidates = new ArrayList<>();
        collectCandidates(clazz, candidates);

        MemberChooserBuilder<JSNamedElementNode> builder =
            MemberChooserBuilder.create(candidates.toArray(new JSNamedElementNode[candidates.size()]));
        if (canHaveEmptySelectedElements()) {
            builder.withEmptySelection();
        }

        builder.withTitle(getTitle());
        builder.showAsync(
            project,
            dataHolder -> {
                List data = dataHolder.getUserData(ClassMember.KEY_OF_LIST);

                run(clazz, data, project, editor, file);
            }
        );
    }

    private void run(JSClass clazz, Collection<JSNamedElementNode> selectedElements, Project project, Editor editor, PsiFile file) {
        final JSClass jsClass = clazz;
        final Collection<JSNamedElementNode> selectedElements1 = selectedElements;
        Runnable runnable = new Runnable() {
            @Override
            @RequiredUIAccess
            public void run() {
                project.getApplication().runWriteAction(new Runnable() {
                    @Override
                    public void run() {
                        try {
                            final BaseCreateMethodsFix createMethodsFix = createFix(jsClass);
                            createMethodsFix.addElementsToProcessFrom(selectedElements1);
                            createMethodsFix.invoke(project, editor, file);
                        }
                        catch (IncorrectOperationException ex) {
                            Logger.getInstance(getClass().getName()).error(ex);
                        }
                    }
                });
            }
        };

        CommandProcessor processor = CommandProcessor.getInstance();
        if (processor.hasCurrentCommand()) {
            processor.executeCommand(project, runnable, getClass().getName(), null);
        }
        else {
            runnable.run();
        }
    }

    protected void appendOwnOptions(List<JComponent> jComponentList) {
    }

    protected boolean canHaveEmptySelectedElements() {
        return false;
    }

    @RequiredReadAction
    static JSClass findClass(PsiFile file, Editor editor) {
        final PsiElement at = file.findElementAt(editor.getCaretModel().getOffset());
        if (at == null) {
            return null;
        }

        JSClass clazz = PsiTreeUtil.getParentOfType(at, JSClass.class);
        if (clazz == null) {
            final PsiFile containingFile = at.getContainingFile();
            final PsiElement element = JSResolveUtil.getClassReferenceForXmlFromContext(containingFile);
            if (element instanceof JSClass jsClass) {
                clazz = jsClass;
            }
        }
        else if (JSResolveUtil.isArtificialClassUsedForReferenceList(clazz)) {
            clazz = null;
        }

        return clazz;
    }

    protected abstract LocalizeValue getTitle();

    protected abstract BaseCreateMethodsFix createFix(JSClass clazz);

    protected abstract void collectCandidates(final JSClass clazz, final Collection<JSNamedElementNode> candidates);

    @Override
    public boolean startInWriteAction() {
        return false;
    }

    @Override
    public boolean isValidFor(final Editor editor, final PsiFile file) {
        return true;
    }
}