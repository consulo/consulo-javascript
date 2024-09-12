/*
 * Copyright 2013-2015 must-be.org
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

package consulo.json.validation.descriptionByAnotherPsiElement;

import consulo.annotation.access.RequiredReadAction;
import consulo.fileEditor.EditorNotificationBuilder;
import consulo.fileEditor.EditorNotificationProvider;
import consulo.fileEditor.EditorNotifications;
import consulo.fileEditor.FileEditor;
import consulo.json.JsonFileType;
import consulo.json.jom.JomElement;
import consulo.json.jom.JomFileElement;
import consulo.json.jom.JomManager;
import consulo.language.editor.DaemonCodeAnalyzer;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiManager;
import consulo.language.psi.PsiModificationTracker;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import consulo.util.lang.StringUtil;
import consulo.virtualFileSystem.VirtualFile;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.function.Supplier;

/**
 * @author VISTALL
 * @since 12.11.2015
 */
public class DescriptionByAnotherPsiElementEditorNotification<T extends PsiElement> implements EditorNotificationProvider {
    private Project myProject;
    private DescriptionByAnotherPsiElementProvider<T> myProvider;

    public DescriptionByAnotherPsiElementEditorNotification(
        @Nonnull Project project,
        @Nonnull DescriptionByAnotherPsiElementProvider<T> provider
    ) {
        myProject = project;
        myProvider = provider;
    }

    @Nonnull
    @Override
    public String getId() {
        return myProvider.getId();
    }

    @Override
    @Nullable
    @RequiredReadAction
    public EditorNotificationBuilder buildNotification(
        @Nonnull VirtualFile file,
        @Nonnull FileEditor fileEditor,
        @Nonnull Supplier<EditorNotificationBuilder> supplier
    ) {
        if (file.getFileType() != JsonFileType.INSTANCE) {
            return null;
        }

        final PsiFile psiFile = PsiManager.getInstance(myProject).findFile(file);
        if (psiFile == null) {
            return null;
        }

        JomFileElement<JomElement> fileElement = JomManager.getInstance(myProject).getFileElement(psiFile);
        if (fileElement != null) {
            return null;
        }

        if (!myProvider.isAvailable(myProject)) {
            return null;
        }

        String registeredPsiElementId = DescriptionByAnotherPsiElementService.getInstance(myProject).getRegisteredPsiElementId(file);
        if (registeredPsiElementId == null) {
            EditorNotificationBuilder panel = supplier.get();
            panel.withText(LocalizeValue.localizeTODO(StringUtil.SINGLE_QUOTER.apply(myProvider.getId()) + " model description is available for this file"));
            panel.withAction(
                LocalizeValue.localizeTODO("Choose " + myProvider.getPsiElementName()),
                e -> {
                    T chooseElement = myProvider.chooseElement(myProject);
                    if (chooseElement == null) {
                        return;
                    }

                    DescriptionByAnotherPsiElementService.getInstance(myProject).registerFile(file, chooseElement, myProvider);

                    wantUpdate(psiFile);
                }
            );
            return panel;
        }
        else {
            EditorNotificationBuilder panel = supplier.get();
            panel.withText(LocalizeValue.localizeTODO(
                StringUtil.SINGLE_QUOTER.apply(myProvider.getId()) +
                    " model description is registered for this file. " +
                    myProvider.getPsiElementName() + ": " + registeredPsiElementId
            ));
            panel.withAction(
                LocalizeValue.localizeTODO("Cancel"),
                e -> {
                    if(DescriptionByAnotherPsiElementService.getInstance(myProject).removeFile(file)) {
                        wantUpdate(psiFile);
                    }
                }
            );
            return panel;
        }
    }

    private void wantUpdate(PsiFile psiFile) {
        PsiModificationTracker.getInstance(myProject).incCounter();

        DaemonCodeAnalyzer.getInstance(myProject).restart(psiFile);

        EditorNotifications.getInstance(psiFile.getProject()).updateNotifications(psiFile.getVirtualFile());
    }
}
