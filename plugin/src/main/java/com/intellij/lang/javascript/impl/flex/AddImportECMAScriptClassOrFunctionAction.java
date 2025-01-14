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

package com.intellij.lang.javascript.impl.flex;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.impl.JSPsiImplUtils;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.application.Application;
import consulo.codeEditor.Editor;
import consulo.codeEditor.EditorPopupHelper;
import consulo.document.util.TextRange;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.hint.HintManager;
import consulo.language.editor.hint.QuestionAction;
import consulo.language.editor.inspection.LocalQuickFix;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.editor.intention.HintAction;
import consulo.language.editor.ui.PopupNavigationUtil;
import consulo.language.editor.ui.PsiElementListCellRenderer;
import consulo.language.inject.InjectedLanguageManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiPolyVariantReference;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.resolve.PsiElementProcessor;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.language.util.ModuleUtilCore;
import consulo.localize.LocalizeValue;
import consulo.module.Module;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.ex.popup.JBPopup;
import consulo.undoRedo.CommandProcessor;
import consulo.util.collection.Sets;
import consulo.util.collection.SmartList;
import consulo.util.lang.StringUtil;
import jakarta.annotation.Nonnull;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

/**
 * @author Maxim.Mossienko
 * @since 2008-04-25
 */
public class AddImportECMAScriptClassOrFunctionAction implements HintAction, QuestionAction, LocalQuickFix {
    private final PsiPolyVariantReference myReference;
    private Editor myEditor;
    private boolean isAvailable;
    private boolean isAvailableCalculated;
    private long modificationCount = -1;
    private String calculatedClass;
    private static final String HAS_MORE_SUFFIX = ", ...";
    private boolean isFunction;

    public AddImportECMAScriptClassOrFunctionAction(Editor editor, PsiPolyVariantReference psiReference) {
        myReference = psiReference;
        myEditor = editor;
    }

    @Override
    @RequiredUIAccess
    public boolean showHint(@Nonnull Editor editor) {
        myEditor = editor;
        final PsiElement element = myReference.getElement();
        TextRange textRange = InjectedLanguageManager.getInstance(element.getProject()).injectedToHost(element, element.getTextRange());
        HintManager.getInstance().showQuestionHint(editor, getText(), textRange.getStartOffset(), textRange.getEndOffset(), this);
        return true;
    }

    @Override
    @Nonnull
    public String getText() {
        String className = StringUtil.notNullize(calculatedClass);
        return isFunction
            ? JavaScriptLocalize.flexImportFunction(className).get()
            : JavaScriptLocalize.flexImportClass(className).get();
    }

    @Override
    @Nonnull
    public String getName() {
        return getText();
    }

    @Override
    @Nonnull
    public String getFamilyName() {
        return getText();
    }

    @Override
    @RequiredWriteAction
    public void applyFix(@Nonnull Project project, @Nonnull ProblemDescriptor descriptor) {
        invoke(project, myEditor, descriptor.getPsiElement().getContainingFile());
    }

    @Override
    @RequiredReadAction
    public boolean isAvailable(@Nonnull Project project, Editor editor, PsiFile file) {
        if (!myReference.getElement().isValid()) {
            return false;
        }
        final long modL = myReference.getElement().getManager().getModificationTracker().getModificationCount();

        if (!isAvailableCalculated || modL != modificationCount) {
            final ResolveResult[] results = myReference.multiResolve(false);
            boolean hasValidResult = false;

            for (ResolveResult r : results) {
                if (r.isValidResult()) {
                    hasValidResult = true;
                    break;
                }
            }

            if (!hasValidResult) {
                final Collection<JSQualifiedNamedElement> candidates = getCandidates(editor, file);

                isAvailableCalculated = true;
                isAvailable = candidates.size() > 0;
                if (isAvailable) {
                    JSQualifiedNamedElement element = candidates.iterator().next();
                    calculatedClass = element.getQualifiedName();
                    isFunction = element instanceof JSFunction;
                    if (candidates.size() > 1) {
                        calculatedClass += HAS_MORE_SUFFIX;
                    }
                }
                else {
                    calculatedClass = "";
                }
            }
            else {
                isAvailableCalculated = true;
                isAvailable = false;
            }

            modificationCount = modL;
        }

        return isAvailable;
    }

    @RequiredReadAction
    private Collection<JSQualifiedNamedElement> getCandidates(Editor editor, PsiFile file) {
        final Collection<JSQualifiedNamedElement> candidates;

        if (myReference instanceof JSReferenceExpression referenceExpression && referenceExpression.getQualifier() == null) {
            Collection<JSQualifiedNamedElement> c = getCandidates(editor, file, myReference.getCanonicalText());
            filterDefaultPackage(c);
            candidates = Sets.newHashSet(c, JSPsiImplUtils.QUALIFIED_NAME_HASHING_STRATEGY);
        }
        else {
            JSQualifiedNamedElement invalidResult = null;

            for (ResolveResult r : myReference.multiResolve(false)) {
                PsiElement element = r.getElement();
                if (element instanceof JSQualifiedNamedElement qualifiedNamedElement) {
                    invalidResult = qualifiedNamedElement;
                }
            }
            if (invalidResult != null) {
                candidates = new SmartList<>();
                candidates.add(invalidResult);
            }
            else {
                candidates = Collections.emptyList();
            }
        }
        return candidates;
    }

    @RequiredReadAction
    public static Collection<JSQualifiedNamedElement> getCandidates(Editor editor, PsiFile file, String name) {
        final Module element = ModuleUtilCore.findModuleForPsiElement(file);
        if (element != null) {
            return JSResolveUtil.findElementsByName(
                name,
                editor.getProject(),
                GlobalSearchScope.moduleWithDependenciesAndLibrariesScope(element, false)
            );
        }
        else {
            return Collections.emptyList();
        }
    }

    @Override
    @RequiredWriteAction
    public void invoke(@Nonnull final Project project, final Editor editor, final PsiFile file) {
        final Collection<JSQualifiedNamedElement> candidates = getCandidates(editor, file);

        if (candidates.size() > 0) {
            if (candidates.size() > 1) {
                JBPopup popup = PopupNavigationUtil.getPsiElementPopup(
                    candidates.toArray(new JSQualifiedNamedElement[candidates.size()]),
                    new PsiElementListCellRenderer<>() {
                        @Override
                        @RequiredReadAction
                        public String getElementText(final JSQualifiedNamedElement element) {
                            return element.getName();
                        }

                        @Override
                        @RequiredReadAction
                        protected String getContainerText(final JSQualifiedNamedElement element, final String name) {
                            final String qName = element.getQualifiedName();
                            final String elementName = element.getName();
                            String s = qName.equals(elementName)
                                ? ""
                                : qName.substring(0, qName.length() - elementName.length() - 1);
                            if ("".equals(s)) {
                                s = element.getContainingFile().getName();
                            }
                            return "( " + s + " )";
                        }

                        @Override
                        protected int getIconFlags() {
                            return 0;
                        }
                    },
                    JavaScriptLocalize.chooseClassTitle().get(),
                    new PsiElementProcessor<>() {
                        @Override
                        public boolean execute(@Nonnull JSQualifiedNamedElement element) {
                            CommandProcessor.getInstance().newCommand()
                                .project(project)
                                .name(LocalizeValue.ofNullable(getClass().getName()))
                                .groupId(this)
                                .run(() -> doImport(editor, element.getQualifiedName()));

                            return false;
                        }
                    }
                );

                EditorPopupHelper.getInstance().showPopupInBestPositionFor(editor, popup);
            }
            else {
                doImport(editor, candidates.iterator().next().getQualifiedName());
            }
        }
    }

    @RequiredWriteAction
    private void doImport(Editor editor, final String qName) {
        Application.get().runWriteAction(() -> {
            final PsiElement element = myReference.getElement();
            ImportUtils.doImport(element, qName);
        });
    }

    @Override
    public boolean startInWriteAction() {
        return false;
    }

    @Override
    @RequiredWriteAction
    public boolean execute() {
        final PsiFile containingFile = myReference.getElement().getContainingFile();
        invoke(containingFile.getProject(), myEditor, containingFile);

        return true;
    }

    @RequiredReadAction
    private static void filterDefaultPackage(Collection<JSQualifiedNamedElement> candidates) {
        for (Iterator<JSQualifiedNamedElement> i = candidates.iterator(); i.hasNext(); ) {
            if (!i.next().getQualifiedName().contains(".")) {
                i.remove();
            }
        }
    }
}
