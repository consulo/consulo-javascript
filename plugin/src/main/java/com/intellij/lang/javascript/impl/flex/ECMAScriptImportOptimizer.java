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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.validation.JSUnusedImportsHelper;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.document.util.TextRange;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.editor.FileModificationService;
import consulo.language.editor.refactoring.ImportOptimizer;
import consulo.language.inject.InjectedLanguageManager;
import consulo.language.psi.*;
import consulo.project.Project;
import consulo.util.collection.MultiMap;
import consulo.util.lang.EmptyRunnable;
import consulo.util.lang.Pair;

import jakarta.annotation.Nonnull;

import java.util.*;

/**
 * @author Maxim.Mossienko
 * Date: Jul 23, 2008
 * Time: 12:10:44 AM
 */
@ExtensionImpl
public class ECMAScriptImportOptimizer implements ImportOptimizer {
    @Override
    public boolean supports(PsiFile file) {
        return file.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4 || JavaScriptSupportLoader.isFlexMxmFile(file);
    }

    @Override
    @Nonnull
    public Runnable processFile(final PsiFile file) {
        if (!FileModificationService.getInstance().prepareFileForWrite(file)) {
            return EmptyRunnable.INSTANCE;
        }

        return new Runnable() {
            @Override
            @RequiredReadAction
            public void run() {
                final JSUnusedImportsHelper.Results unusedImportsResults = JSUnusedImportsHelper.getUnusedImports(file);

                MultiMap<JSElement, String> importsByHolder = new MultiMap<JSElement, String>() {
                    @Override
                    protected Collection<String> createCollection() {
                        return new HashSet<String>();
                    }
                };

                Project project = file.getProject();
                final SmartPointerManager pointerManager = SmartPointerManager.getInstance(project);
                final List<SmartPsiElementPointer> oldImportsPointers =
                    new ArrayList<SmartPsiElementPointer>(unusedImportsResults.allImports.size());
                for (JSImportStatement anImport : unusedImportsResults.allImports) {
                    oldImportsPointers.add(pointerManager.createSmartPsiElementPointer(anImport));
                    if (unusedImportsResults.unusedImports.contains(anImport)) {
                        continue;
                    }

                    JSElement importHolder =
                        ImportUtils.getImportHolder(anImport, JSFunction.class, JSPackageStatement.class, JSFile.class);
                    assert importHolder != null : "Import holder not found for " + anImport.getText();
                    importsByHolder.putValue(importHolder, anImport.getImportText());
                }

                final List<SmartPsiElementPointer> replaceWithShortName = new ArrayList<SmartPsiElementPointer>();
                for (Map.Entry<JSReferenceExpression, String> e : unusedImportsResults.fqnsToReplaceWithImport.entrySet()) {
                    final Collection<String> importsInScope;
                    final Collection<String> importsInEnclosingScope;
                    final JSElement importHolder;

                    JSElement enclosingFunction = ImportUtils.getImportHolder(e.getKey(), JSFunction.class);
                    JSElement enclosingPackage = ImportUtils.getImportHolder(e.getKey(), JSPackageStatement.class);

                    if (enclosingFunction != null && !importsByHolder.get(enclosingFunction).isEmpty()) {
                        importHolder = enclosingFunction;
                        importsInScope = importsByHolder.get(enclosingFunction);
                        importsInEnclosingScope = importsByHolder.get(enclosingPackage);
                    }
                    else if (enclosingPackage != null) {
                        importHolder = enclosingPackage;
                        importsInScope = importsByHolder.get(enclosingPackage);
                        importsInEnclosingScope = Collections.emptyList();
                    }
                    else {
                        importHolder = ImportUtils.getImportHolder(e.getKey(), JSFile.class);
                        importsInScope = importsByHolder.get(importHolder);
                        importsInEnclosingScope = Collections.emptyList();
                    }

                    String fqn = e.getValue();

                    ResolveResult resolve = resolveUsingImports(importsInScope, fqn, file);
                    ResolveResult implicitResolve = resolveUsingImports(importsInEnclosingScope, fqn, file);

                    if (resolve == ResolveResult.None && implicitResolve == ResolveResult.None) {
                        importsByHolder.putValue(importHolder, fqn);
                        replaceWithShortName.add(pointerManager.createSmartPsiElementPointer(e.getKey()));
                    }
                    else if ((resolve == ResolveResult.ThisOne || resolve == ResolveResult.None)
                        && (implicitResolve == ResolveResult.ThisOne || implicitResolve == ResolveResult.None)) {
                        replaceWithShortName.add(pointerManager.createSmartPsiElementPointer(e.getKey()));
                    }
                }

                final Collection<Pair<SmartPsiElementPointer<JSElement>, Collection<String>>> importsByHolderPointer =
                    new ArrayList<Pair<SmartPsiElementPointer<JSElement>, Collection<String>>>(importsByHolder.size());
                for (JSElement holder : importsByHolder.keySet()) {
                    importsByHolderPointer.add(Pair.create(
                        pointerManager.createSmartPsiElementPointer(holder),
                        importsByHolder.get(holder)
                    ));
                }

                for (Pair<SmartPsiElementPointer<JSElement>, Collection<String>> entry : importsByHolderPointer) {
                    JSElement holder = entry.getFirst().getElement();
                    assert holder != null && holder.isValid();

                    Pair<PsiElement, Boolean> defaultInsertionPlace = ImportUtils.getImportInsertionPlace(holder);
                    boolean before = defaultInsertionPlace.second;
                    PsiElement insertionPlace = defaultInsertionPlace.first;

                    PsiElement earlyImport = ImportUtils.findEarlyImport(before ? insertionPlace : insertionPlace.getNextSibling());
                    Pair<PsiElement, PsiElement> elementToDelete = null;
                    if (earlyImport != null) {
                        for (PsiElement e = (before ? insertionPlace : insertionPlace.getNextSibling()); e != earlyImport;
                             e = e.getNextSibling()) {
                            if (!(e instanceof PsiWhiteSpace)) {
                                break;
                            }
                        }
                        insertionPlace = earlyImport;
                        before = true;
                        PsiElement deleteTo = earlyImport;
                        while (deleteTo.getNextSibling() instanceof PsiWhiteSpace
                            || deleteTo.getNextSibling() instanceof JSImportStatement) {
                            deleteTo = deleteTo.getNextSibling();
                        }
                        elementToDelete = Pair.create(insertionPlace, deleteTo);
                    }
                    else if (before && insertionPlace instanceof PsiWhiteSpace) {
                        insertionPlace =
                            insertionPlace.replace(JSChangeUtil.createJSTreeFromText(insertionPlace.getProject(), " ").getPsi());
                    }
                    else if (insertionPlace.getNextSibling() instanceof PsiWhiteSpace) {
                        insertionPlace.getNextSibling()
                            .replace(JSChangeUtil.createJSTreeFromText(insertionPlace.getProject(), " ").getPsi());
                    }

                    String importBlock = ImportUtils.createImportBlock(project, entry.getSecond());
                    PsiElement newImports =
                        PsiFileFactory.getInstance(project).createFileFromText("dummy.js", JavaScriptLanguage.INSTANCE, importBlock);

                    PsiElement firstAdded;
                    if (before) {
                        firstAdded = insertionPlace.getParent()
                            .addRangeBefore(newImports.getFirstChild(), newImports.getLastChild(), insertionPlace);
                    }
                    else {
                        firstAdded =
                            insertionPlace.getParent().addRangeAfter(newImports.getFirstChild(), newImports.getLastChild(), insertionPlace);
                    }

                    if (elementToDelete != null) {
                        deleteRange(elementToDelete.first, elementToDelete.second);
                    }

                    PsiElement lastAdded = firstAdded;
                    final String lastImportText = newImports.getLastChild().getText();
                    while (!lastImportText.equals(lastAdded.getText())) {
                        lastAdded = lastAdded.getNextSibling();
                    }

                    PsiElement formatFrom = firstAdded.getPrevSibling() instanceof PsiWhiteSpace ? firstAdded.getPrevSibling() : firstAdded;
                    PsiElement formatTo = lastAdded.getNextSibling() instanceof PsiWhiteSpace ? lastAdded.getNextSibling() : lastAdded;

                    PsiFile realFile = file.getContext() != null ? file.getContext().getContainingFile() : file;

                    TextRange textRange =
                        InjectedLanguageManager.getInstance(file.getProject()).injectedToHost(firstAdded, firstAdded.getTextRange());
                    CodeStyleManager.getInstance(project).reformatText(realFile, textRange.getStartOffset(), textRange.getEndOffset());
                }

                for (SmartPsiElementPointer pointer : oldImportsPointers) {
                    final JSImportStatement statement = (JSImportStatement)pointer.getElement();
                    if (statement != null) {
                        deleteImport(statement);
                    }
                }

                for (SmartPsiElementPointer pointer : replaceWithShortName) {
                    final JSReferenceExpression fqn = (JSReferenceExpression)pointer.getElement();
                    if (fqn == null || !fqn.isValid()) {
                        continue;
                    }
                    String name = fqn.getReferencedName().substring(fqn.getReferencedName().lastIndexOf('.') + 1);
                    fqn.replace(JSChangeUtil.createExpressionFromText(project, name));
                }
            }
        };
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }

    private enum ResolveResult {
        None,
        ThisOne,
        OtherOne
    }

    private static ResolveResult resolveUsingImports(Collection<String> imports, String fqnToCheck, PsiElement context) {
        String shortName = fqnToCheck.substring(fqnToCheck.lastIndexOf('.') + 1);
        String firstOneResolved = null;
        for (String anImport : imports) {
            String name = anImport.substring(anImport.lastIndexOf('.') + 1);
            String resolvedFqn = null;
            if (name.equals(shortName)) {
                resolvedFqn = anImport;
            }
            else if ("*".equals(name)) {
                String qualifier = anImport.substring(0, anImport.lastIndexOf('.'));
                if (JSResolveUtil.findClassByQName(qualifier + "." + shortName, context) != null) {
                    resolvedFqn = qualifier + "." + shortName;
                }
            }
            if (resolvedFqn != null) {
                if (firstOneResolved == null) {
                    firstOneResolved = resolvedFqn;
                }
                else {
                    assert !firstOneResolved.equals(resolvedFqn);
                    return ResolveResult.OtherOne;
                }
            }
        }

        if (firstOneResolved == null) {
            return ResolveResult.None;
        }
        else {
            return fqnToCheck.equals(firstOneResolved) ? ResolveResult.ThisOne : ResolveResult.OtherOne;
        }
    }

    private static void deleteImport(final JSImportStatement anImport) {
        if (!anImport.isValid()) {
            return;
        }
        PsiElement nextSibling = anImport.getNextSibling();
        if (nextSibling instanceof PsiWhiteSpace) {
            // remove with the following whitespace
            String whitespace = nextSibling.getText();
            if (whitespace.contains("]]>")) {
                nextSibling.replace(JSChangeUtil.createJSTreeFromText(anImport.getProject(), "]]>").getPsi());
            }
            // don't remove trailing line break if it is an injection suffix
            else if (nextSibling.getNextSibling() == null
                || nextSibling.getNextSibling().getNode().getElementType() != JSTokenTypes.RBRACE
                || !(nextSibling.getParent() instanceof JSBlockStatement)
                || !ImportUtils.isAnonymousEventHandler((JSBlockStatement)nextSibling.getParent())) {
                nextSibling.delete();
            }
        }
        anImport.delete();
    }

    private static void deleteRange(PsiElement first, PsiElement last) {
        PsiElement e = first;
        while (true) {
            PsiElement next = e.getNextSibling();
            e.delete();
            if (e == last) {
                break;
            }
            assert next != null;
            e = next;
        }
    }
}
