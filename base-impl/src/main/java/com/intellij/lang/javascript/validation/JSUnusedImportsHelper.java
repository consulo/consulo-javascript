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

package com.intellij.lang.javascript.validation;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.application.util.CachedValue;
import consulo.application.util.CachedValueProvider;
import consulo.application.util.CachedValuesManager;
import consulo.javascript.psi.JavaScriptImportStatementBase;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiModificationTracker;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.resolve.PsiElementProcessor;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.util.dataholder.Key;
import consulo.util.lang.Pair;
import consulo.util.lang.StringUtil;
import consulo.xml.psi.xml.XmlAttributeValue;
import consulo.xml.psi.xml.XmlFile;
import consulo.xml.psi.xml.XmlTag;
import consulo.xml.psi.xml.XmlText;
import jakarta.annotation.Nullable;

import java.util.*;

public class JSUnusedImportsHelper {
    private final Collection<PsiElement> myElements;

    public static class Results {
        public final Collection<JSImportStatement> unusedImports;
        public final Collection<JSImportStatement> allImports;
        public final Map<JSReferenceExpression, String> fqnsToReplaceWithImport;

        public Results(
            Map<JSReferenceExpression, String> fqnsToReplaceWithImport,
            Collection<JSImportStatement> unusedImports,
            Collection<JSImportStatement> allImports
        ) {
            this.fqnsToReplaceWithImport = fqnsToReplaceWithImport;
            this.unusedImports = unusedImports;
            this.allImports = allImports;
        }
    }

    private static final Collection<? extends Class<? extends JSQualifiedNamedElement>> REFERENCED_ELEMENTS_CLASSES =
        Arrays.asList(JSClass.class, JSNamespaceDeclaration.class, JSFunction.class, JSVariable.class);

    // in Flex 3 for some reason fqn reference to function or variable does not reqiure explicit import statement
    private static final Collection<? extends Class<? extends JSQualifiedNamedElement>> QUALIFIED_REFERENCE_NEEDS_IMPORT =
        Arrays.asList(JSClass.class, JSNamespaceDeclaration.class);

    private static final Key<CachedValue<Results>> ourUnusedImportsKey = Key.create("js.unused.imports");

    private final Set<JSImportStatement> unusedImports = new HashSet<>();
    private final Collection<JSImportStatement> allImports = new HashSet<>();
    private final Set<JSImportStatement> importsUsedAheadOfDefinition = new HashSet<>();
    private final Map<JSReferenceExpression, String> fqnsToReplaceWithImport = new HashMap<>();
    private final PsiFile myContainingFile;

    private JSUnusedImportsHelper(PsiFile containingFile, Collection<PsiElement> elements) {
        myContainingFile = containingFile;
        myElements = elements;
    }

    private void registerUnused(JSImportStatement importStatement) {
        allImports.add(importStatement);

        String importText = importStatement.getImportText();
        if (importText == null) {
            return;
        }

        if (importsUsedAheadOfDefinition.contains(importStatement)) {
            return;
        }

        unusedImports.add(importStatement);
    }

    private static boolean isInstance(PsiElement element, Collection<? extends Class<?>> classes) {
        for (Class<?> clazz : classes) {
            if (clazz.isInstance(element)) {
                return true;
            }
        }
        return false;
    }

    private void process(JSReferenceExpression node) {
        if (node.getQualifier() == null) {
            String thisPackage = JSResolveUtil.findPackageStatementQualifier(node);
            registerUsedImportsFromResolveResults(node, thisPackage);
        }
        else {
            registerUsedImportsFromResolveResults(node, null);

            Pair<Boolean, Boolean> replaceStatus = getReplaceStatus(node);

            if (replaceStatus.second) {
                if (sameContainingFile(node.getContainingFile(), myContainingFile)) {
                    fqnsToReplaceWithImport.put(node, node.getText());
                }
            }
        }
    }

    private void registerUsedImportsFromResolveResults(JSReferenceExpression node, String thisPackage) {
        for (ResolveResult r : node.multiResolve(false)) {
            // TODO can we get different import statements here?
            if (r instanceof JSResolveUtil.MyResolveResult resolveResult
                && resolveResult.getImportUsed() instanceof JSImportStatement jsImportStatement
                && isInstance(r.getElement(), REFERENCED_ELEMENTS_CLASSES)) {
                String importString = jsImportStatement.getImportText();
                String importedPackage = importString.substring(0, importString.lastIndexOf('.'));
                if (thisPackage == null || !thisPackage.equals(importedPackage)) {
                    registerUsed(jsImportStatement);
                }
            }
        }
    }

    private static boolean sameContainingFile(PsiFile containingFile, PsiFile subjectFile) {
        return subjectFile instanceof XmlFile ? getContainingFile(containingFile) == subjectFile : containingFile == subjectFile;
    }

    public static boolean isSomeNodeThatShouldNotHaveImportsWhenQualified(JSReferenceExpression expression, PsiElement element) {
        Pair<Boolean, Boolean> pair = getReplaceStatus(expression, element);
        return !pair.first;
    }

    private static Pair<Boolean, Boolean> getReplaceStatus(JSReferenceExpression node) {
        return getReplaceStatus(node, null);
    }

    /**
     * @return Pair(needs import statement, can be replaced)
     */
    private static Pair<Boolean, Boolean> getReplaceStatus(JSReferenceExpression node, PsiElement resolve) {
        if (resolve == null) {
            resolve = JSResolveUtil.findClassByQName(node.getText(), node);
        }
        PsiElement parent = node.getParent();
        if (parent instanceof JavaScriptImportStatementBase) {
            return Pair.create(false, false);
        }

        if (parent instanceof JSExpressionStatement && !(resolve instanceof JSNamespaceDeclaration)) {
            return Pair.create(false, false);
        }

        // implicit parameter of anonymous event listener
        if (parent instanceof JSParameter) {
            JSFunction parentFunctionExpression = PsiTreeUtil.getParentOfType(node, JSFunction.class);
            if (parentFunctionExpression instanceof JSFunctionExpression) {
                if (node.getContainingFile().getContext() instanceof XmlAttributeValue
                    || node.getContainingFile().getContext() instanceof XmlText) {
                    return Pair.create(false, false);
                }
            }
        }

        if (parent.getNode().getElementType() == JSElementTypes.EXTENDS_LIST && parent.getParent() instanceof JSClass jsClass) {
            String className = jsClass.getName();
            if (StringUtil.isNotEmpty(className) && className.equals(node.getReferencedName())) {
                return Pair.create(true, false);
            }
        }

        // part of implements list of mxml component
        if (parent.getNode().getElementType() == JSElementTypes.IMPLEMENTS_LIST
            && node.getContainingFile().getContext() instanceof XmlAttributeValue) {
            return Pair.create(false, false);
        }

        PsiElement element = resolve;
        if (element == null || !isInstance(element, REFERENCED_ELEMENTS_CLASSES)) {
            return Pair.create(false, false);
        }

        return Pair.create(isInstance(element, QUALIFIED_REFERENCE_NEEDS_IMPORT), true);
    }

    private void registerUsed(JSImportStatement importStatement) {
        allImports.add(importStatement);
        if (importStatement.getImportText() == null) {
            return;
        }

        if (!unusedImports.remove(importStatement)) {
            importsUsedAheadOfDefinition.add(importStatement);
        }
    }

    private Collection<JSImportStatement> filter(Collection<JSImportStatement> original) {
        Collection<JSImportStatement> result = new ArrayList<>();
        for (JSImportStatement importStatement : original) {
            if (importStatement.isValid() && sameContainingFile(importStatement.getContainingFile(), myContainingFile)) {
                result.add(importStatement);
            }
        }
        return result;
    }

    public static Results getUnusedImports(PsiFile file) {
        PsiFile containingFile = getContainingFile(file);

        CachedValue<Results> data = containingFile.getUserData(ourUnusedImportsKey);
        if (data == null) {
            PsiFile containingFile1 = containingFile;
            data = CachedValuesManager.getManager(file.getProject()).createCachedValue(
                () -> {
                    Map<XmlTag, Collection<PsiElement>> allElements = new HashMap<>();
                    Collection<JSFile> processedFiles = new HashSet<>();
                    collectElements(null, containingFile1, allElements, processedFiles);

                    Results allResults = new Results(new HashMap<>(), new ArrayList<>(), new ArrayList<>());
                    for (Collection<PsiElement> elements : allElements.values()) {
                        Results results = new JSUnusedImportsHelper(containingFile1, elements).getUnusedImports();
                        allResults.fqnsToReplaceWithImport.putAll(results.fqnsToReplaceWithImport);
                        allResults.unusedImports.addAll(results.unusedImports);
                        allResults.allImports.addAll(results.allImports);
                    }

                    // TODO explicit depencencies
                    return new CachedValueProvider.Result<>(allResults, PsiModificationTracker.MODIFICATION_COUNT);
                },
                false
            );
            containingFile1.putUserData(ourUnusedImportsKey, data);
        }
        return data.getValue();
    }

    private static PsiFile getContainingFile(PsiFile file) {
        return file.getContext() != null ? file.getContext().getContainingFile() : file;
    }

    private Results getUnusedImports() {
        for (PsiElement e : myElements) {
            if (e instanceof JSImportStatement) {
                JSImportStatement importStatement = (JSImportStatement)e;
                registerUnused(importStatement);
            }
            else if (e instanceof JSReferenceExpression) {
                process((JSReferenceExpression)e);
            }
        }

        return new Results(fqnsToReplaceWithImport, filter(unusedImports), filter(allImports));
    }

    private static void collectElements(
        @Nullable XmlTag rootTag,
        PsiFile file,
        Map<XmlTag, Collection<PsiElement>> result,
        Collection<JSFile> processedFiles
    ) {
        if (processedFiles.contains(file)) {
            return;
        }
        if (file instanceof JSFile jsFile) {
            processedFiles.add(jsFile);

            PsiTreeUtil.processElements(
                file,
                element -> {
                    if (element instanceof JSIncludeDirective includeDirective) {
                        PsiFile includedFile = includeDirective.resolveFile();
                        // we check processed files before since we may include this file to self and setting context will make cycle
                        if (includedFile instanceof JSFile includedJsFile && !processedFiles.contains(includedJsFile)) {
                            includedFile.putUserData(JSResolveUtil.contextKey, element);
                            collectElements(rootTag, includedFile, result, processedFiles);
                        }
                    }
                    else if (element instanceof JSElement && !(element instanceof JSFile)) {
                        Collection<PsiElement> elements = result.get(rootTag);
                        if (elements == null) {
                            elements = new ArrayList<>();
                            result.put(rootTag, elements);
                        }
                        elements.add(element);
                    }
                    return true;
                }
            );
        }
        else if (JavaScriptSupportLoader.isFlexMxmFile(file)) {
            XmlBackedJSClassImpl.visitInjectedFiles(
                (XmlFile)file,
                (rootTag1, jsFile) -> collectElements(rootTag1, jsFile, result, processedFiles)
            );
        }
    }
}
