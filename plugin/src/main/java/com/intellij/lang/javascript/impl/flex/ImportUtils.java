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
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.document.Document;
import consulo.document.util.TextRange;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.editor.FileModificationService;
import consulo.language.inject.InjectedLanguageManager;
import consulo.language.psi.PsiDocumentManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.logging.Logger;
import consulo.project.Project;
import consulo.util.collection.ArrayUtil;
import consulo.util.lang.Comparing;
import consulo.util.lang.Pair;
import consulo.xml.psi.xml.XmlAttributeValue;
import consulo.xml.psi.xml.XmlTag;
import consulo.xml.psi.xml.XmlText;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.*;

/**
 * @author Maxim.Mossienko
 * @since 2008-03-03
 */
public class ImportUtils {
    private static final String CDATA = "<![CDATA[";
    private static final String SCRIPT_TAG_NAME = "Script";

    private static final List<Class<? extends JSElement>> ANONYMOUS_EVENT_LISTENER_CLASSES = Arrays.asList(JSBlockStatement.class,
        JSFunctionExpression.class, JSParenthesizedExpression.class, JSCallExpression.class, JSExpressionStatement.class, JSFile.class
    );

    @Nullable
    @RequiredReadAction
    private static PsiElement findLBrace(JSElement holder) {
        for (PsiElement child = holder.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child.getNode().getElementType() == JSTokenTypes.LBRACE) {
                return child;
            }
        }
        return null;
    }

    @RequiredReadAction
    private static PsiElement specifyInsertionPlace(PsiElement insertBefore, String fqn) {
        JSImportStatement earlyImport = findEarlyImport(insertBefore);
        if (earlyImport == null) {
            return insertBefore;
        }

        while (compareImports(fqn, earlyImport.getImportText()) > 0) {
            if (earlyImport.getNextSibling() instanceof JSImportStatement importStatement) {
                earlyImport = importStatement;
            }
            else if (earlyImport.getNextSibling() instanceof PsiWhiteSpace whiteSpace
                && whiteSpace.getNextSibling() instanceof JSImportStatement importStatement) {
                earlyImport = importStatement;
            }
            else {
                return earlyImport.getNextSibling();
            }
        }
        return earlyImport;
    }

    @RequiredWriteAction
    public static void doImport(@Nonnull PsiElement subject, @Nonnull String fqn) {
        assert fqn.contains(".") : "Qualified name belongs to default package: " + fqn;

        if (!FileModificationService.getInstance().prepareFileForWrite(subject.getContainingFile())) {
            return;
        }
        Project project = subject.getProject();

        JSReferenceExpression refExpr = PsiTreeUtil.getNonStrictParentOfType(subject, JSReferenceExpression.class);
        if (refExpr != null && JSResolveUtil.referenceExpressionShouldBeQualified(refExpr)) {
            refExpr.replace(JSChangeUtil.createExpressionFromText(project, fqn)); // TODO should commit corresponding document before?
            return;
        }

        JSElement importHolder = getImportHolder(subject, JSPackageStatement.class, JSFile.class);
        if (importHolder == null) { // importHolder is null when completing js2 class name from js code
            return;
        }

        PsiFile file = importHolder.getContainingFile();
        Document document = PsiDocumentManager.getInstance(project).getDocument(file);
        PsiDocumentManager.getInstance(project).doPostponedOperationsAndUnblockDocument(document);

        String textToInsert = createImportBlock(project, Collections.singletonList(fqn));

        Pair<PsiElement, Boolean/*before*/> insertionPlace = getImportInsertionPlace(importHolder);
        if (!insertionPlace.second && insertionPlace.first.getNextSibling() != null) {
            insertionPlace = Pair.create(insertionPlace.first.getNextSibling(), true);
        }

        int offset;
        String prefix;
        String suffix;
        if (insertionPlace.second) {
            PsiElement insertBefore = specifyInsertionPlace(insertionPlace.first, fqn);
            offset = insertBefore.getTextRange().getStartOffset();
            prefix = (insertBefore.getPrevSibling() == null && file.getContext() == null)
                || insertBefore.getPrevSibling() instanceof PsiWhiteSpace ? "" : "\n";
            suffix = insertBefore instanceof PsiWhiteSpace ? "" : " ";
        }
        else {
            offset = insertionPlace.first.getTextRange().getEndOffset();
            prefix = insertionPlace.first instanceof PsiWhiteSpace ? "" : "\n";
            suffix = "";
        }

        document.insertString(offset, prefix + textToInsert + suffix);
        PsiDocumentManager.getInstance(project).commitDocument(document);

        PsiElement inserted = file.findElementAt(offset);
        if (prefix.length() > 0) {
            if (inserted.getNextSibling() instanceof JSImportStatement importStatement) {
                inserted = importStatement;
            }
        }
        else {
            JSImportStatement importStatement = PsiTreeUtil.getParentOfType(inserted, JSImportStatement.class);
            if (importStatement != null) {
                inserted = importStatement;
            }
        }
        PsiElement formatFrom = inserted.getPrevSibling() instanceof PsiWhiteSpace prevWhiteSpace ? prevWhiteSpace : inserted;
        PsiElement formatTo = inserted.getNextSibling() instanceof PsiWhiteSpace nextWhiteSpace ? nextWhiteSpace : inserted;

        PsiFile realFile = file.getContext() != null ? file.getContext().getContainingFile() : file;
        TextRange injectionOffset = InjectedLanguageManager.getInstance(project).injectedToHost(inserted, inserted.getTextRange());

        CodeStyleManager.getInstance(project).reformatText(
            realFile,
            injectionOffset.getStartOffset() + formatFrom.getTextRange().getStartOffset(),
            injectionOffset.getEndOffset() + formatTo.getTextRange().getEndOffset()
        );
    }

    @RequiredReadAction
    private static JSElement getAnonymousEventHandlerBody(JSFile injectedFile) {
        if (injectedFile.getFirstChild() instanceof JSExpressionStatement expression
            && expression.getExpression() instanceof JSCallExpression call
            && call.getMethodExpression() instanceof JSParenthesizedExpression parenthesized
            && parenthesized.getInnerExpression() instanceof JSFunctionExpression functionExpr) {

            JSFunction function = functionExpr.getFunction();
            if (function.getBody().length > 0) {
                return function.getBody()[0];
            }
        }
        assert false : "Couldn't find anonymous event handler body: " + injectedFile;
        return null;
    }

    private static boolean isAnonymousEventHandlerTag(JSFile jsFile) {
        PsiElement context = jsFile.getContext();
        return context instanceof XmlText xmlText && !SCRIPT_TAG_NAME.equals(((XmlTag)xmlText.getParent()).getLocalName());
    }

    private static boolean isAnonymousEventHandlerAttribute(JSFile jsFile) {
        PsiElement context = jsFile.getContext();
        return context instanceof XmlAttributeValue;
    }

    public static boolean isAnonymousEventHandler(JSBlockStatement block) {
        JSFile file = (JSFile)iterateUp(block, ANONYMOUS_EVENT_LISTENER_CLASSES);
        return file != null && (isAnonymousEventHandlerTag(file) || isAnonymousEventHandlerAttribute(file));
    }

    @Nullable
    private static PsiElement iterateUp(PsiElement element, List<Class<? extends JSElement>> classes) {
        Iterator<Class<? extends JSElement>> i = classes.iterator();
        while (i.hasNext()) {
            if (element == null || !i.next().isInstance(element)) {
                return null;
            }
            if (i.hasNext()) {
                element = element.getParent();
            }
        }
        return element;
    }

    @Nullable
    @RequiredReadAction
    private static JSElement getImportHolderFromXmlBackedClass(XmlBackedJSClassImpl jsClass) {
        try {
            return jsClass.createOrGetFirstScriptTag();
        }
        catch (IncorrectOperationException ex) {
            Logger.getInstance(ImportUtils.class).error(ex);
        }
        return null;
    }


    private static int compareImports(String qname1, String qname2) {
        // TODO keep certain classes at the top
        return Comparing.compare(qname1, qname2);
    }

    @RequiredReadAction
    public static Pair<PsiElement, Boolean /*before*/> getImportInsertionPlace(JSElement holder) {
        PsiElement insertionPlace;
        boolean before;
        if (holder instanceof JSPackageStatement packageStatement) {
            insertionPlace = findLBrace(packageStatement);
            assert insertionPlace != null : "LBrace not found";
            before = false;
        }
        else if (holder instanceof JSFunction function) {
            JSBlockStatement block = PsiTreeUtil.getChildOfType(function, JSBlockStatement.class);
            assert block != null : "Function block not found";
            insertionPlace = findLBrace(block);
            before = false;
        }
        else if (holder instanceof JSFile jsFile && isAnonymousEventHandlerTag(jsFile)) {
            holder = getAnonymousEventHandlerBody(jsFile);
            insertionPlace = findLBrace(holder);
            if (hasCDATA(insertionPlace.getNextSibling())) {
                insertionPlace = insertionPlace.getNextSibling();
            }
            before = false;
        }
        else {
            JSPackageStatement aPackage = PsiTreeUtil.getChildOfType(holder, JSPackageStatement.class);
            if (aPackage != null) {
                insertionPlace = aPackage;
                before = false;
            }
            else {
                insertionPlace = holder.getFirstChild();
                before = !hasCDATA(insertionPlace);
            }
        }
        return Pair.create(insertionPlace, before);
    }

    @RequiredReadAction
    private static boolean hasCDATA(@Nullable PsiElement element) {
        return element instanceof PsiWhiteSpace whiteSpace && whiteSpace.getText().contains(CDATA);
    }

    @Nullable
    @RequiredReadAction
    @SafeVarargs
    public static JSElement getImportHolder(PsiElement origin, Class<? extends JSElement>... classes) {
        if (origin instanceof XmlBackedJSClassImpl xmlBackedJSClass) {
            return getImportHolderFromXmlBackedClass(xmlBackedJSClass);
        }

        JSElement importHolder = PsiTreeUtil.getParentOfType(origin, classes);
        if (importHolder instanceof JSFunctionExpression functionExpr
            && functionExpr.getContainingFile() instanceof JSFile jsFile
            && (isAnonymousEventHandlerTag(jsFile) || isAnonymousEventHandlerAttribute(jsFile))) {
            importHolder = ArrayUtil.contains(JSFile.class, classes) ? jsFile : null;
        }

        if (importHolder instanceof JSFile jsFile && isAnonymousEventHandlerAttribute(jsFile)) {
            XmlBackedJSClassImpl jsClass = JSResolveUtil.getXmlBackedClass(jsFile);
            assert jsClass != null;
            importHolder = getImportHolderFromXmlBackedClass(jsClass);
        }
        return importHolder;
    }

    public static String createImportBlock(Project project, Collection<String> fqns) {
        List<String> sorted = new ArrayList<>(fqns);
        Collections.sort(sorted, ImportUtils::compareImports);

        final String semicolon = JSChangeUtil.getSemicolon(project);
        StringBuilder s = new StringBuilder();
        for (String fqn : sorted) {
            s.append("import ").append(fqn).append(semicolon);
        }
        return s.toString();
    }

    @Nullable
    @RequiredReadAction
    public static JSImportStatement findEarlyImport(@Nullable PsiElement startFrom) {
        for (PsiElement element = startFrom; element != null; element = element.getNextSibling()) {
            if (element instanceof JSImportStatement importStatement) {
                return importStatement;
            }
            if (element instanceof JSClass || element instanceof JSStatement || element instanceof JSFunction) {
                break;
            }
        }
        return null;
    }
}
