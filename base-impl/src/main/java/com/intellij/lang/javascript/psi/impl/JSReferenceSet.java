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

package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.document.util.TextRange;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.ASTNode;
import consulo.language.psi.*;
import consulo.language.psi.resolve.ResolveCache;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import consulo.util.collection.ArrayUtil;
import consulo.util.lang.StringUtil;
import consulo.xml.psi.xml.*;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Maxim.Mossienko
 */
public class JSReferenceSet {
    private static final String GLOBAL_PREFIX = "global#";

    private String myReferenceText;
    private PsiReference[] myReferences;
    private final PsiElement element;
    private final boolean isSoft;
    private final boolean onlyPackages;
    private final boolean onlyFqns;
    private boolean myOnlyDefaultPackage;

    public JSReferenceSet(PsiElement element, String text, int offset, boolean soft) {
        this(element, text, offset, soft, false, false);
    }

    public JSReferenceSet(PsiElement element, String text, int offset, boolean soft, boolean _onlyPackages, boolean _onlyFqns) {
        this.element = element;
        isSoft = soft;
        myReferenceText = text;
        myReferences = reparse(text, offset);
        onlyPackages = _onlyPackages;
        onlyFqns = _onlyFqns;
    }

    public JSReferenceSet(PsiElement element, boolean soft) {
        this.element = element;
        isSoft = soft;
        onlyPackages = false;
        onlyFqns = false;
    }

    public JSReferenceSet(PsiElement element) {
        this(element, true);
    }

    public PsiReference[] getReferences() {
        return myReferences;
    }

    public void update(String text, int offset) {
        if (myReferences != null &&
            myReferenceText != null &&
            myReferenceText.equals(text)) {
            return;
        }

        if (!StringUtil.startsWithChar(text, '"') && !StringUtil.startsWithChar(text, '\'')) {
            myReferenceText = text;
            myReferences = PsiReference.EMPTY_ARRAY;
        }
        else {
            PsiReference[] list = reparse(StringUtil.stripQuotesAroundValue(text), offset + 1);
            myReferenceText = text;
            myReferences = list;
        }
    }

    private PsiReference[] reparse(String value, int offset) {
        if (value.startsWith(GLOBAL_PREFIX)) {
            value = value.substring(GLOBAL_PREFIX.length());
            offset += GLOBAL_PREFIX.length();
            myOnlyDefaultPackage = true;
        }
        List<PsiReference> refs = new ArrayList<>(1);
        int lastPos = 0;
        int dotPos = findSeparatorPosition(value, lastPos);

        while (dotPos != -1) {
            String s = value.substring(lastPos, dotPos).trim();

            if (s.length() > 0) {
                refs.add(new MyPsiReference(s, offset + lastPos, false));
            }

            lastPos = dotPos + 1;
            dotPos = findSeparatorPosition(value, lastPos);
        }

        int end = value.length();

        int lpar = value.indexOf('(', lastPos);
        if (lpar != -1) {
            end = lpar;
        }

        String s = value.substring(lastPos, end).trim();

        if (s.length() > 0) {
            refs.add(new MyPsiReference(s, offset + lastPos, lastPos > 0 && value.charAt(lastPos - 1) == '#'));
        }

        return refs.toArray(new PsiReference[refs.size()]);
    }

    private static int findSeparatorPosition(String s, int fromIndex) {
        int pos = s.indexOf('.', fromIndex);
        // no more than one ':' and '#' symbol after last '.'
        if (pos == -1 && s.indexOf(":") >= fromIndex) {
            pos = s.indexOf(":", fromIndex);
        }
        if (pos == -1 && s.indexOf("#") >= fromIndex) {
            pos = s.indexOf("#", fromIndex);
        }
        return pos;
    }

    public boolean isSoft() {
        return isSoft;
    }

    private class MyPsiReference implements PsiPolyVariantReference, EmptyResolveMessageProvider {
        private String myText;
        private int myOffset;
        private boolean myMethodRef;

        MyPsiReference(String s, int i, boolean methodRef) {
            myText = s;
            myOffset = i;
            myMethodRef = methodRef;
        }

        @Override
        @RequiredReadAction
        public PsiElement getElement() {
            return element;
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public TextRange getRangeInElement() {
            return new TextRange(myOffset, myOffset + myText.length());
        }

        @Nullable
        @Override
        @RequiredReadAction
        public PsiElement resolve() {
            ResolveResult[] resolveResults = multiResolve(false);
            return resolveResults.length == 1 ? resolveResults[0].getElement() : null;
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public String getCanonicalText() {
            return myText;
        }

        @Override
        @RequiredWriteAction
        public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
            int i = newElementName.lastIndexOf('.');
            if (i != -1) {
                newElementName = newElementName.substring(0, i);
            }
            return handleContentChange(getElement(), getRangeInElement(), newElementName);
        }

        @Override
        @RequiredWriteAction
        public PsiElement bindToElement(@Nonnull PsiElement element) throws IncorrectOperationException {
            String qName = JSPsiImplUtils.getQNameForMove(getElement(), element);
            if (qName != null) {
                handleContentChange(
                    getElement(),
                    new TextRange(myReferences[0].getRangeInElement().getStartOffset(), getRangeInElement().getEndOffset()),
                    qName
                );
            }
            return null;
        }

        @Override
        @RequiredReadAction
        public boolean isReferenceTo(PsiElement element) {
            return (element instanceof PsiNamedElement || element instanceof XmlAttributeValue)
                && JSResolveUtil.isReferenceTo(this, myText, element);
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public Object[] getVariants() {
            PsiFile containingFile = element.getContainingFile();
            ResolveProcessor processor = null;

            if (isNewResolveAndCompletion(containingFile)) {
                processor = doProcess(element.getContainingFile(), null);
                if (!(element instanceof JSLiteralExpression)) {
                    return processor.getResultsAsObjects();
                }
            }
            return getOldVariants(containingFile, processor);
        }

        @RequiredReadAction
        private Object[] getOldVariants(PsiFile containingFile, ResolveProcessor localProcessor) {
            List<String> contextIds = fillContextIds();
            VariantsProcessor processor =
                new VariantsProcessor(contextIds != null ? ArrayUtil.toStringArray(contextIds) : null, containingFile, false, element);

            processor.setAddOnlyCompleteMatches(contextIds != null || !(element instanceof JSLiteralExpression));
            if (localProcessor != null) {
                processor.addLocalResults(localProcessor.getResults());  // TODO: remove this stuff as we create new js index
            }
            //JSResolveUtil.processGlobalSymbols(containingFile, processor);

            PsiElement context = containingFile.getContext();
            if (context != null) {
                JSResolveUtil.treeWalkUp(processor, containingFile, containingFile, element);
            }

            return processor.getResult();
        }

        @Nullable
        @RequiredReadAction
        private List<String> fillContextIds() {
            List<String> contextIds = null;
            PsiReference prevContextReference = null;
            for (PsiReference ref : myReferences) {
                if (ref == this) {
                    break;
                }
                if (contextIds == null) {
                    contextIds = new ArrayList<>(3);
                }
                contextIds.add(ref.getCanonicalText());
                prevContextReference = ref;
            }

            if (contextIds == null && myOffset > 0) {
                PsiElement elt = findNearestClass();

                if (elt instanceof JSClass jsClass && !(getElement() instanceof JSLiteralExpression)) {
                    String qName = jsClass.getQualifiedName();
                    BaseJSSymbolProcessor.addIndexListFromQName(qName, elt, contextIds = new ArrayList<>());
                }
            }
            else if (contextIds != null) {
                PsiElement psiElement = JSResolveUtil.unwrapProxy(prevContextReference.resolve());

                if (psiElement instanceof XmlToken xmlToken) {
                    BaseJSSymbolProcessor.TagContextBuilder builder =
                        new BaseJSSymbolProcessor.TagContextBuilder(xmlToken, BaseJSSymbolProcessor.HTML_ELEMENT_TYPE_NAME);
                    psiElement = builder.element;
                }
                if (psiElement instanceof JSClass jsClass) {
                    String qName = jsClass.getQualifiedName();
                    BaseJSSymbolProcessor.addIndexListFromQName(qName, jsClass, contextIds = new ArrayList<>());
                }
            }

            return contextIds;
        }

        @Override
        @RequiredReadAction
        public boolean isSoft() {
            return JSReferenceSet.this.isSoft();
        }

        @Override
        @Nonnull
        @RequiredReadAction
        public ResolveResult[] multiResolve(boolean incompleteCode) {
            PsiFile containingFile = element.getContainingFile();
            if (containingFile == null) {
                return ResolveResult.EMPTY_ARRAY;
            }
            return ResolveCache.getInstance(containingFile.getProject())
                .resolveWithCaching(this, MyResolver.INSTANCE, true, incompleteCode, containingFile);
        }

        @RequiredReadAction
        private ResolveResult[] doResolve(PsiFile psiFile) {
            if ("int".equals(myText) ||
                "uint".equals(myText) ||
                (onlyPackages && "*".equals(myText))) {
                return new ResolveResult[]{new JSResolveUtil.MyResolveResult(element)};
            }

            if (isNewResolveAndCompletion(psiFile)) {
                ResolveProcessor processor = doProcess(psiFile, myText);

                return processor.getResultsAsResolveResults();
            }

            return doOldResolve(psiFile);
        }

        private boolean isNewResolveAndCompletion(PsiFile psiFile) {
            return JSResolveUtil.isNewResolveAndCompletion(psiFile) || onlyFqns;
        }

        @RequiredReadAction
        private ResolveProcessor doProcess(PsiFile psiFile, String text) {
            ResolveProcessor processor = new ResolveProcessor(text) {
                @Override
                @RequiredReadAction
                public boolean execute(PsiElement element, ResolveState state) {
                    if (onlyPackages) {
                        return false;
                    }
                    if (onlyFqns) {
                        return true;
                    }
                    if (myOnlyDefaultPackage && element instanceof JSQualifiedNamedElement qualifiedNamedElement) {
                        String qName = qualifiedNamedElement.getQualifiedName();
                        if (qName != null && !StringUtil.isEmpty(StringUtil.getPackageName(qName))) {
                            return true;
                        }
                    }
                    return super.execute(element, state);
                }
            };

            int i;
            for (i = 0; i < myReferences.length && myReferences[i] != this; ++i) {
                ;
            }

            if (i == 0) {
                PsiElement elt = findNearestClass();

                if (myOffset > 0) {
                    if (elt instanceof JSClass jsClass && !(element instanceof JSLiteralExpression)) {
                        processor.setToProcessHierarchy(true);
                        processor.setTypeContext(true);
                        if (!jsClass.processDeclarations(processor, ResolveState.initial(), jsClass, jsClass)) {
                            return processor;
                        }
                    }
                }
                else if (elt instanceof JSClass jsClass) {
                    processor.setTypeContext(true);
                    processor.setToProcessMembers(false);
                    if (!jsClass.processDeclarations(processor, ResolveState.initial(), jsClass, jsClass)) {
                        return processor;
                    }
                }
                JSResolveUtil.treeWalkUp(processor, element, element.getParent(), element);
            }
            else {
                PsiElement psiElement = JSResolveUtil.unwrapProxy(myReferences[i - 1].resolve());

                if (psiElement instanceof XmlToken xmlToken) {
                    BaseJSSymbolProcessor.TagContextBuilder builder =
                        new BaseJSSymbolProcessor.TagContextBuilder(xmlToken, BaseJSSymbolProcessor.HTML_ELEMENT_TYPE_NAME);
                    psiElement = builder.element;
                }
                if (psiElement != null) {
                    if (psiElement instanceof JSClass) {
                        processor.setToProcessHierarchy(true);
                    }
                    psiElement.processDeclarations(processor, ResolveState.initial(), psiElement, psiElement);
                }
            }

            if (psiFile instanceof XmlFile xmlFile && !JavaScriptSupportLoader.isFlexMxmFile(xmlFile)) {
                // TODO: short names during completion should be
                JSResolveUtil.processTopLevelClasses(
                    processor,
                    ResolveState.initial(),
                    xmlFile.getProject(),
                    xmlFile.getResolveScope(),
                    onlyFqns,
                    false
                );
            }
            return processor;
        }

        @RequiredReadAction
        private PsiElement findNearestClass() {
            PsiElement elt = element;
            PsiElement parent;
            while (!((parent = elt.getParent()) instanceof JSFile) && !(parent instanceof JSPackageStatement)) {
                if (parent instanceof XmlTagChild) {
                    break;
                }
                elt = parent;
                if (elt == null || elt instanceof JSClass) {
                    break;
                }
            }

            if (parent instanceof XmlTag tag && XmlBackedJSClassImpl.isInlineComponentTag(tag)) {
                elt = XmlBackedJSClassImpl.getXmlBackedClass(tag);
            }

            if (elt != null && !(elt instanceof JSClass)) {
                elt = elt.getNextSibling();
                if (elt instanceof PsiWhiteSpace whiteSpace) {
                    elt = whiteSpace.getNextSibling();
                }
            }
            return elt;
        }

        @RequiredReadAction
        private ResolveResult[] doOldResolve(PsiFile psiFile) {
            if ("*".equals(myText)) {
                return new ResolveResult[]{new JSResolveUtil.MyResolveResult(element)};
            }
            List<String> contextIds = fillContextIds();

            String text = myText;

            if (getElement() instanceof JSDocTagValue
                && myReferences.length == 1 && myReferences[myReferences.length - 1] == this && !myMethodRef) {
                text = StringUtil.capitalize(text);
            }

            WalkUpResolveProcessor processor = new WalkUpResolveProcessor(
                text,
                contextIds != null ? ArrayUtil.toStringArray(contextIds) : null,
                psiFile,
                false,
                element
            );

            processor.setAddOnlyCompleteMatches(contextIds != null || !(element instanceof JSLiteralExpression));
            //JSResolveUtil.processGlobalSymbols(psiFile, processor);
            StringBuilder b = new StringBuilder();

            for (PsiReference ref : myReferences) {
                if (b.length() > 0) {
                    b.append('.');
                }
                b.append(ref.getCanonicalText());
                if (ref == this) {
                    break;
                }
            }
            String str = b.toString();

            PsiElement context = psiFile.getContext();
            if (context != null && str.indexOf('.') == -1) {
                JSResolveUtil.treeWalkUp(processor, psiFile, psiFile, element);
            }

            return processor.getResults();
        }

        @Nonnull
        @Override
        public LocalizeValue buildUnresolvedMessage(@Nonnull String text) {
            text = "'" + text.replace("'", "''") + "'";
            return JavaScriptLocalize.javascriptUnresolvedVariableOrTypeNameMessage2(text);
        }
    }

    static class MyResolver implements ResolveCache.PolyVariantContextResolver<MyPsiReference> {
        private static final MyResolver INSTANCE = new MyResolver();

        @Nonnull
        @Override
        @RequiredReadAction
        public ResolveResult[] resolve(@Nonnull MyPsiReference ref, @Nonnull PsiFile containingFile, boolean incompleteCode) {
            return ref.doResolve(containingFile);
        }
    }

    @RequiredReadAction
    private static PsiElement handleContentChange(PsiElement elt, TextRange range, String newElementName) {
        if (elt instanceof XmlTag || elt instanceof XmlAttributeValue) {
            int i = newElementName.indexOf('.');
            if (i != -1) {
                newElementName = newElementName.substring(0, i);
            }
            return ElementManipulators.getManipulator(elt).handleContentChange(elt, range, newElementName);
        }

        String myReferenceText = elt.getText();
        String newLiteralText =
            myReferenceText.substring(0, range.getStartOffset()) + newElementName + myReferenceText.substring(range.getEndOffset());
        ASTNode expressionFromText;

        Project project = elt.getProject();
        if (elt instanceof JSExpression) {
            expressionFromText = JSChangeUtil.createExpressionFromText(project, newLiteralText).getNode();
        }
        else if (elt instanceof JSAttributeNameValuePair) {
            PsiElement element = JSChangeUtil.createJSTreeFromText(project, "[XXX(" + newLiteralText + ")]").getPsi();
            expressionFromText = ((JSAttribute)element.getFirstChild()).getValues()[0].getNode();
        }
        else {
            assert elt instanceof JSDocTagValue;
            PsiElement tag = JSChangeUtil.createJSTreeFromText(project, "/** @see " + newLiteralText + " */").getPsi();
            expressionFromText = ((JSDocTag)tag.getFirstChild().getChildren()[0]).getValue().getNode();
        }

        if (expressionFromText.getPsi().getClass() == elt.getClass()) {
            ASTNode astNode = elt.getNode();
            astNode.replaceChild(astNode.getFirstChildNode(), expressionFromText.getFirstChildNode());
        }

        return null;
    }
}
