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

import java.util.List;

import jakarta.annotation.Nonnull;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.JSSourceElement;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.StubBasedPsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.stub.StubElement;
import consulo.language.util.IncorrectOperationException;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.file.FileViewProvider;
import consulo.language.impl.psi.PsiFileBase;
import jakarta.annotation.Nullable;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 28, 2005
 * Time: 12:25:04 AM
 * To change this template use File | Settings | File Templates.
 */
public class JSFileImpl extends PsiFileBase implements JSFile {
    public JSFileImpl(FileViewProvider fileViewProvider) {
        super(fileViewProvider, JavaScriptLanguage.INSTANCE);
    }

    @Override
    public String toString() {
        return "JSFile:" + getName();
    }

    @Override
    public boolean processDeclarations(
        @Nonnull final PsiScopeProcessor processor,
        @Nonnull final ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        boolean result = JSResolveUtil.processDeclarationsInScope(this, processor, state, lastParent, place);
        if (lastParent == null) {
            return result;
        }

        if (result) {
            if (getContext() == null) {
                if (!(lastParent instanceof JSPackageStatement)) {
                    result = JSImportHandlingUtil.tryResolveImports(processor, this, place);
                }

                if (result &&
                    JSResolveUtil.isNewResolveAndCompletion(this) &&
                    (JSResolveUtil.shouldProcessImports(place, processor))) {
                    result = JSResolveUtil.processGlobalThings(processor, state, place, this);
                }
            }
        }

        return result;
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor elementVisitor) {
            elementVisitor.visitJSElement(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @Override
    public PsiElement addRangeBefore(
        @Nonnull PsiElement first,
        @Nonnull PsiElement last,
        PsiElement anchor
    ) throws IncorrectOperationException {
        if (JSChangeUtil.isStatementOrComment(first)) {
            return JSChangeUtil.doAddRangeBefore(this, first, last, anchor);
        }
        return super.addRangeBefore(first, last, anchor);
    }

    @Override
    public PsiElement addRange(PsiElement first, PsiElement last) throws IncorrectOperationException {
        return addRangeAfter(first, last, null);
    }

    @Override
    public PsiElement addAfter(@Nonnull PsiElement element, PsiElement anchor) throws IncorrectOperationException {
        if (JSChangeUtil.isStatementOrComment(element)) {
            return JSChangeUtil.doAddAfter(this, element, anchor);
        }
        return super.addAfter(element, anchor);
    }

    @Override
    public PsiElement addBefore(@Nonnull PsiElement element, PsiElement anchor) throws IncorrectOperationException {
        if (JSChangeUtil.isStatementOrComment(element)) {
            return JSChangeUtil.doAddBefore(this, element, anchor);
        }
        return super.addBefore(element, anchor);
    }

    @Override
    public PsiElement addRangeAfter(PsiElement first, PsiElement last, PsiElement anchor) throws IncorrectOperationException {
        if (JSChangeUtil.isStatementOrComment(first)) {
            return JSChangeUtil.doAddRangeAfter(this, first, last, anchor);
        }

        return super.addRangeAfter(first, last, anchor);
    }

    @Override
    public PsiElement add(@Nonnull PsiElement element) throws IncorrectOperationException {
        return addAfter(element, null);
    }

    @Override
    public StubBasedPsiElement findStubbedElementAtOffset(final int offset, final Class<? extends StubBasedPsiElement> clazz) {
        final StubElement stub = getStub();

        if (stub != null) {
            final List<StubElement> children = stub.getChildrenStubs();

            for (StubElement child : children) {
                final PsiElement psi = child.getPsi();

                if (psi.getTextRange().getStartOffset() == offset && clazz.isInstance(psi)) {
                    return (StubBasedPsiElement)psi;
                }
            }
        }
        return null;
    }

    @Override
    public JSSourceElement[] getStatements() {
        final StubElement stub = getStub();
        if (stub != null) {
            return (JSSourceElement[])stub.getChildrenByType(JSElementTypes.SOURCE_ELEMENTS, JSSourceElement.EMPTY_ARRAY);
        }
        return findChildrenByClass(JSSourceElement.class);
    }
}

