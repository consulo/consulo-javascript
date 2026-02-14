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

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.stubs.JSReferenceListStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.StubIndex;
import consulo.project.Project;
import consulo.util.collection.ArrayUtil;
import jakarta.annotation.Nonnull;

import java.util.ArrayList;
import java.util.Collection;

/**
 * @author Maxim.Mossienko
 */
public class JSReferenceListImpl extends JSStubElementImpl<JSReferenceListStub> implements JSReferenceList {
    public JSReferenceListImpl(ASTNode node) {
        super(node);
    }

    public JSReferenceListImpl(JSReferenceListStub stub, IStubElementType stubElementType) {
        super(stub, stubElementType);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSReferenceList(this);
    }

    @RequiredReadAction
    @Nonnull
    @Override
    public JSReferenceExpression[] getExpressions() {
        return findChildrenByClass(JSReferenceExpression.class);
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public String[] getReferenceTexts() {
        JSReferenceListStub stub = getStub();
        if (stub != null) {
            return stub.getReferenceTexts();
        }

        JSReferenceExpression[] referenceExpressions = getExpressions();
        if (referenceExpressions.length == 0) {
            return ArrayUtil.EMPTY_STRING_ARRAY;
        }
        int count = referenceExpressions.length;
        String[] result = ArrayUtil.newStringArray(count);

        for (int i = 0; i < count; ++i) {
            result[i] = referenceExpressions[i].getText();
        }
        return result;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public JSClass[] getReferencedClasses() {
        String[] texts = getReferenceTexts();

        if (texts.length == 0) {
            return JSClass.EMPTY_ARRAY;
        }

        Project project = getProject();
        ArrayList<JSClass> supers = new ArrayList<>(1);

        for (String text : texts) {
            int index = supers.size();

            text = JSImportHandlingUtil.resolveTypeName(text, this);

            Collection<JSQualifiedNamedElement> candidates = StubIndex.getElements(JavaScriptIndexKeys.ELEMENTS_BY_QNAME,
                text,
                project,
                getResolveScope(),
                JSQualifiedNamedElement.class
            );
            for (JSQualifiedNamedElement _clazz : candidates) {
                if (!(_clazz instanceof JSClass)) {
                    continue;
                }
                JSClass clazz = (JSClass)_clazz;

                if (text.equals(clazz.getQualifiedName())) {
                    if (clazz.canNavigate()) {
                        supers.add(index, clazz);
                    }
                    else {
                        supers.add(clazz);
                    }
                }
            }

            if (candidates.size() == 0) {
                PsiElement element = JSClassImpl.findClassFromNamespace(text, this);
                if (element instanceof JSClass jsClass) {
                    supers.add(jsClass);
                }
            }
        }

        return supers.toArray(new JSClass[supers.size()]);
    }
}