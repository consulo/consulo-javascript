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

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttribute;
import com.intellij.lang.javascript.psi.JSAttributeNameValuePair;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.stubs.JSAttributeStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.document.util.TextRange;
import consulo.language.ast.ASTNode;
import consulo.language.psi.EmptyResolveMessageProvider;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.util.collection.ArrayUtil;
import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 */
public class JSAttributeImpl extends JSStubElementImpl<JSAttributeStub> implements JSAttribute {
    private PsiReference[] myReferences;
    private static
    String[] myPossibleMetaData = new String[]{
        "AccessibilityClass",
        "ArrayElementType",
        "Bindable",
        "DefaultProperty",
        "Deprecated",
        "Effect",
        "Embed",
        "Event",
        "Exclude",
        "ExcludeClass",
        "IconFile",
        "Inspectable",
        "InstanceType",
        "HostComponent",
        "NonCommittingChangeEvent",
        "Frame",
        "RemoteClass",
        "ResourceBundle",
        "Style",
        "Transient"
    };

    public JSAttributeImpl(ASTNode node) {
        super(node);
    }

    public JSAttributeImpl(JSAttributeStub node) {
        super(node, JSElementTypes.ATTRIBUTE);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSAttribute(this);
    }

    @Override
    @RequiredReadAction
    public String getName() {
        JSAttributeStub attributeStub = getStub();
        if (attributeStub != null) {
            return attributeStub.getName();
        }
        ASTNode node = getNode().findChildByType(JSTokenTypes.IDENTIFIER);
        return node != null ? node.getText() : null;
    }

    @Override
    @RequiredWriteAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        throw new IncorrectOperationException();
    }

    @Override
    public JSAttributeNameValuePair[] getValues() {
        return getStubOrPsiChildren(JSElementTypes.ATTRIBUTE_NAME_VALUE_PAIR, JSAttributeNameValuePair.EMPTY_ARRAY);
    }

    @Override
    public JSAttributeNameValuePair getValueByName(String name) {
        for (JSAttributeNameValuePair p : getValues()) {
            String pName = p.getName();

            if ((name != null && name.equals(pName)) || (name == null && name == pName)) {
                return p;
            }
        }
        return null;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public PsiReference[] getReferences() {
        if (myReferences == null) {
            ASTNode node = getNode().findChildByType(JSTokenTypes.IDENTIFIER);

            if (node == null) {
                myReferences = PsiReference.EMPTY_ARRAY;
            }
            else {
                int startOffsetInParent = node.getPsi().getStartOffsetInParent();
                TextRange range = new TextRange(startOffsetInParent, startOffsetInParent + node.getTextLength());

                myReferences = new PsiReference[]{new AttrNameReference(range)};
            }
        }

        return myReferences;
    }

    private class AttrNameReference implements PsiReference, EmptyResolveMessageProvider {
        private final TextRange myRange;

        public AttrNameReference(TextRange range) {
            myRange = range;
        }

        @Override
        @RequiredReadAction
        public PsiElement getElement() {
            return JSAttributeImpl.this;
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public TextRange getRangeInElement() {
            return myRange;
        }

        @Override
        @RequiredReadAction
        public PsiElement resolve() {
            String s = getCanonicalText();
            return ArrayUtil.indexOf(myPossibleMetaData, s) >= 0 ? JSAttributeImpl.this : null;
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public String getCanonicalText() {
            return getName();
        }

        @Override
        @RequiredWriteAction
        public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
            return null;
        }

        @Override
        @RequiredWriteAction
        public PsiElement bindToElement(@Nonnull PsiElement element) throws IncorrectOperationException {
            return null;
        }

        @Override
        @RequiredReadAction
        public boolean isReferenceTo(PsiElement element) {
            if (element instanceof JSAttribute attribute) {
                String name = getName();
                return name != null && name.equals(attribute.getName());
            }
            return false;
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public Object[] getVariants() {
            return myPossibleMetaData;
        }

        @Override
        @RequiredReadAction
        public boolean isSoft() {
            return true;
        }

        @Nonnull
        @Override
        public LocalizeValue buildUnresolvedMessage(@Nonnull String referenceText) {
            return LocalizeValue.localizeTODO("Unknown metadata tag");
        }
    }
}