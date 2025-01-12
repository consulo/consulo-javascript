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

import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttribute;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.stubs.JSAttributeListStub;
import consulo.language.ast.TokenSet;
import consulo.language.psi.PsiElement;
import consulo.language.ast.IElementType;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;

import jakarta.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author Maxim.Mossienko
 */
public class JSAttributeListImpl extends JSStubElementImpl<JSAttributeListStub> implements JSAttributeList {
    private static final TokenSet MODIFIERS_TYPE_SET = TokenSet.create(
        JSTokenTypes.PUBLIC_KEYWORD,
        JSTokenTypes.PRIVATE_KEYWORD,
        JSTokenTypes.PROTECTED_KEYWORD,
        JSTokenTypes.INTERNAL_KEYWORD
    );

    private static final Map<IElementType, AccessType> ACCESS_TYPE_MAP = Map.of(
        JSTokenTypes.PUBLIC_KEYWORD, AccessType.PUBLIC,
        JSTokenTypes.PROTECTED_KEYWORD, AccessType.PROTECTED,
        JSTokenTypes.PRIVATE_KEYWORD, AccessType.PRIVATE,
        JSTokenTypes.INTERNAL_KEYWORD, AccessType.PACKAGE_LOCAL
    );

    public JSAttributeListImpl(ASTNode node) {
        super(node);
    }

    public JSAttributeListImpl(JSAttributeListStub stub) {
        super(stub, JSElementTypes.ATTRIBUTE_LIST);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSAttributeList(this);
    }

    @Nullable
    @Override
    @RequiredReadAction
    public String getNamespace() {
        JSAttributeListStub attributeListStub = getStub();
        if (attributeListStub != null) {
            return attributeListStub.getNamespace();
        }
        JSReferenceExpression namespaceElement = getNamespaceElement();
        return namespaceElement != null ? namespaceElement.getText() : null;
    }

    @Override
    @RequiredReadAction
    public JSReferenceExpression getNamespaceElement() {
        ASTNode node = getNode().findChildByType(JSElementTypes.REFERENCE_EXPRESSION);
        return node != null ? (JSReferenceExpression)node.getPsi() : null;
    }

    @Override
    public JSAttribute[] getAttributes() {
        return getStubOrPsiChildren(JSElementTypes.ATTRIBUTE, JSAttribute.ARRAY_FACTORY);
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public JSAttribute[] getAttributesByName(@Nonnull String name) {
        List<JSAttribute> attributes = null;
        for (JSAttribute attr : getAttributes()) {
            if (name.equals(attr.getName())) {
                if (attributes == null) {
                    attributes = new ArrayList<>();
                }
                attributes.add(attr);
            }
        }
        return attributes != null ? attributes.toArray(new JSAttribute[attributes.size()]) : JSAttribute.EMPTY_ARRAY;
    }

    @Override
    @RequiredReadAction
    public AccessType getAccessType() {
        JSAttributeListStub stub = getStub();
        if (stub != null) {
            return stub.getAccessType();
        }

        ASTNode node = getNode().findChildByType(MODIFIERS_TYPE_SET);
        return ACCESS_TYPE_MAP.getOrDefault(node != null ? node.getElementType() : null, AccessType.PACKAGE_LOCAL);
    }

    @Override
    @RequiredReadAction
    public PsiElement findAccessTypeElement() {
        ASTNode modifier = getNode().findChildByType(MODIFIERS_TYPE_SET);
        return modifier != null ? modifier.getPsi() : null;
    }

    @Override
    @RequiredReadAction
    public boolean hasModifier(ModifierType modifier) {
        JSAttributeListStub stub = getStub();
        if (stub != null) {
            return stub.hasModifier(modifier);
        }

        IElementType type = switch (modifier) {
            case DYNAMIC -> JSTokenTypes.DYNAMIC_KEYWORD;
            case OVERRIDE -> JSTokenTypes.OVERRIDE_KEYWORD;
            case NATIVE -> JSTokenTypes.NATIVE_KEYWORD;
            case STATIC -> JSTokenTypes.STATIC_KEYWORD;
            case FINAL -> JSTokenTypes.FINAL_KEYWORD;
            case VIRTUAL -> JSTokenTypes.VIRTUAL_KEYWORD;
            default -> null;
        };
        return type != null && getNode().findChildByType(type) != null;
    }

    @Override
    @RequiredWriteAction
    public PsiElement add(@Nonnull PsiElement element) throws IncorrectOperationException {
        if (element.getNode().getElementType() == JSTokenTypes.OVERRIDE_KEYWORD) {
            return JSChangeUtil.doDoAddBefore(this, element, getFirstChild());
        }
        return JSChangeUtil.doDoAddAfter(this, element, getLastChild());
    }
}