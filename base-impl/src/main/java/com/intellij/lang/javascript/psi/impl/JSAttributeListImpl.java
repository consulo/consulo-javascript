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

/**
 * @author Maxim.Mossienko
 */
public class JSAttributeListImpl extends JSStubElementImpl<JSAttributeListStub> implements JSAttributeList {
    private static final TokenSet ourModifiersTypeSet = TokenSet.create(
        JSTokenTypes.PUBLIC_KEYWORD,
        JSTokenTypes.PRIVATE_KEYWORD,
        JSTokenTypes.PROTECTED_KEYWORD,
        JSTokenTypes.INTERNAL_KEYWORD
    );

    public JSAttributeListImpl(final ASTNode node) {
        super(node);
    }

    public JSAttributeListImpl(final JSAttributeListStub stub) {
        super(stub, JSElementTypes.ATTRIBUTE_LIST);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSAttributeList(this);
    }

    @Override
    @Nullable
    public String getNamespace() {
        final JSAttributeListStub attributeListStub = getStub();
        if (attributeListStub != null) {
            return attributeListStub.getNamespace();
        }
        final JSReferenceExpression namespaceElement = getNamespaceElement();
        return namespaceElement != null ? namespaceElement.getText() : null;
    }

    @Override
    public JSReferenceExpression getNamespaceElement() {
        final ASTNode node = getNode().findChildByType(JSElementTypes.REFERENCE_EXPRESSION);
        return node != null ? (JSReferenceExpression)node.getPsi() : null;
    }

    @Override
    public JSAttribute[] getAttributes() {
        return getStubOrPsiChildren(JSElementTypes.ATTRIBUTE, JSAttribute.ARRAY_FACTORY);
    }

    @Nonnull
    @Override
    public JSAttribute[] getAttributesByName(final @Nonnull String name) {
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
    public AccessType getAccessType() {
        final JSAttributeListStub stub = getStub();
        if (stub != null) {
            return stub.getAccessType();
        }

        final ASTNode node = getNode().findChildByType(ourModifiersTypeSet);
        if (node != null) {
            final IElementType nodeType = node.getElementType();
            if (nodeType == JSTokenTypes.PUBLIC_KEYWORD) {
                return AccessType.PUBLIC;
            }
            if (nodeType == JSTokenTypes.PROTECTED_KEYWORD) {
                return AccessType.PROTECTED;
            }
            if (nodeType == JSTokenTypes.PRIVATE_KEYWORD) {
                return AccessType.PRIVATE;
            }
            if (nodeType == JSTokenTypes.INTERNAL_KEYWORD) {
                return AccessType.PACKAGE_LOCAL;
            }
        }
        return AccessType.PACKAGE_LOCAL;
    }

    @Override
    public PsiElement findAccessTypeElement() {
        final ASTNode modifier = getNode().findChildByType(ourModifiersTypeSet);
        return modifier != null ? modifier.getPsi() : null;
    }

    @Override
    public boolean hasModifier(final ModifierType modifier) {
        final JSAttributeListStub stub = getStub();
        if (stub != null) {
            return stub.hasModifier(modifier);
        }

        IElementType type = null;
        switch (modifier) {
            case DYNAMIC:
                type = JSTokenTypes.DYNAMIC_KEYWORD;
                break;
            case OVERRIDE:
                type = JSTokenTypes.OVERRIDE_KEYWORD;
                break;
            case NATIVE:
                type = JSTokenTypes.NATIVE_KEYWORD;
                break;
            case STATIC:
                type = JSTokenTypes.STATIC_KEYWORD;
                break;
            case FINAL:
                type = JSTokenTypes.FINAL_KEYWORD;
                break;
            case VIRTUAL:
                type = JSTokenTypes.VIRTUAL_KEYWORD;
                break;
        }
        return type != null && getNode().findChildByType(type) != null;
    }

    @Override
    public PsiElement add(@Nonnull final PsiElement element) throws IncorrectOperationException {
        if (element.getNode().getElementType() == JSTokenTypes.OVERRIDE_KEYWORD) {
            return JSChangeUtil.doDoAddBefore(this, element, getFirstChild());
        }
        return JSChangeUtil.doDoAddAfter(this, element, getLastChild());
    }
}