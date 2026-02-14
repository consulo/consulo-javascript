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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.application.util.CachedValueProvider;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.psi.JSComputedName;
import consulo.javascript.psi.impl.reference.JSPropertyNameReferenceProvider;
import consulo.language.ast.ASTNode;
import consulo.language.ast.TokenSet;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiModificationTracker;
import consulo.language.psi.PsiReference;
import consulo.language.psi.util.LanguageCachedValueUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.util.lang.StringUtil;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author max
 */
public class JSPropertyImpl extends JSElementImpl implements JSProperty {
    private static TokenSet IDENTIFIER_TOKENS_SET =
        TokenSet.orSet(TokenSet.create(JSTokenTypes.NUMERIC_LITERAL, JSTokenTypes.IDENTIFIER), JavaScriptTokenSets.STRING_LITERALS);

    public JSPropertyImpl(ASTNode node) {
        super(node);
    }

    @Override
    @Nonnull
    public PsiReference[] getReferences() {
        return LanguageCachedValueUtil.getCachedValue(this, new CachedValueProvider<PsiReference[]>() {
            @Nullable
            @Override
            @RequiredReadAction
            public Result<PsiReference[]> compute() {
                return Result.create(
                    buildReferences(),
                    JSPropertyImpl.this,
                    PsiModificationTracker.MODIFICATION_COUNT
                );
            }
        });
    }

    @Nonnull
    @RequiredReadAction
    private PsiReference[] buildReferences() {
        PsiElement nameIdentifier = getNameIdentifier();
        if (nameIdentifier == null) {
            return PsiReference.EMPTY_ARRAY;
        }
        PsiReference reference = JSPropertyNameReferenceProvider.EP_NAME.computeSafeIfAny(it -> it.getReference(this));
        if (reference != null) {
            return new PsiReference[]{reference};
        }
        return PsiReference.EMPTY_ARRAY;
    }

    @Override
    @RequiredReadAction
    public String getName() {
        PsiElement nameIdentifier = getNameIdentifier();
        return nameIdentifier != null ? StringUtil.stripQuotesAroundValue(nameIdentifier.getText()) : null;
    }

    @Override
    @RequiredWriteAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        PsiElement nameNode = getNameIdentifier();
        assert nameNode != null;
        ASTNode nameElement = JSChangeUtil.createNameIdentifier(getProject(), name, nameNode.getNode().getElementType());
        getNode().replaceChild(nameNode.getNode(), nameElement);
        return this;
    }

    @RequiredReadAction
    @Nonnull
    @Override
    public JavaScriptType getType() {
        JSExpression value = getValue();
        if (value != null) {
            return value.getType();
        }
        return JavaScriptType.UNKNOWN;
    }

    @RequiredReadAction
    @Override
    public JSExpression getValue() {
        return findChildByClass(JSExpression.class);
    }

    @RequiredReadAction
    @Nullable
    @Override
    public PsiElement getColonElement() {
        return findChildByType(JSTokenTypes.COLON);
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSComputedName getComputedName() {
        return findChildByClass(JSComputedName.class);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSProperty(this);
    }

    @RequiredReadAction
    @Override
    public int getTextOffset() {
        PsiElement name = getNameIdentifier();
        return name != null ? name.getTextOffset() : super.getTextOffset();
    }

    @Override
    @RequiredWriteAction
    public void delete() throws IncorrectOperationException {
        ASTNode myNode = getNode();
        JSChangeUtil.removeRangeWithRemovalOfCommas(myNode, myNode.getTreeParent());
    }

    @Override
    @RequiredReadAction
    public PsiElement getNameIdentifier() {
        PsiElement nameIdentifier = findChildByType(IDENTIFIER_TOKENS_SET);
        if (nameIdentifier == null) {
            JSExpression value = getValue();
            // auto-property name
            if (value != null && getColonElement() == null) {
                return value;
            }
        }
        return nameIdentifier;
    }
}
