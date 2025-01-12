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
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSLabeledStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.document.util.TextRange;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;
import consulo.language.psi.resolve.PsiElementProcessor;
import consulo.language.util.IncorrectOperationException;
import consulo.language.ast.ASTNode;
import consulo.util.collection.ArrayUtil;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * @author max
 * @since 2005-01-30
 */
abstract class JSStatementWithLabelReferenceImpl extends JSStatementImpl {
    private PsiReference[] myReferences;
    private String myReferencesText;

    protected JSStatementWithLabelReferenceImpl(ASTNode node) {
        super(node);
    }

    @RequiredReadAction
    public String getLabel() {
        ASTNode label = getNode().findChildByType(JSTokenTypes.IDENTIFIER);
        return label != null ? label.getText() : null;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public PsiReference[] getReferences() {
        String text = getText();

        if (!text.equals(myReferencesText) || myReferences == null) {
            ASTNode label = getNode().findChildByType(JSTokenTypes.IDENTIFIER);
            if (label != null) {
                myReferences = new PsiReference[]{new JSStatementWithLabelReferenceImpl.LabelReference(label.getPsi())};
            }
            else {
                myReferences = PsiReference.EMPTY_ARRAY;
            }
            myReferencesText = text;
        }
        return myReferences;
    }

    private class LabelReference implements PsiReference {
        private PsiElement labelNode;

        LabelReference(PsiElement _labelNode) {
            labelNode = _labelNode;
        }

        @Override
        @RequiredReadAction
        public PsiElement getElement() {
            return JSStatementWithLabelReferenceImpl.this;
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public TextRange getRangeInElement() {
            int startOffsetInParent = labelNode.getStartOffsetInParent();
            return new TextRange(startOffsetInParent, startOffsetInParent + labelNode.getTextLength());
        }

        @Override
        @Nullable
        @RequiredReadAction
        public PsiElement resolve() {
            PsiElement[] result = new PsiElement[1];

            processElements(new PsiElementProcessor<>() {
                private final String label = getCanonicalText();

                @Override
                @RequiredReadAction
                public boolean execute(@Nonnull JSLabeledStatement element) {
                    if (label.equals(element.getLabel())) {
                        result[0] = element;
                        return false;
                    }
                    return true;
                }
            });

            return result[0];
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public String getCanonicalText() {
            return labelNode.getText();
        }

        @Override
        @RequiredWriteAction
        public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
            JSChangeUtil.doIdentifierReplacement(getElement(), labelNode, newElementName);
            return getElement();
        }

        @Override
        @RequiredWriteAction
        public PsiElement bindToElement(@Nonnull PsiElement element) throws IncorrectOperationException {
            return null;
        }

        @Override
        @RequiredReadAction
        public boolean isReferenceTo(PsiElement element) {
            return getManager().areElementsEquivalent(resolve(), element);
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public Object[] getVariants() {
            final List<String> labels = new ArrayList<>(1);
            processElements(element -> {
                labels.add(element.getLabel());
                return true;
            });
            return ArrayUtil.toStringArray(labels);
        }

        private void processElements(PsiElementProcessor<JSLabeledStatement> processor) {
            PsiElement run = getParent();
            while (run != null) {
                if (run instanceof JSLabeledStatement labeledStatement && !processor.execute(labeledStatement)) {
                    return;
                }

                if (run instanceof JSFunction) {
                    break;
                }
                run = run.getParent();
            }
        }

        @Override
        @RequiredReadAction
        public boolean isSoft() {
            return false;
        }
    }
}
