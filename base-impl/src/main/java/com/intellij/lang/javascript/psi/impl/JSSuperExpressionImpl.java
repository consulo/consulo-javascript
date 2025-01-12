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
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.document.util.TextRange;
import consulo.language.ast.ASTNode;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.resolve.ResolveState;
import consulo.util.collection.ArrayUtil;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSSuperExpressionImpl extends JSExpressionImpl implements JSSuperExpression {
    public JSSuperExpressionImpl(ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSSuperExpression(this);
    }

    @Override
    public PsiReference getReference() {
        return getReferences()[0];
    }

    @Override
    @Nonnull
    public PsiReference[] getReferences() {
        PsiReference[] refs = {
            new PsiReference() {
                @Override
                @RequiredReadAction
                public PsiElement getElement() {
                    return JSSuperExpressionImpl.this;
                }

                @Nonnull
                @Override
                @RequiredReadAction
                public TextRange getRangeInElement() {
                    return new TextRange(0, getTextLength());
                }

                @Nullable
                @Override
                @RequiredReadAction
                public PsiElement resolve() {
                    PsiElement element = findClass();

                    if (getElement().getParent() instanceof JSCallExpression && element instanceof JSClass jsClass) {
                        ResolveProcessor processor = new ResolveProcessor(jsClass.getName(), JSSuperExpressionImpl.this);
                        element.processDeclarations(processor, ResolveState.initial(), jsClass, getElement());
                        if (processor.getResult() != null) {
                            return processor.getResult();
                        }
                    }

                    return element;
                }

                @RequiredReadAction
                private PsiElement findClass() {
                    JSClass jsClass = PsiTreeUtil.getParentOfType(getElement(), JSClass.class);

                    if (jsClass != null) {
                        JSReferenceList extendsList = jsClass.getExtendsList();
                        if (extendsList != null) {
                            JSReferenceExpression[] referenceExpressions = extendsList.getExpressions();
                            if (referenceExpressions != null && referenceExpressions.length > 0) {
                                ResolveResult[] results = referenceExpressions[0].multiResolve(false);
                                return results.length > 0 ? results[0].getElement() : null;
                            }
                        }
                    }
                    else {
                        JSFile jsFile = PsiTreeUtil.getParentOfType(getElement(), JSFile.class);

                        if (jsFile != null) {
                            return JSResolveUtil.getClassReferenceForXmlFromContext(jsFile);
                        }
                    }
                    return null;
                }

                @Nonnull
                @Override
                @RequiredReadAction
                public String getCanonicalText() {
                    return getText();
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
                    return false;
                }

                @Nonnull
                @Override
                @RequiredReadAction
                public Object[] getVariants() {
                    return ArrayUtil.EMPTY_OBJECT_ARRAY;
                }

                @Override
                @RequiredReadAction
                public boolean isSoft() {
                    return true;
                }
            }
        };
        return refs;
    }
}