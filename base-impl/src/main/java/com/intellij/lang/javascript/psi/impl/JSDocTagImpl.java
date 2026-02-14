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

import com.intellij.lang.javascript.JSDocTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.util.JSLookupUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.document.util.TextRange;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.ASTNode;
import consulo.language.editor.util.PsiUtilBase;
import consulo.language.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.util.collection.ArrayUtil;
import jakarta.annotation.Nonnull;

public class JSDocTagImpl extends JSElementImpl implements JSDocTag {
    private volatile PsiReference[] myRefs;
    private volatile long myModificationCount = -1;

    public JSDocTagImpl(ASTNode node) {
        super(node);
    }

    @Override
    @RequiredReadAction
    public String getName() {
        PsiElement element = findChildByType(JSDocTokenTypes.DOC_TAG_NAME);
        return element != null ? element.getText().substring(1) : null;
    }

    @Override
    @RequiredWriteAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        throw new IncorrectOperationException();
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSDocTag(this);
    }

    @Override
    public JSDocTagValue getValue() {
        return findChildByClass(JSDocTagValue.class);
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public PsiReference[] getReferences() {
        long count = getManager().getModificationTracker().getModificationCount();

        if (count != myModificationCount) {
            String name = getName();
            PsiReference[] result = PsiReference.EMPTY_ARRAY;

            if ("param".equals(name)) {
                PsiElement data = findChildByType(JSDocTokenTypes.DOC_COMMENT_DATA);
                if (data != null) {
                    result = new PsiReference[]{new ParamReference(this)};
                }
            }

            myRefs = result;
            myModificationCount = count;
        }

        return myRefs;
    }

    private static class ParamReference implements PsiReference, EmptyResolveMessageProvider {
        private PsiElement myJsDocTagValue;
        private TextRange myRange;

        public ParamReference(JSDocTagImpl elt) {
            reset(elt);
        }

        private void reset(JSDocTagImpl elt) {
            myJsDocTagValue = elt.findChildByType(JSDocTokenTypes.DOC_COMMENT_DATA);
            int offsetInParent = myJsDocTagValue.getStartOffsetInParent();
            int textLength;

            if (myJsDocTagValue.textContains('[')) {
                String text = myJsDocTagValue.getText();
                int at = text.indexOf('[');
                offsetInParent += at + 1;

                // @param [name] | //@param[name="something"] | [obj.prop2(='somestring')?]
                int rBracketIndex = text.indexOf(']');
                int eqIndex = text.indexOf('=');
                int dotIndex = text.indexOf('.');
                int combinedIndex = text.length();

                if (rBracketIndex != -1) {
                    combinedIndex = rBracketIndex;
                }
                if (eqIndex != -1) {
                    combinedIndex = eqIndex;
                }
                if (dotIndex != -1 && (eqIndex == -1 || dotIndex < eqIndex)) {
                    combinedIndex = dotIndex;
                }
                textLength = combinedIndex - at - 1;

            }
            else if (myJsDocTagValue.textContains('=')) {
                textLength = myJsDocTagValue.getText().indexOf('='); // @param name=""
            }
            else if (myJsDocTagValue.textContains('.')) { //@param userInfo.email
                textLength = myJsDocTagValue.getText().indexOf('.');
            }
            else {
                textLength = myJsDocTagValue.getTextLength();
            }
            myRange = new TextRange(offsetInParent, offsetInParent + textLength);
        }

        @Override
        @RequiredReadAction
        public PsiElement getElement() {
            return myJsDocTagValue.getParent();
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public TextRange getRangeInElement() {
            return myRange;
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public String getCanonicalText() {
            int offsetInText = myRange.getStartOffset() - myJsDocTagValue.getStartOffsetInParent();
            return myJsDocTagValue.getText().substring(offsetInText, offsetInText + myRange.getLength());
        }

        @Override
        @RequiredWriteAction
        public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
            JSDocTag jsDocTag = (JSDocTag)myJsDocTagValue.getParent();
            ElementManipulator<JSDocTag> manipulator = ElementManipulators.getManipulator(jsDocTag);
            jsDocTag = manipulator.handleContentChange(jsDocTag, myRange, newElementName);
            reset((JSDocTagImpl)jsDocTag);
            return myJsDocTagValue;
        }

        @Override
        @RequiredWriteAction
        public PsiElement bindToElement(@Nonnull PsiElement element) throws IncorrectOperationException {
            return null;
        }

        @Override
        @RequiredReadAction
        public boolean isReferenceTo(PsiElement element) {
            return element.isEquivalentTo(resolve());
        }

        @RequiredReadAction
        private static JSParameterList findParameterList(PsiElement elt) {
            if (elt == null) {
                return null;
            }
            PsiComment psiComment = PsiTreeUtil.getParentOfType(elt, PsiComment.class);
            if (psiComment == null) {
                return null;
            }
            PsiElement parent = psiComment.getParent();
            if (parent instanceof PsiComment comment) {
                psiComment = comment;
            }

            PsiElement next = psiComment.getNextSibling();
            if (next instanceof PsiWhiteSpace whiteSpace) {
                next = whiteSpace.getNextSibling();
            }
            if (next instanceof PsiComment comment) {
                next = comment.getNextSibling();
                if (next instanceof PsiWhiteSpace whiteSpace) {
                    next = whiteSpace.getNextSibling();
                }
            }

            if (next instanceof JSExpressionStatement expressionStatement) {
                if (expressionStatement.getExpression() instanceof JSAssignmentExpression assignment) {
                    JSExpression roperand = assignment.getROperand();
                    if (roperand instanceof JSNewExpression newExpression) {
                        roperand = newExpression.getMethodExpression();
                    }

                    if (roperand instanceof JSFunctionExpression functionExpr) {
                        next = functionExpr;
                    }
                }
            }
            else if (next instanceof JSProperty property) {
                next = property.getValue();
            }
            else if (next instanceof JSVarStatement varStatement) {
                JSVariable[] variables = varStatement.getVariables();
                if (variables.length > 0 && variables[0].getInitializer() instanceof JSFunctionExpression functionExpr) {
                    next = functionExpr;
                }
            }
            else if (next != null) {
                if (next instanceof JSVariable variable && variable.getInitializer() instanceof JSFunctionExpression functionExpr) {
                    next = functionExpr;
                }
                if (next.getParent() instanceof JSFunction function) {
                    next = function;
                }
            }
            if (next instanceof JSFunction function) {
                return function.getParameterList();
            }

            return null;
        }

        @Override
        @RequiredReadAction
        public PsiElement resolve() {
            JSParameterList parameterList = findParameterList(getElement());

            if (parameterList != null) {
                String name = getCanonicalText();
                for (JSParameter param : parameterList.getParameters()) {
                    if (name.equals(param.getName())) {
                        return param;
                    }
                }
            }

            return null;
        }

        @Nonnull
        @Override
        @RequiredReadAction
        public Object[] getVariants() {
            PsiElement elt = getElement();
            JSParameterList parameterList = findParameterList(PsiUtilBase.getOriginalElement(elt, elt.getClass()));

            if (parameterList != null) {
                JSParameter[] parameters = parameterList.getParameters();
                Object[] result = new Object[parameters.length];

                for (int i = 0; i < parameters.length; ++i) {
                    result[i] = JSLookupUtil.createLookupItem(parameters[i], parameters[i].getName(), JSLookupUtil.LookupPriority.HIGHEST);
                }
                return result;
            }

            return ArrayUtil.EMPTY_OBJECT_ARRAY;
        }

        @Nonnull
        @Override
        public LocalizeValue buildUnresolvedMessage(@Nonnull String referenceText) {
            return JavaScriptLocalize.javascriptValidationMessageIncorrectParameterName();
        }
    }
}