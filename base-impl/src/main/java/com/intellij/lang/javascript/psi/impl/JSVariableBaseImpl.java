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

import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.stubs.JSVariableStubBase;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.application.util.RecursionManager;
import consulo.content.scope.SearchScope;
import consulo.javascript.impl.language.psi.JSStubElementType;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;

import java.util.function.Supplier;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSVariableBaseImpl<T extends JSVariableStubBase<T2>, T2 extends JSVariable>
    extends JSStubElementImpl<T> implements JSVariable {
    protected JSVariableBaseImpl(ASTNode node) {
        super(node);
    }

    protected JSVariableBaseImpl(T stub, JSStubElementType<T, T2> elementType) {
        super(stub, elementType);
    }

    @Override
    @RequiredReadAction
    public boolean hasInitializer() {
        return getInitializerText() != null;
    }

    @Override
    @RequiredReadAction
    public JSExpression getInitializer() {
        PsiElement element = findChildByType(JSTokenTypes.EQ);
        if (element == null) {
            return null;
        }

        while ((element = element.getNextSibling()) != null) {
            if (element instanceof JSExpression expression) {
                return expression;
            }
        }
        return null;
    }

    @Override
    @RequiredReadAction
    public String getInitializerText() {
        T stub = getStub();
        if (stub != null) {
            return stub.getInitializerText();
        }

        JSExpression expression = getInitializer();
        return expression != null ? expression.getText() : null;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public SearchScope getUseScope() {
        return JSResolveUtil.findUseScope(this);
    }

    @Override
    @RequiredReadAction
    public String getName() {
        T stub = getStub();
        if (stub != null) {
            return stub.getName();
        }
        PsiElement name = getNameIdentifier();

        if (name instanceof JSReferenceExpression refExpr) {
            return refExpr.getReferencedName();
        }
        return name != null ? name.getText() : "";
    }

    @Override
    public JSAttributeList getAttributeList() {
        if (getParent() instanceof JSVarStatement varStatement) {
            return ((JSVarStatementImpl)varStatement).getStubOrPsiChild(JSElementTypes.ATTRIBUTE_LIST);
        }
        return null;
    }

    @Override
    public void setInitializer(JSExpression expr) throws IncorrectOperationException {
        throw new UnsupportedOperationException("TODO: implement");
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public JavaScriptType getType() {
        JSExpression initializer = getInitializer();
        if (initializer != null) {
            if (initializer instanceof JSObjectLiteralExpression lit) {
                return new JavaScriptSimpleType(getName(), lit);
            }
            
            JavaScriptType javaScriptType = RecursionManager.doPreventingRecursion(
                this,
                false,
                new Supplier<JavaScriptType>() {
                    @Override
                    @RequiredReadAction
                    public JavaScriptType get() {
                        return initializer.getType();
                    }
                }
            );
            return javaScriptType == null ? JavaScriptType.UNKNOWN : javaScriptType;
        }
        return JavaScriptType.UNKNOWN;
    }

    @Override
    @RequiredReadAction
    public String getTypeString() {
        T stub = getStub();
        return stub != null ? stub.getTypeString() : doGetType();
    }

    @RequiredReadAction
    @Override
    public JavaScriptTypeElement getTypeElement() {
        return JSPsiImplUtils.findTypeElement(this);
    }

    @RequiredReadAction
    protected String doGetType() {
        return JSPsiImplUtils.getType(this);
    }

    @Override
    @RequiredWriteAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        PsiElement nameNode = getNameIdentifier();
        if (nameNode == null) {
            return this;
        }
        ASTNode nameElement = JSChangeUtil.createNameIdentifier(getProject(), name);
        getNode().replaceChild(nameNode.getNode(), nameElement);
        return this;
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSVariable(this);
    }

    @RequiredReadAction
    @Override
    public int getTextOffset() {
        PsiElement name = getNameIdentifier();
        return name != null ? name.getTextOffset() : super.getTextOffset();
    }

    @Override
    @RequiredReadAction
    public boolean isConst() {
        T stub = getStub();
        if (stub != null) {
            return stub.isConst();
        }
        ASTNode parent = getNode().getTreeParent();

        if (parent.getElementType() == JSElementTypes.VAR_STATEMENT) {
            ASTNode node = parent.getFirstChildNode();
            IElementType type = node.getElementType();

            if (type == JSElementTypes.ATTRIBUTE_LIST || type == JSElementTypes.REFERENCE_EXPRESSION) {
                node = node.getTreeNext();

                if (node != null && node.getElementType() == JSTokenTypes.WHITE_SPACE) {
                    node = node.getTreeNext();
                }
            }
            return node != null && node.getElementType() == JSTokenTypes.CONST_KEYWORD;
        }

        return false;
    }

    @Override
    @RequiredReadAction
    public boolean isLocal() {
        T stub = getStub();
        if (stub != null) {
            return stub.isLocal();
        }
        ASTNode parent = getNode().getTreeParent();
        return parent.getElementType() == JSElementTypes.VAR_STATEMENT
            && parent.getFirstChildNode().getElementType() == JSTokenTypes.LET_KEYWORD;
    }

    @Override
    public boolean isDeprecated() {
        T stub = getStub();
        if (stub != null) {
            return stub.isDeprecated();
        }
        return JSDocumentationUtils.calculateDeprecated(this);
    }

    @Override
    @RequiredWriteAction
    public void delete() throws IncorrectOperationException {
        ASTNode myNode = getNode();
        ASTNode parent = myNode.getTreeParent();

        if (parent.getElementType() == JSElementTypes.VAR_STATEMENT) {
            JSVariable[] jsVariables = ((JSVarStatement)parent.getPsi()).getVariables();

            if (jsVariables.length == 1) {
                parent.getPsi().delete();
            }
            else {
                JSChangeUtil.removeRangeWithRemovalOfCommas(myNode, parent);
            }
            return;
        }

        throw new IncorrectOperationException("Cannot delete variable from parent : " + parent.getElementType());
    }

    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        if (lastParent != null && lastParent.getParent() == this) {
            processor.handleEvent(PsiScopeProcessor.Event.SET_DECLARATION_HOLDER, this);
        }
        return processor.execute(this, state);
    }

    @Override
    @RequiredReadAction
    public String getQualifiedName() {
        T stub = getStub();
        return stub != null ? stub.getQualifiedName() : JSPsiImplUtils.getQName(this);
    }

    @Override
    @RequiredReadAction
    public PsiElement getNameIdentifier() {
        return findChildByType(JavaScriptTokenSets.NAME_TOKEN_TYPES);
    }
}
