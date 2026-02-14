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
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSSourceElement;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.stubs.JSPackageStatementStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.language.ast.ASTNode;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import consulo.virtualFileSystem.VirtualFile;
import jakarta.annotation.Nonnull;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
 */
public class JSPackageStatementImpl extends JSStubbedStatementImpl<JSPackageStatementStub> implements JSPackageStatement {
    public JSPackageStatementImpl(ASTNode node) {
        super(node);
    }

    public JSPackageStatementImpl(JSPackageStatementStub stub) {
        super(stub, JSElementTypes.PACKAGE_STATEMENT);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSPackageStatement(this);
    }

    @Override
    @RequiredReadAction
    public String getName() {
        JSPackageStatementStub stub = getStub();
        if (stub != null) {
            return stub.getName();
        }
        PsiElement node = getNameIdentifier();
        if (node != null) {
            return ((JSReferenceExpression)node).getReferencedName();
        }
        return null;
    }

    @Override
    @RequiredReadAction
    public String getQualifiedName() {
        JSPackageStatementStub stub = getStub();
        if (stub != null) {
            return stub.getQualifiedName();
        }

        PsiElement node = getNameIdentifier();
        return node != null ? node.getText() : null;
    }

    @Override
    public JSSourceElement[] getStatements() {
        return getStubOrPsiChildren(JSElementTypes.SOURCE_ELEMENTS, JSSourceElement.EMPTY_ARRAY);
    }

    @Override
    @RequiredWriteAction
    public void setQualifiedName(String expectedPackageNameFromFile) {
        doChangeName(getProject(), this, expectedPackageNameFromFile);
    }

    @Override
    @RequiredWriteAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        VirtualFile virtualFile = getContainingFile().getVirtualFile();
        String expectedPackageNameFromFile =
            JSResolveUtil.getExpectedPackageNameFromFile(virtualFile, getProject(), false);
        if (expectedPackageNameFromFile != null && expectedPackageNameFromFile.equals(getQualifiedName())) {
            try {
                JSPsiImplUtils.doRenameParentDirectoryIfNeeded(virtualFile, name, this);
            }
            catch (IOException ex) {
                throw new IncorrectOperationException("", ex);
            }
        }

        PsiElement child = getNameIdentifier();
        if (child instanceof JSReferenceExpression expr) {
            PsiElement element = expr.getReferenceNameElement();
            if (element != null) {
                JSChangeUtil.doIdentifierReplacement(expr, element, name);
            }
        }

        return this;
    }

    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState substitutor,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        return !(lastParent != null && lastParent.getParent() == this)
            || JSImportHandlingUtil.tryResolveImports(processor, this, place);
    }

    @Override
    @RequiredReadAction
    public PsiElement getNameIdentifier() {
        return findChildByType(JSElementTypes.REFERENCE_EXPRESSION);
    }

    @Override
    @RequiredWriteAction
    public PsiElement addBefore(@Nonnull PsiElement element, PsiElement anchor) throws IncorrectOperationException {
        if (JSChangeUtil.isStatementOrComment(element)) {
            PsiElement insertedElement = JSChangeUtil.doAddBefore(this, element, anchor);
            CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(getNode(), insertedElement.getNode());
            return insertedElement;
        }
        return super.addBefore(element, anchor);
    }

    @Override
    @RequiredWriteAction
    public PsiElement addAfter(@Nonnull PsiElement element, PsiElement anchor) throws IncorrectOperationException {
        if (JSChangeUtil.isStatementOrComment(element)) {
            PsiElement insertedElement = JSChangeUtil.doAddAfter(this, element, anchor);
            CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(getNode(), insertedElement.getNode());
            return insertedElement;
        }
        return super.addAfter(element, anchor);
    }

    @RequiredWriteAction
    public static void doChangeName(Project project, JSPackageStatement packageStatement, String expected) {
        if (expected == null) {
            return;
        }
        PsiElement node = packageStatement.getNameIdentifier();
        ASTNode parent = packageStatement.getNode();

        if (expected.length() == 0) {
            if (node != null) {
                ASTNode treeNext = node.getNode().getTreeNext();
                parent.removeChild(node.getNode());
                if (treeNext.getPsi() instanceof PsiWhiteSpace) {
                    parent.removeChild(treeNext);
                }
            }
        }
        else {
            ASTNode child = JSChangeUtil.createExpressionFromText(project, expected).getNode();
            if (node != null) {
                parent.replaceChild(node.getNode(), child);
            }
            else {
                parent.addChild(child, parent.findChildByType(JSTokenTypes.LBRACE));
            }
        }
    }
}
