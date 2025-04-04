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
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSIncludeDirective;
import com.intellij.lang.javascript.psi.stubs.JSIncludeDirectiveStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.application.util.SystemInfo;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiReference;
import consulo.language.psi.path.FileReference;
import consulo.language.psi.path.FileReferenceSet;
import consulo.util.lang.StringUtil;
import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 */
public class JSIncludeDirectiveImpl extends JSStubbedStatementImpl<JSIncludeDirectiveStub> implements JSIncludeDirective {
    public JSIncludeDirectiveImpl(ASTNode node) {
        super(node);
    }

    public JSIncludeDirectiveImpl(JSIncludeDirectiveStub stub) {
        super(stub, JSElementTypes.INCLUDE_DIRECTIVE);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSIncludeDirective(this);
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public PsiReference[] getReferences() {
        ASTNode node = getIncludedFileNode();

        if (node != null) {
            return new FileReferenceSet(
                StringUtil.stripQuotesAroundValue(node.getText()),
                this,
                node.getPsi().getStartOffsetInParent() + 1,
                null,
                SystemInfo.isFileSystemCaseSensitive
            ).getAllReferences();
        }
        return PsiReference.EMPTY_ARRAY;
    }

    @RequiredReadAction
    private ASTNode getIncludedFileNode() {
        return getNode().findChildByType(JavaScriptTokenSets.STRING_LITERALS);
    }

    @Override
    @RequiredReadAction
    public String getIncludeText() {
        JSIncludeDirectiveStub stub = getStub();
        if (stub != null) {
            return stub.getIncludeText();
        }
        ASTNode astNode = getIncludedFileNode();

        return astNode != null ? StringUtil.stripQuotesAroundValue(astNode.getText()) : null;
    }

    @Override
    @RequiredReadAction
    public PsiFile resolveFile() {
        String includeText = getIncludeText();
        if (includeText == null) {
            return null;
        }
        FileReference[] references = new FileReferenceSet(
            includeText,
            this,
            0,
            null,
            SystemInfo.isFileSystemCaseSensitive
        ).getAllReferences();

        return references != null && references.length > 0 && references[references.length - 1].resolve() instanceof PsiFile file
            ? file
            : null;
    }
}