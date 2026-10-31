/*
 * Copyright 2013-2026 consulo.io
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
package consulo.javascript.ecmascript.psi.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.impl.JSStatementImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.ecmascript.psi.ES6ExportDeclaration;
import consulo.javascript.ecmascript.psi.ES6ExportSpecifier;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.module.resolve.JavaScriptModuleReference;
import consulo.javascript.module.resolve.JavaScriptModuleRequest;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;
import org.jspecify.annotations.Nullable;

import java.util.List;

/**
 * @since 2026-10-31
 */
public class ES6ExportDeclarationImpl extends JSStatementImpl implements ES6ExportDeclaration {
    public ES6ExportDeclarationImpl(ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(JSElementVisitor visitor) {
        visitor.visitJSElement(this);
    }

    @RequiredReadAction
    @Override
    public boolean isExportAll() {
        return findChildByType(JSTokenTypes.ASTERISK) != null;
    }

    @RequiredReadAction
    @Override
    public @Nullable String getNamespaceName() {
        if (!isExportAll()) {
            return null;
        }
        PsiElement identifier = findChildByType(JSTokenTypes.IDENTIFIER);
        return identifier == null ? null : identifier.getText();
    }

    @RequiredReadAction
    @Override
    public List<ES6ExportSpecifier> getSpecifiers() {
        return List.of(findChildrenByClass(ES6ExportSpecifier.class));
    }

    @RequiredReadAction
    @Override
    public @Nullable String getModuleSpecifier() {
        PsiElement string = findString();
        if (string == null) {
            return null;
        }
        String text = string.getText();
        return text.length() < 2 ? null : text.substring(1, text.length() - 1);
    }

    /**
     * The module of {@code from}
     */
    @RequiredReadAction
    @Override
    public PsiReference[] getReferences() {
        PsiElement string = findString();
        if (string == null) {
            return PsiReference.EMPTY_ARRAY;
        }
        JavaScriptModuleReference reference = JavaScriptModuleReference.create(
            this,
            string.getTextRange().shiftLeft(getTextRange().getStartOffset()),
            JavaScriptModuleRequest.Kind.IMPORT
        );
        return reference == null ? PsiReference.EMPTY_ARRAY : new PsiReference[]{reference};
    }

    private @Nullable PsiElement findString() {
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (JavaScriptTokenSets.STRING_LITERALS.contains(child.getNode().getElementType())) {
                return child;
            }
        }
        return null;
    }
}
