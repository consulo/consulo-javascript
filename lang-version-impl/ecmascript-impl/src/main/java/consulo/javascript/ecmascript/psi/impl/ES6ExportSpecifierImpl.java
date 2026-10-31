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
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.ecmascript.psi.ES6ExportSpecifier;
import consulo.language.ast.ASTNode;
import consulo.language.ast.TokenSet;
import consulo.language.psi.PsiElement;
import org.jspecify.annotations.Nullable;

/**
 * @since 2026-10-31
 */
public class ES6ExportSpecifierImpl extends JSElementImpl implements ES6ExportSpecifier {
    private static final TokenSet NAMES = TokenSet.create(JSTokenTypes.IDENTIFIER, JSTokenTypes.DEFAULT_KEYWORD);

    public ES6ExportSpecifierImpl(ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(JSElementVisitor visitor) {
        visitor.visitJSElement(this);
    }

    @RequiredReadAction
    @Override
    public @Nullable String getLocalName() {
        ASTNode first = getNode().findChildByType(NAMES);
        return first == null ? null : first.getText();
    }

    @RequiredReadAction
    @Override
    public @Nullable String getExportedName() {
        ASTNode[] names = getNode().getChildren(NAMES);
        return names.length == 0 ? null : names[names.length - 1].getText();
    }
}
