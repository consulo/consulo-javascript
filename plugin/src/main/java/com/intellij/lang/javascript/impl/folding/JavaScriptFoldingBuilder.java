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

package com.intellij.lang.javascript.impl.folding;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.folding.CodeFoldingSettings;
import consulo.language.ast.ASTNode;
import consulo.language.editor.folding.FoldingDescriptor;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.types.JSFileElementType;
import consulo.document.Document;
import consulo.document.util.TextRange;
import consulo.language.psi.PsiComment;
import consulo.language.psi.PsiElement;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiUtilCore;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.psi.JavaScriptImportStatementBase;
import consulo.language.editor.folding.FoldingBuilder;
import consulo.language.psi.PsiWhiteSpace;

import jakarta.annotation.Nonnull;

import java.util.ArrayList;
import java.util.List;

/**
 * @author max
 */
@ExtensionImpl
public class JavaScriptFoldingBuilder implements FoldingBuilder {
    @RequiredReadAction
    @Nonnull
    @Override
    public FoldingDescriptor[] buildFoldRegions(@Nonnull ASTNode node, @Nonnull Document document) {
        List<FoldingDescriptor> descriptors = new ArrayList<>();
        appendDescriptors(node, document, descriptors);
        return descriptors.toArray(new FoldingDescriptor[descriptors.size()]);
    }

    @RequiredReadAction
    private static ASTNode appendDescriptors(final ASTNode node, final Document document, final List<FoldingDescriptor> descriptors) {
        final IElementType type = node.getElementType();
        if (type == JSElementTypes.BLOCK_STATEMENT
            || type == JSElementTypes.OBJECT_LITERAL_EXPRESSION
            || type == JSElementTypes.ARRAY_LITERAL_EXPRESSION
            || type == JSElementTypes.XML_LITERAL_EXPRESSION) {
            TextRange textRange = node.getTextRange();
            if (textRange.getEndOffset() < document.getTextLength()
                && document.getLineNumber(textRange.getStartOffset()) != document.getLineNumber(textRange.getEndOffset())) {
                addDescriptorForNode(node, descriptors);
            }
        }
        else if (type == JSTokenTypes.DOC_COMMENT) {
            addDescriptorForNode(node, descriptors);
        }
        else if (type == JSTokenTypes.C_STYLE_COMMENT) {
            addDescriptorForNode(node, descriptors);
        }
        else if (type == JSTokenTypes.END_OF_LINE_COMMENT) {
            return collapseConsequentNodesOfSpecifiedType(node, descriptors, JSTokenTypes.END_OF_LINE_COMMENT);
        }
        else if (node.getPsi() instanceof JavaScriptImportStatementBase) {
            return collapseConsequentNodesOfSpecifiedType(node, descriptors, PsiUtilCore.getElementType(node));
        }
        else if (type == JSElementTypes.CLASS || type == JSElementTypes.PACKAGE_STATEMENT) {
            ASTNode lbrace = node.findChildByType(JSTokenTypes.LBRACE);

            if (lbrace != null) {
                addDescriptorForRange(node, descriptors, new TextRange(lbrace.getStartOffset(), node.getTextRange().getEndOffset()));
            }
        }

        if (type instanceof JSFileElementType) {
            // expand chameleon
            node.getPsi().getFirstChild();
        }

        ASTNode child = node.getFirstChildNode();
        while (child != null) {
            child = appendDescriptors(child, document, descriptors).getTreeNext();
        }

        return node;
    }

    private static void addDescriptorForRange(ASTNode node, List<FoldingDescriptor> descriptors, TextRange range) {
        if (range.getLength() > 1) {
            descriptors.add(new FoldingDescriptor(node, range));
        }
    }

    private static void addDescriptorForNode(ASTNode node, List<FoldingDescriptor> descriptors) {
        TextRange range = node.getTextRange();
        addDescriptorForRange(node, descriptors, range);
    }

    @RequiredReadAction
    private static ASTNode collapseConsequentNodesOfSpecifiedType(
        final ASTNode node,
        final List<FoldingDescriptor> descriptors,
        final IElementType endOfLineComment
    ) {
        PsiElement lastEoLComment = node.getPsi();
        PsiElement current = lastEoLComment.getNextSibling();

        while (current != null) {
            if (!(current instanceof PsiWhiteSpace)) {
                break;
            }
            current = current.getNextSibling();
            if (current != null && current.getNode().getElementType() == endOfLineComment) {
                lastEoLComment = current;
                current = current.getNextSibling();
            }
        }

        if (lastEoLComment != node) {
            addDescriptorForRange(
                node,
                descriptors,
                new TextRange(node.getStartOffset(), lastEoLComment.getTextOffset() + lastEoLComment.getTextLength())
            );
            return lastEoLComment.getNode();
        }
        else {
            return node;
        }
    }

    @RequiredReadAction
    @Override
    public String getPlaceholderText(ASTNode node) {
        final IElementType type = node.getElementType();
        if (type == JSTokenTypes.DOC_COMMENT) {
            return "/**...*/";
        }
        else if (type == JSTokenTypes.C_STYLE_COMMENT) {
            return "/*...*/";
        }
        else if (type == JSTokenTypes.END_OF_LINE_COMMENT) {
            return "//...";
        }
        else if (node.getPsi() instanceof JavaScriptImportStatementBase) {
            return "import ...";
        }
        else if (type == JSElementTypes.BLOCK_STATEMENT
            || type == JSElementTypes.OBJECT_LITERAL_EXPRESSION
            || type == JSElementTypes.CLASS
            || type == JSElementTypes.PACKAGE_STATEMENT) {
            return "{...}";
        }
        else if (type == JSElementTypes.ARRAY_LITERAL_EXPRESSION) {
            return "[...]";
        }
        else if (type == JSElementTypes.XML_LITERAL_EXPRESSION) {
            return "<xml/>";
        }
        return null;
    }

    @RequiredReadAction
    @Override
    public boolean isCollapsedByDefault(ASTNode node) {
        if (node.getTreeParent().getElementType() instanceof JSFileElementType
            && node.getTreePrev() == null
            && node.getPsi() instanceof PsiComment) {
            return CodeFoldingSettings.getInstance().COLLAPSE_FILE_HEADER;
        }
        if (node.getPsi() instanceof JavaScriptImportStatementBase) {
            return CodeFoldingSettings.getInstance().COLLAPSE_IMPORTS;
        }
        return CodeFoldingSettings.getInstance().COLLAPSE_DOC_COMMENTS && node.getElementType() == JSTokenTypes.DOC_COMMENT;
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
