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
package com.intellij.lang.javascript.folding;

import com.intellij.codeInsight.folding.CodeFoldingSettings;
import com.intellij.lang.ASTNode;
import com.intellij.lang.folding.FoldingBuilder;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.types.JSFileElementType;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.tree.IElementType;

import java.util.ArrayList;
import java.util.List;

/**
 * @author max
 */
public class JavaScriptFoldingBuilder implements FoldingBuilder {
  public FoldingDescriptor[] buildFoldRegions(ASTNode node, Document document) {
    List<FoldingDescriptor> descriptors = new ArrayList<FoldingDescriptor>();
    appendDescriptors(node, document, descriptors);
    return descriptors.toArray(new FoldingDescriptor[descriptors.size()]);
  }

  private static ASTNode appendDescriptors(final ASTNode node, final Document document, final List<FoldingDescriptor> descriptors) {
    final IElementType type = node.getElementType();
    if (type == JSElementTypes.BLOCK_STATEMENT ||
        type == JSElementTypes.OBJECT_LITERAL_EXPRESSION ||
        type == JSElementTypes.ARRAY_LITERAL_EXPRESSION ||
        type == JSElementTypes.XML_LITERAL_EXPRESSION) {
      TextRange textRange = node.getTextRange();
      if (textRange.getEndOffset() < document.getTextLength() &&
          document.getLineNumber(textRange.getStartOffset()) != document.getLineNumber(textRange.getEndOffset())) {
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
    else if (type == JSElementTypes.IMPORT_STATEMENT) {
      return collapseConsequentNodesOfSpecifiedType(node, descriptors, JSElementTypes.IMPORT_STATEMENT);
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

  private static ASTNode collapseConsequentNodesOfSpecifiedType(final ASTNode node, final List<FoldingDescriptor> descriptors,
                                                                final IElementType endOfLineComment) {
    PsiElement lastEoLComment = node.getPsi();
    PsiElement current = lastEoLComment.getNextSibling();

    while(current != null) {
      if (!(current instanceof PsiWhiteSpace)) break;
      current = current.getNextSibling();
      if (current != null && current.getNode().getElementType() == endOfLineComment) {
        lastEoLComment = current;
        current = current.getNextSibling();
      }
    }

    if (lastEoLComment != node) {
      addDescriptorForRange(node, descriptors, new TextRange(node.getStartOffset(), lastEoLComment.getTextOffset() + lastEoLComment.getTextLength()));
      return lastEoLComment.getNode();
    }
    else {
      return node;
    }
  }

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
    else if (type == JSElementTypes.IMPORT_STATEMENT) {
      return "import ...";
    }
    else if (type == JSElementTypes.BLOCK_STATEMENT ||
             type == JSElementTypes.OBJECT_LITERAL_EXPRESSION ||
             type == JSElementTypes.CLASS ||
             type == JSElementTypes.PACKAGE_STATEMENT) {
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

  public boolean isCollapsedByDefault(ASTNode node) {
    if (node.getTreeParent().getElementType() instanceof JSFileElementType && node.getTreePrev() == null &&
        node.getPsi() instanceof PsiComment) {
      return CodeFoldingSettings.getInstance().COLLAPSE_FILE_HEADER;
    }
    if (node.getElementType() == JSElementTypes.IMPORT_STATEMENT) {
      return CodeFoldingSettings.getInstance().COLLAPSE_IMPORTS;
    }
    return CodeFoldingSettings.getInstance().COLLAPSE_DOC_COMMENTS && node.getElementType() == JSTokenTypes.DOC_COMMENT;
  }
}
