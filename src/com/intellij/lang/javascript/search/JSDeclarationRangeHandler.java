package com.intellij.lang.javascript.search;

import com.intellij.codeInsight.hint.DeclarationRangeHandler;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.ASTNode;
import org.jetbrains.annotations.NotNull;

/**
 * @author Maxim.Mossienko
*         Date: Apr 28, 2008
*         Time: 8:36:19 PM
*/
public class JSDeclarationRangeHandler implements DeclarationRangeHandler {
  @NotNull
    public TextRange getDeclarationRange(@NotNull PsiElement container) {
    if (container instanceof JSNamedElementProxy) {
      container = ((JSNamedElementProxy)container).getElement();
    }

    JSNamedElement namedElement = (JSNamedElement)container;

    final TextRange textRange = namedElement.getTextRange();
    final ASTNode nameIdentifier = namedElement.findNameIdentifier();
    final TextRange nameIdentifierRange = nameIdentifier != null ? nameIdentifier.getTextRange() : null;
    int startOffset = nameIdentifierRange != null ? nameIdentifierRange.getStartOffset() : textRange.getStartOffset();
    int endOffset = nameIdentifierRange != null ? nameIdentifierRange.getEndOffset() : textRange.getEndOffset();

    return new TextRange(startOffset, endOffset);
  }
}
