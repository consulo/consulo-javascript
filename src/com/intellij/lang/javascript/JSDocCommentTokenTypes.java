package com.intellij.lang.javascript;

import com.intellij.lexer.DocCommentTokenTypes;
import com.intellij.psi.tree.IElementType;

/**
 * @author yole
 */
public class JSDocCommentTokenTypes implements DocCommentTokenTypes {
  public IElementType commentStart() {
    return JSDocTokenTypes.DOC_COMMENT_START;
  }

  public IElementType commentEnd() {
    return JSDocTokenTypes.DOC_COMMENT_END;
  }

  public IElementType commentData() {
    return JSDocTokenTypes.DOC_COMMENT_DATA;
  }

  public IElementType space() {
    return JSDocTokenTypes.DOC_SPACE;
  }

  public IElementType tagValueToken() {
    return JSDocTokenTypes.DOC_TAG_VALUE; // TODO[yole] is this correct?
  }

  public IElementType tagValueLParen() {
    return JSDocTokenTypes.DOC_TAG_VALUE_LPAREN;
  }

  public IElementType tagValueRParen() {
    return JSDocTokenTypes.DOC_TAG_VALUE_RPAREN;
  }

  public IElementType tagValueSharp() {
    return JSDocTokenTypes.DOC_TAG_VALUE_SHARP;
  }

  public IElementType tagValueComma() {
    return JSDocTokenTypes.DOC_TAG_VALUE_COMMA;
  }

  public IElementType tagName() {
    return JSDocTokenTypes.DOC_TAG_NAME;
  }

  public IElementType tagValueLT() {
    return JSDocTokenTypes.DOC_TAG_VALUE_LT;
  }

  public IElementType tagValueGT() {
    return JSDocTokenTypes.DOC_TAG_VALUE_GT;
  }

  public IElementType inlineTagStart() {
    return JSDocTokenTypes.INLINE_TAG_START;
  }

  public IElementType inlineTagEnd() {
    return JSDocTokenTypes.INLINE_TAG_END;
  }

  public IElementType badCharacter() {
    return JSDocTokenTypes.DOC_BAD_CHARACTER;
  }

  public IElementType commentLeadingAsterisks() {
    return JSDocTokenTypes.DOC_COMMENT_LEADING_ASTERISK;
  }
}
