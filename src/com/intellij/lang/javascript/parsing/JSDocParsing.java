package com.intellij.lang.javascript.parsing;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JSDocTokenTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;

/**
 * @author Maxim.Mossienko
 *         Date: Aug 5, 2008
 *         Time: 3:55:54 PM
 */
public class JSDocParsing {
  public static void parseJSDoc(final PsiBuilder builder) {
    final PsiBuilder.Marker root = builder.mark();

    while(!builder.eof()) {
      final IElementType tokenType = builder.getTokenType();

      if (tokenType == JSDocTokenTypes.DOC_TAG_NAME) {
        if(parseDocTag(builder)) continue;
      }

      builder.advanceLexer();
    }

    root.done(JSTokenTypes.DOC_COMMENT);
  }

  private static boolean parseDocTag(final PsiBuilder builder) {
    assert builder.getTokenType() == JSDocTokenTypes.DOC_TAG_NAME;
    final PsiBuilder.Marker docTagMarker = builder.mark();

    try {
      final @NonNls String tagName = builder.getTokenText();

      builder.advanceLexer();
      if ("@param".equals(tagName)) {

        if (isInvalidTokenType(builder)) {
          builder.error(JSBundle.message("javascript.parser.message.expected.doc.tag.name"));
          return false;
        }

        String currentText = builder.getTokenText();
        if (currentText != null && currentText.startsWith("{")) {
          createDocTagValue(builder);
          builder.getTokenType();
        }
        builder.advanceLexer();
        currentText = builder.getTokenText();
        if (currentText != null) {
          if(currentText.equals(":")) builder.advanceLexer();
          else if (!currentText.startsWith("{")) return true;
        }

        if (isInvalidTokenType(builder)) return true;
      } else {
        final boolean hasDocTagValue = isToCreateDocTagValue(tagName);
        if (!hasDocTagValue) return true;

        if (isInvalidTokenType(builder)) {
          builder.error(JSBundle.message("javascript.parser.message.expected.doc.tag.value"));
          return false;
        }
      }

      createDocTagValue(builder);
    } finally {
      docTagMarker.done(JSDocTokenTypes.DOC_TAG);
    }

    return true;
  }

  private static void createDocTagValue(final PsiBuilder builder) {
    PsiBuilder.Marker marker = builder.mark();
    builder.advanceLexer();
    marker.done(JSDocTokenTypes.DOC_TAG_VALUE);
  }

  private static boolean isInvalidTokenType(final PsiBuilder builder) {
    final IElementType tokenType = builder.getTokenType();
    return builder.eof() || tokenType == JSDocTokenTypes.DOC_COMMENT_LEADING_ASTERISK || tokenType == JSDocTokenTypes.DOC_COMMENT_END;
  }

  private static boolean isToCreateDocTagValue(final @NonNls String tokenText) {
    return tokenText.equals("@see") ||
           tokenText.equals("@class") ||
           tokenText.equals("@member") ||
           tokenText.equals("@requires") ||
           tokenText.equals("@type") ||
           tokenText.equals("@copy") ||
           tokenText.equals("@extends") ||
           tokenText.equals("@base") ||
           tokenText.equals("@base") ||
           tokenText.equals("@returns");
  }
}
