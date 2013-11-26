/*
 * @author max
 */
package com.intellij.lang.javascript;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.fileTypes.SingleLazyInstanceSyntaxHighlighterFactory;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import org.jetbrains.annotations.NotNull;

public class JSONLanguageDialect extends JSLanguageDialect {
  public String getFileExtension() {
    return JavaScriptSupportLoader.JSON_FILE_EXTENSION;
  }

  public JSONLanguageDialect() {
    super("JavaScript Object Notation");

    SyntaxHighlighterFactory.LANGUAGE_FACTORY.addExplicitExtension(this, new SingleLazyInstanceSyntaxHighlighterFactory() {
      @NotNull
      protected SyntaxHighlighter createHighlighter() {
        return new JSHighlighter(JavascriptLanguage.DIALECT_OPTION_HOLDER) {
          @NotNull
          public Lexer getHighlightingLexer() {
            return new JSONLexer(super.getHighlightingLexer());
          }
        };
      }
    });
  }

}