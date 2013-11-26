/*
 * @author max
 */
package com.intellij.lang.javascript;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.parsing.JSParser;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.psi.tree.IFileElementType;
import org.jetbrains.annotations.NotNull;

public class GwtParserDefinition extends JavascriptParserDefinition {
  @NotNull
  public Lexer createLexer(final Project project) {
    return new JavaScriptParsingLexer(GwtLanguageDialect.DIALECT_OPTION_HOLDER);
  }

  public IFileElementType getFileNodeType() {
    return JSElementTypes.GWT_FILE;
  }


  @NotNull
  public PsiParser createParser(final Project project) {
    return new JSParser(JavaScriptSupportLoader.GWT_DIALECT);
  }
}