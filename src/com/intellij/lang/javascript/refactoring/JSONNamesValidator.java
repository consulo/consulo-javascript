/*
 * @author max
 */
package com.intellij.lang.javascript.refactoring;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.javascript.*;

public class JSONNamesValidator extends JSNamesValidator {
  public JSONNamesValidator() {
    super(JavascriptLanguage.DIALECT_OPTION_HOLDER);
  }

  protected Lexer createLexer(final DialectOptionHolder optionHolder) {
    return new JSONLexer(new JavaScriptLexer(optionHolder));
  }

  public synchronized boolean isIdentifier(String name, final Project project) {
    if (!StringUtil.startsWithChar(name,'\'') && !StringUtil.startsWithChar(name,'\"')) {
      name = "\"" + name;
    }

    if (!StringUtil.endsWithChar(name,'"') && !StringUtil.endsWithChar(name,'\"')) {
      name += "\"";
    }

    myLexer.start(name, 0, name.length(), 0);
    IElementType type = myLexer.getTokenType();

    return myLexer.getTokenEnd() == name.length() && (type == JSTokenTypes.STRING_LITERAL || type == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL);
  }

}