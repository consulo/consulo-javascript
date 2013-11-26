package com.intellij.lang.javascript;
import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;

%%

%{
  private boolean highlightMode;
  public _JSDocLexer(boolean _highlightMode) {
    this((java.io.Reader)null);
    highlightMode = _highlightMode;
  }
%}

%class _JSDocLexer
%implements FlexLexer
%unicode
%function advance
%type IElementType
%eof{ return;
%eof}

%state COMMENT_DATA_START
%state FOUND_ASTERISK

WHITE_DOC_SPACE_CHAR=[\ \t\f\n\r]
WHITE_DOC_SPACE_NO_LR=[\ \t\f]
DIGIT=[0-9]
ALPHA=[:jletter:]
IDENTIFIER={ALPHA}({ALPHA}|{DIGIT}|[":.-"])*

%%

<YYINITIAL> "/**/" { return JSDocTokenTypes.DOC_COMMENT_END; }
<YYINITIAL> "/**" { yybegin(COMMENT_DATA_START); return JSDocTokenTypes.DOC_COMMENT_START; }

<COMMENT_DATA_START> [\r\n]+ {WHITE_DOC_SPACE_NO_LR}* "*" {
  yybegin(FOUND_ASTERISK);
  yypushback(yylength());
}

<COMMENT_DATA_START> {WHITE_DOC_SPACE_CHAR}+ {
  return highlightMode ? JSDocTokenTypes.DOC_SPACE:JSDocTokenTypes.DOC_REAL_WHITESPACE;
}

<COMMENT_DATA_START> "*/" {  return JSDocTokenTypes.DOC_COMMENT_END;  }

<FOUND_ASTERISK> {WHITE_DOC_SPACE_CHAR}+ {WHITE_DOC_SPACE_NO_LR}* {
  return highlightMode ? JSDocTokenTypes.DOC_SPACE:JSDocTokenTypes.DOC_REAL_WHITESPACE;
}

<FOUND_ASTERISK> "*" { yybegin(COMMENT_DATA_START); return JSDocTokenTypes.DOC_COMMENT_LEADING_ASTERISK; }
<COMMENT_DATA_START> "@" {IDENTIFIER} { return JSDocTokenTypes.DOC_TAG_NAME; }
<COMMENT_DATA_START> [^\ \t\f\n\r]+ { return JSDocTokenTypes.DOC_COMMENT_DATA; }

"*"+"/" { return JSDocTokenTypes.DOC_COMMENT_END; }
[^] { return JSDocTokenTypes.DOC_BAD_CHARACTER; }
