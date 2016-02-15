package org.mustbe.consulo.json.lang.lexer;

import com.intellij.lexer.LexerBase;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.javascript.JSTokenTypes;

%%

%public
%class JsonLexer
%extends LexerBase
%unicode
%function advanceImpl
%type IElementType
%eof{  return;
%eof}

DIGIT=[0-9]
OCTAL_DIGIT=[0-7]
HEX_DIGIT=[0-9A-Fa-f]
WHITE_SPACE_CHAR=[\ \n\r\t\f]+

IDENTIFIER=[:jletter:] [:jletterdigit:]*

C_STYLE_COMMENT=("/*"[^"*"]{COMMENT_TAIL})|"/*"
DOC_COMMENT="/*""*"+("/"|([^"/""*"]{COMMENT_TAIL}))?
COMMENT_TAIL=([^"*"]*("*"+[^"*""/"])?)*("*"+"/")?
END_OF_LINE_COMMENT="/""/"[^\r\n]*

INTEGER_LITERAL={DECIMAL_INTEGER_LITERAL}|{HEX_INTEGER_LITERAL}
DECIMAL_INTEGER_LITERAL=(0|([1-9]({DIGIT})*))
HEX_INTEGER_LITERAL=0[Xx]({HEX_DIGIT})*

FLOAT_LITERAL=({FLOATING_POINT_LITERAL1})|({FLOATING_POINT_LITERAL2})|({FLOATING_POINT_LITERAL3})|({FLOATING_POINT_LITERAL4})
FLOATING_POINT_LITERAL1=({DIGIT})+"."({DIGIT})*({EXPONENT_PART})?
FLOATING_POINT_LITERAL2="."({DIGIT})+({EXPONENT_PART})?
FLOATING_POINT_LITERAL3=({DIGIT})+({EXPONENT_PART})
FLOATING_POINT_LITERAL4=({DIGIT})+
EXPONENT_PART=[Ee]["+""-"]?({DIGIT})*

CRLF= [\ \t \f]* (\n | \r | \r\n)
QUOTED_LITERAL="'"([^\\\'\r\n]|{ESCAPE_SEQUENCE}|\\{CRLF})*("'"|\\)?
DOUBLE_QUOTED_LITERAL=\"([^\\\"\r\n]|{ESCAPE_SEQUENCE}|\\{CRLF})*(\"|\\)?
ESCAPE_SEQUENCE=\\[^\r\n]

DIGIT=[0-9]

%%

<YYINITIAL> {WHITE_SPACE_CHAR}   { return JSTokenTypes.WHITE_SPACE; }

<YYINITIAL> {C_STYLE_COMMENT}     { return JSTokenTypes.C_STYLE_COMMENT; }
<YYINITIAL> {END_OF_LINE_COMMENT} { return JSTokenTypes.END_OF_LINE_COMMENT; }

<YYINITIAL> {INTEGER_LITERAL}     { yybegin(YYINITIAL); return JSTokenTypes.NUMERIC_LITERAL; }
<YYINITIAL> {FLOAT_LITERAL}       { yybegin(YYINITIAL); return JSTokenTypes.NUMERIC_LITERAL; }

<YYINITIAL> {QUOTED_LITERAL}      { return JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL; }

<YYINITIAL> {DOUBLE_QUOTED_LITERAL}      { yybegin(YYINITIAL); return JSTokenTypes.STRING_LITERAL; }

<YYINITIAL> "true"                { yybegin(YYINITIAL); return JSTokenTypes.TRUE_KEYWORD; }
<YYINITIAL> "false"               { yybegin(YYINITIAL); return JSTokenTypes.FALSE_KEYWORD; }
<YYINITIAL> "null"                { yybegin(YYINITIAL); return JSTokenTypes.NULL_KEYWORD; }

<YYINITIAL> {IDENTIFIER}          { yybegin(YYINITIAL);       return JSTokenTypes.IDENTIFIER; }

<YYINITIAL> "{"                   { yybegin(YYINITIAL); return JSTokenTypes.LBRACE; }
<YYINITIAL> "}"                   { yybegin(YYINITIAL); return JSTokenTypes.RBRACE; }
<YYINITIAL> "["                   { yybegin(YYINITIAL); return JSTokenTypes.LBRACKET; }
<YYINITIAL> "]"                   { yybegin(YYINITIAL); return JSTokenTypes.RBRACKET; }
<YYINITIAL> ","                   { yybegin(YYINITIAL); return JSTokenTypes.COMMA; }
<YYINITIAL> ":"                   { yybegin(YYINITIAL); return JSTokenTypes.COLON; }
<YYINITIAL> "-"                   { yybegin(YYINITIAL); return JSTokenTypes.MINUS; }


<YYINITIAL> [^] {return JSTokenTypes.BAD_CHARACTER; }