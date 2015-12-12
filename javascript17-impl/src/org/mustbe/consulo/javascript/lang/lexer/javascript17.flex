package org.mustbe.consulo.javascript.lang.lexer;

import com.intellij.lexer.LexerBase;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.javascript.JSTokenTypes;

%%


%public
%class JavaScript17Lexer
%extends LexerBase
%unicode
%function advanceImpl
%type IElementType
%eof{  return;
%eof}

DIGIT=[0-9]
OCTAL_DIGIT=[0-7]
HEX_DIGIT=[0-9A-Fa-f]
WHITE_SPACE_CHAR=[\ \n\r\t\f]

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
GROUP = "[" [^\]]* "]"

REGEXP_LITERAL="/"([^\*\\/\r\n]|{ESCAPE_SEQUENCE}|{GROUP})([^\\/\r\n]|{ESCAPE_SEQUENCE}|{GROUP})*("/"[gimx]*)?
DIGIT=[0-9]

%state COMMENT
%state LAST_STATE

%%

<YYINITIAL, COMMENT> {WHITE_SPACE_CHAR}+   { return JSTokenTypes.WHITE_SPACE; }

<COMMENT> [^] { yybegin(YYINITIAL); yypushback(1); }

<YYINITIAL> {C_STYLE_COMMENT}     { return JSTokenTypes.C_STYLE_COMMENT; }
<YYINITIAL> {END_OF_LINE_COMMENT} { return JSTokenTypes.END_OF_LINE_COMMENT; }
<YYINITIAL> {DOC_COMMENT}         { return JSTokenTypes.DOC_COMMENT; }

<YYINITIAL> {INTEGER_LITERAL}     { yybegin(YYINITIAL); return JSTokenTypes.NUMERIC_LITERAL; }
<YYINITIAL> {FLOAT_LITERAL}       { yybegin(YYINITIAL); return JSTokenTypes.NUMERIC_LITERAL; }

<YYINITIAL> {QUOTED_LITERAL}      { return JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL; }

<YYINITIAL> {DOUBLE_QUOTED_LITERAL}      { yybegin(YYINITIAL); return JSTokenTypes.STRING_LITERAL; }

<YYINITIAL> "true"                { yybegin(YYINITIAL); return JSTokenTypes.TRUE_KEYWORD; }
<YYINITIAL> "false"               { yybegin(YYINITIAL); return JSTokenTypes.FALSE_KEYWORD; }
<YYINITIAL> "null"                { yybegin(YYINITIAL); return JSTokenTypes.NULL_KEYWORD; }
<YYINITIAL> "undefined"           { yybegin(YYINITIAL); return JSTokenTypes.UNDEFINED_KEYWORD; }

<YYINITIAL> "break"               { yybegin(YYINITIAL); return JSTokenTypes.BREAK_KEYWORD; }
<YYINITIAL> "case"                { yybegin(YYINITIAL); return JSTokenTypes.CASE_KEYWORD; }
<YYINITIAL> "catch"               { yybegin(YYINITIAL); return JSTokenTypes.CATCH_KEYWORD; }
<YYINITIAL> "const"               { yybegin(YYINITIAL); return JSTokenTypes.CONST_KEYWORD; }
<YYINITIAL> "continue"            { yybegin(YYINITIAL); return JSTokenTypes.CONTINUE_KEYWORD; }
<YYINITIAL> "default"             { yybegin(YYINITIAL); return JSTokenTypes.DEFAULT_KEYWORD; }
<YYINITIAL> "delete"              { yybegin(YYINITIAL); return JSTokenTypes.DELETE_KEYWORD; }
<YYINITIAL> "do"                  { yybegin(YYINITIAL); return JSTokenTypes.DO_KEYWORD; }
<YYINITIAL> "else"                { yybegin(YYINITIAL); return JSTokenTypes.ELSE_KEYWORD; }
<YYINITIAL> "finally"             { yybegin(YYINITIAL); return JSTokenTypes.FINALLY_KEYWORD; }
<YYINITIAL> "for"                 { yybegin(YYINITIAL); return JSTokenTypes.FOR_KEYWORD; }
<YYINITIAL> "function"            { yybegin(YYINITIAL); return JSTokenTypes.FUNCTION_KEYWORD; }
<YYINITIAL> "if"                  { yybegin(YYINITIAL); return JSTokenTypes.IF_KEYWORD; }
<YYINITIAL> "in"                  { yybegin(YYINITIAL); return JSTokenTypes.IN_KEYWORD; }
<YYINITIAL> "instanceof"          { yybegin(YYINITIAL); return JSTokenTypes.INSTANCEOF_KEYWORD; }
<YYINITIAL> "new"                 { yybegin(YYINITIAL); return JSTokenTypes.NEW_KEYWORD; }
<YYINITIAL> "return"              { yybegin(YYINITIAL); return JSTokenTypes.RETURN_KEYWORD; }
<YYINITIAL> "switch"              { yybegin(YYINITIAL); return JSTokenTypes.SWITCH_KEYWORD; }
<YYINITIAL> "this"                { yybegin(YYINITIAL); return JSTokenTypes.THIS_KEYWORD; }
<YYINITIAL> "throw"               { yybegin(YYINITIAL); return JSTokenTypes.THROW_KEYWORD; }
<YYINITIAL> "try"                 { yybegin(YYINITIAL); return JSTokenTypes.TRY_KEYWORD; }
<YYINITIAL> "typeof"              { yybegin(YYINITIAL); return JSTokenTypes.TYPEOF_KEYWORD; }
<YYINITIAL> "var"                 { yybegin(YYINITIAL); return JSTokenTypes.VAR_KEYWORD; }
<YYINITIAL> "void"                { yybegin(YYINITIAL); return JSTokenTypes.VOID_KEYWORD; }
<YYINITIAL> "while"               { yybegin(YYINITIAL); return JSTokenTypes.WHILE_KEYWORD; }
<YYINITIAL> "with"                { yybegin(YYINITIAL); return JSTokenTypes.WITH_KEYWORD; }
<YYINITIAL> "get"                 { yybegin(YYINITIAL); return JSTokenTypes.GET_KEYWORD; }
<YYINITIAL> "set"                 { yybegin(YYINITIAL); return JSTokenTypes.SET_KEYWORD; }
<YYINITIAL> "yield"               { yybegin(YYINITIAL); return JSTokenTypes.YIELD_KEYWORD; }
<YYINITIAL> "let"                 { yybegin(YYINITIAL); return JSTokenTypes.LET_KEYWORD; }


<YYINITIAL> {IDENTIFIER}          { yybegin(YYINITIAL);       return JSTokenTypes.IDENTIFIER; }


<YYINITIAL> "*"       { return JSTokenTypes.MULT; }

<YYINITIAL> "."                   { yybegin(YYINITIAL); return JSTokenTypes.DOT; }


<YYINITIAL> "==="                 { yybegin(YYINITIAL); return JSTokenTypes.EQEQEQ; }
<YYINITIAL> "!=="                 { yybegin(YYINITIAL); return JSTokenTypes.NEQEQ; }

<YYINITIAL> "++"                  { return JSTokenTypes.PLUSPLUS; }
<YYINITIAL> "--"                  { return JSTokenTypes.MINUSMINUS; }

<YYINITIAL> "=="                  { yybegin(YYINITIAL); return JSTokenTypes.EQEQ; }
<YYINITIAL> "!="                  { yybegin(YYINITIAL); return JSTokenTypes.NE; }
<YYINITIAL> "<"       { yybegin(YYINITIAL); return JSTokenTypes.LT; }
<YYINITIAL> ">"                   { yybegin(YYINITIAL); return JSTokenTypes.GT; }
<YYINITIAL> "<="      { yybegin(YYINITIAL); return JSTokenTypes.LE; }
<YYINITIAL> ">="                  { yybegin(YYINITIAL); return JSTokenTypes.GE; }
<YYINITIAL> "<<"      { yybegin(YYINITIAL); return JSTokenTypes.LTLT; }
<YYINITIAL> ">>"                  { yybegin(YYINITIAL); return JSTokenTypes.GTGT; }
<YYINITIAL> ">>>"                 { yybegin(YYINITIAL); return JSTokenTypes.GTGTGT; }

<YYINITIAL> "&"                   { yybegin(YYINITIAL); return JSTokenTypes.AND; }
<YYINITIAL> "&&"                  { yybegin(YYINITIAL); return JSTokenTypes.ANDAND; }
<YYINITIAL> "|"                   { yybegin(YYINITIAL); return JSTokenTypes.OR; }
<YYINITIAL> "||"                  { yybegin(YYINITIAL); return JSTokenTypes.OROR; }

<YYINITIAL> "+="                  { yybegin(YYINITIAL); return JSTokenTypes.PLUSEQ; }
<YYINITIAL> "-="                  { yybegin(YYINITIAL); return JSTokenTypes.MINUSEQ; }
<YYINITIAL> "*="                  { yybegin(YYINITIAL); return JSTokenTypes.MULTEQ; }
<YYINITIAL> "/="                  { yybegin(YYINITIAL); return JSTokenTypes.DIVEQ; }
<YYINITIAL> "&="                  { yybegin(YYINITIAL); return JSTokenTypes.ANDEQ; }
<YYINITIAL> "|="                  { yybegin(YYINITIAL); return JSTokenTypes.OREQ; }
<YYINITIAL> "^="                  { yybegin(YYINITIAL); return JSTokenTypes.XOREQ; }
<YYINITIAL> "%="                  { yybegin(YYINITIAL); return JSTokenTypes.PERCEQ; }
<YYINITIAL> "<<="                 { yybegin(YYINITIAL); return JSTokenTypes.LTLTEQ; }
<YYINITIAL> ">>="                 { yybegin(YYINITIAL); return JSTokenTypes.GTGTEQ; }
<YYINITIAL> ">>>="                { yybegin(YYINITIAL); return JSTokenTypes.GTGTGTEQ; }

<YYINITIAL> "("                   { yybegin(YYINITIAL); return JSTokenTypes.LPAR; }
<YYINITIAL> ")"                   { yybegin(YYINITIAL);       return JSTokenTypes.RPAR; }
<YYINITIAL> "{"                   { yybegin(YYINITIAL); return JSTokenTypes.LBRACE; }
<YYINITIAL> "}"                   { yybegin(YYINITIAL); return JSTokenTypes.RBRACE; }
<YYINITIAL> "["                   { yybegin(YYINITIAL); return JSTokenTypes.LBRACKET; }
<YYINITIAL> "]"                   { yybegin(YYINITIAL);       return JSTokenTypes.RBRACKET; }
<YYINITIAL> ";"                   { yybegin(YYINITIAL); return JSTokenTypes.SEMICOLON; }
<YYINITIAL> ","                   { yybegin(YYINITIAL); return JSTokenTypes.COMMA; }

<YYINITIAL> "="                   { yybegin(YYINITIAL); return JSTokenTypes.EQ; }
<YYINITIAL> "!"                   { yybegin(YYINITIAL); return JSTokenTypes.EXCL; }
<YYINITIAL> "~"                   { yybegin(YYINITIAL); return JSTokenTypes.TILDE; }
<YYINITIAL> "?"                   { yybegin(YYINITIAL); return JSTokenTypes.QUEST; }
<YYINITIAL> ":"                   { yybegin(YYINITIAL); return JSTokenTypes.COLON; }
<YYINITIAL> "+"                   { yybegin(YYINITIAL); return JSTokenTypes.PLUS; }
<YYINITIAL> "-"                   { yybegin(YYINITIAL); return JSTokenTypes.MINUS; }
<YYINITIAL> "/"                   { yybegin(YYINITIAL); return JSTokenTypes.DIV; }
<YYINITIAL> "^"                   { yybegin(YYINITIAL); return JSTokenTypes.XOR; }
<YYINITIAL> "%"                   { yybegin(YYINITIAL); return JSTokenTypes.PERC; }

<YYINITIAL> {REGEXP_LITERAL} { return JSTokenTypes.REGEXP_LITERAL; }

<YYINITIAL> . {return JSTokenTypes.BAD_CHARACTER; }