package consulo.javascript.javascript17.lang.lexer;

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
GROUP = "[" [^\]]* "]"

REGEXP_LITERAL="/"([^\*\\/\r\n]|{ESCAPE_SEQUENCE}|{GROUP})([^\\/\r\n]|{ESCAPE_SEQUENCE}|{GROUP})*("/"[gimx]*)?
DIGIT=[0-9]

%state DIV_OR_GT

%%

<YYINITIAL, DIV_OR_GT> {WHITE_SPACE_CHAR}   { return JSTokenTypes.WHITE_SPACE; }

<YYINITIAL,DIV_OR_GT> {C_STYLE_COMMENT}     { return JSTokenTypes.C_STYLE_COMMENT; }
<YYINITIAL,DIV_OR_GT> {END_OF_LINE_COMMENT} { return JSTokenTypes.END_OF_LINE_COMMENT; }
<YYINITIAL,DIV_OR_GT> {DOC_COMMENT}         { return JSTokenTypes.DOC_COMMENT; }

<YYINITIAL,DIV_OR_GT> {INTEGER_LITERAL}     { yybegin(DIV_OR_GT); return JSTokenTypes.NUMERIC_LITERAL; }
<YYINITIAL,DIV_OR_GT> {FLOAT_LITERAL}       { yybegin(DIV_OR_GT); return JSTokenTypes.NUMERIC_LITERAL; }

<YYINITIAL,DIV_OR_GT> {QUOTED_LITERAL}      { return  JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL;}

<YYINITIAL,DIV_OR_GT> {DOUBLE_QUOTED_LITERAL}      { yybegin(YYINITIAL); return JSTokenTypes.STRING_LITERAL; }

<YYINITIAL,DIV_OR_GT> "true"                { yybegin(DIV_OR_GT); return JSTokenTypes.TRUE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "false"               { yybegin(DIV_OR_GT); return JSTokenTypes.FALSE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "null"                { yybegin(DIV_OR_GT); return JSTokenTypes.NULL_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "undefined"           { yybegin(DIV_OR_GT); return JSTokenTypes.UNDEFINED_KEYWORD; }

<YYINITIAL,DIV_OR_GT> "break"               { yybegin(YYINITIAL); return JSTokenTypes.BREAK_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "case"                { yybegin(YYINITIAL); return JSTokenTypes.CASE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "catch"               { yybegin(YYINITIAL); return JSTokenTypes.CATCH_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "const"               { yybegin(YYINITIAL); return JSTokenTypes.CONST_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "continue"            { yybegin(YYINITIAL); return JSTokenTypes.CONTINUE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "default"             { yybegin(YYINITIAL); return JSTokenTypes.DEFAULT_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "delete"              { yybegin(YYINITIAL); return JSTokenTypes.DELETE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "do"                  { yybegin(YYINITIAL); return JSTokenTypes.DO_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "else"                { yybegin(YYINITIAL); return JSTokenTypes.ELSE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "finally"             { yybegin(YYINITIAL); return JSTokenTypes.FINALLY_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "for"                 { yybegin(YYINITIAL); return JSTokenTypes.FOR_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "function"            { yybegin(YYINITIAL); return JSTokenTypes.FUNCTION_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "if"                  { yybegin(YYINITIAL); return JSTokenTypes.IF_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "in"                  { yybegin(YYINITIAL); return JSTokenTypes.IN_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "instanceof"          { yybegin(YYINITIAL); return JSTokenTypes.INSTANCEOF_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "new"                 { yybegin(YYINITIAL); return JSTokenTypes.NEW_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "return"              { yybegin(YYINITIAL); return JSTokenTypes.RETURN_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "switch"              { yybegin(YYINITIAL); return JSTokenTypes.SWITCH_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "this"                { yybegin(DIV_OR_GT); return JSTokenTypes.THIS_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "throw"               { yybegin(YYINITIAL); return JSTokenTypes.THROW_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "try"                 { yybegin(YYINITIAL); return JSTokenTypes.TRY_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "typeof"              { yybegin(YYINITIAL); return JSTokenTypes.TYPEOF_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "var"                 { yybegin(YYINITIAL); return JSTokenTypes.VAR_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "void"                { yybegin(YYINITIAL); return JSTokenTypes.VOID_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "while"               { yybegin(YYINITIAL); return JSTokenTypes.WHILE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "with"                { yybegin(YYINITIAL); return JSTokenTypes.WITH_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "set"                 { yybegin(YYINITIAL); return JSTokenTypes.SET_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "get"                 { yybegin(YYINITIAL); return JSTokenTypes.GET_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "yield"               { yybegin(YYINITIAL); return JSTokenTypes.YIELD_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "let"                 { yybegin(YYINITIAL); return JSTokenTypes.LET_KEYWORD; }

<YYINITIAL, DIV_OR_GT> {IDENTIFIER}                    { yybegin(DIV_OR_GT);       return JSTokenTypes.IDENTIFIER; }


<YYINITIAL, DIV_OR_GT> "."                            { yybegin(YYINITIAL); return JSTokenTypes.DOT; }


<YYINITIAL,DIV_OR_GT> "==="                 { yybegin(YYINITIAL); return JSTokenTypes.EQEQEQ; }
<YYINITIAL,DIV_OR_GT> "!=="                 { yybegin(YYINITIAL); return JSTokenTypes.NEQEQ; }

<YYINITIAL,DIV_OR_GT> "++"                  { return JSTokenTypes.PLUSPLUS; }
<YYINITIAL,DIV_OR_GT> "--"                  { return JSTokenTypes.MINUSMINUS; }

<YYINITIAL,DIV_OR_GT> "=="                  { yybegin(YYINITIAL); return JSTokenTypes.EQEQ; }
<YYINITIAL,DIV_OR_GT> "!="                  { yybegin(YYINITIAL); return JSTokenTypes.NE; }
<DIV_OR_GT> "<"                             { yybegin(YYINITIAL); return JSTokenTypes.LT; }
<YYINITIAL,DIV_OR_GT> ">"                   { yybegin(YYINITIAL); return JSTokenTypes.GT; }
<DIV_OR_GT> "<="                            { yybegin(YYINITIAL); return JSTokenTypes.LE; }
<YYINITIAL,DIV_OR_GT> ">="                 { yybegin(YYINITIAL); return JSTokenTypes.GE; }
<DIV_OR_GT> "<<"                            { yybegin(YYINITIAL); return JSTokenTypes.LTLT; }
<YYINITIAL,DIV_OR_GT> ">>"                  { yybegin(YYINITIAL); return JSTokenTypes.GTGT; }
<YYINITIAL,DIV_OR_GT> ">>>"                 { yybegin(YYINITIAL); return JSTokenTypes.GTGTGT; }

<YYINITIAL,DIV_OR_GT> "&"                   { yybegin(YYINITIAL); return JSTokenTypes.AND; }
<YYINITIAL,DIV_OR_GT> "&&"                  { yybegin(YYINITIAL); return JSTokenTypes.ANDAND; }
<YYINITIAL,DIV_OR_GT> "|"                   { yybegin(YYINITIAL); return JSTokenTypes.OR; }
<YYINITIAL,DIV_OR_GT> "||"                  { yybegin(YYINITIAL); return JSTokenTypes.OROR; }

<YYINITIAL,DIV_OR_GT> "+="                  { yybegin(YYINITIAL); return JSTokenTypes.PLUSEQ; }
<YYINITIAL,DIV_OR_GT> "-="                  { yybegin(YYINITIAL); return JSTokenTypes.MINUSEQ; }
<DIV_OR_GT> "*="                            { yybegin(YYINITIAL); return JSTokenTypes.MULTEQ; }
<DIV_OR_GT> "/="                            { yybegin(YYINITIAL); return JSTokenTypes.DIVEQ; }
<YYINITIAL,DIV_OR_GT> "&="                  { yybegin(YYINITIAL); return JSTokenTypes.ANDEQ; }
<YYINITIAL,DIV_OR_GT> "|="                  { yybegin(YYINITIAL); return JSTokenTypes.OREQ; }
<YYINITIAL,DIV_OR_GT> "^="                  { yybegin(YYINITIAL); return JSTokenTypes.XOREQ; }
<YYINITIAL,DIV_OR_GT> "%="                  { yybegin(YYINITIAL); return JSTokenTypes.PERCEQ; }
<DIV_OR_GT> "<<="                           { yybegin(YYINITIAL); return JSTokenTypes.LTLTEQ; }
<YYINITIAL,DIV_OR_GT> ">>="                 { yybegin(YYINITIAL); return JSTokenTypes.GTGTEQ; }
<YYINITIAL,DIV_OR_GT> ">>>="                { yybegin(YYINITIAL); return JSTokenTypes.GTGTGTEQ; }

<YYINITIAL,DIV_OR_GT> "("                   { yybegin(YYINITIAL); return JSTokenTypes.LPAR; }
<YYINITIAL,DIV_OR_GT> ")"                   { yybegin(DIV_OR_GT);       return JSTokenTypes.RPAR; }
<YYINITIAL,DIV_OR_GT> "{"                   { yybegin(YYINITIAL); return JSTokenTypes.LBRACE; }
<YYINITIAL,DIV_OR_GT> "}"                   { yybegin(YYINITIAL); return JSTokenTypes.RBRACE; }
<YYINITIAL,DIV_OR_GT> "["                   { yybegin(YYINITIAL); return JSTokenTypes.LBRACKET; }
<YYINITIAL,DIV_OR_GT> "]"                   { yybegin(DIV_OR_GT);       return JSTokenTypes.RBRACKET; }
<YYINITIAL,DIV_OR_GT> ";"                   { yybegin(YYINITIAL); return JSTokenTypes.SEMICOLON; }
<YYINITIAL,DIV_OR_GT> ","                   { yybegin(YYINITIAL); return JSTokenTypes.COMMA; }

<YYINITIAL,DIV_OR_GT> "="                   { yybegin(YYINITIAL); return JSTokenTypes.EQ; }
<YYINITIAL,DIV_OR_GT> "!"                   { yybegin(YYINITIAL); return JSTokenTypes.EXCL; }
<YYINITIAL,DIV_OR_GT> "~"                   { yybegin(YYINITIAL); return JSTokenTypes.TILDE; }
<YYINITIAL,DIV_OR_GT> "?"                   { yybegin(YYINITIAL); return JSTokenTypes.QUEST; }
<YYINITIAL,DIV_OR_GT> ":"                   { yybegin(YYINITIAL); return JSTokenTypes.COLON; }
<YYINITIAL,DIV_OR_GT> "+"                   { yybegin(YYINITIAL); return JSTokenTypes.PLUS; }
<YYINITIAL,DIV_OR_GT> "-"                   { yybegin(YYINITIAL); return JSTokenTypes.MINUS; }
<YYINITIAL,DIV_OR_GT> "*"                             { yybegin(YYINITIAL); return JSTokenTypes.MULT; }
<DIV_OR_GT> "/"                             { yybegin(YYINITIAL); return JSTokenTypes.DIV; }
<YYINITIAL,DIV_OR_GT> "^"                   { yybegin(YYINITIAL); return JSTokenTypes.XOR; }
<YYINITIAL,DIV_OR_GT> "%"                   { yybegin(YYINITIAL); return JSTokenTypes.PERC; }

<YYINITIAL> {REGEXP_LITERAL}                { return JSTokenTypes.REGEXP_LITERAL; }

<YYINITIAL> "/"                             { return JSTokenTypes.DIV; }

<YYINITIAL,DIV_OR_GT> [^]                   { return JSTokenTypes.BAD_CHARACTER; }