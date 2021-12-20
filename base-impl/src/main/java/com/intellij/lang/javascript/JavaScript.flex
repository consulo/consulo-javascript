package com.intellij.lang.javascript;

import com.intellij.lexer.FlexLexer;
import com.intellij.psi.tree.IElementType;

%%

%{
    public _JavaScriptLexer(boolean highlightMode, DialectOptionHolder _optionsHolder) {
      this((java.io.Reader)null);
      isHighlightModeOn = highlightMode;
      optionsHolder = _optionsHolder;
    }

    private boolean isHighlightModeOn = false;
    private DialectOptionHolder optionsHolder;
    private int tagCount = 0;

    public final int getTagCount() {
      return tagCount;
    }

    public final void setTagCount(int _tagCount) {
      tagCount = _tagCount;
    }
%}

%class _JavaScriptLexer
%implements FlexLexer
%unicode
%function advance
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
ALPHA=[:letter:]
DIGIT=[0-9]
XML_NAME=({ALPHA}|"_")({ALPHA}|{DIGIT}|"_"|"."|"-")*(":"({ALPHA}|"_")?({ALPHA}|{DIGIT}|"_"|"."|"-")*)?
XML_COMMENT="<!--" ([^\-<\/] | (\-[^\-<\/])| (<[^!\-\/]) | (\-\-[^<>\/] | (\/[^<\/\-]) ))* "-->"

FIELD_OR_METHOD={IDENTIFIER} ("(" [^ \\)]* ")"? )?

%state DIV_OR_GT
%state GENERIC_SIGNATURE
%state MAY_BE_GENERIC_SIGNATURE
%state TAG
%state TAG_ATTRIBUTES
%state TAG_CONTENT
%state TAG_END
%state TAG_ATTR_SQ
%state TAG_ATTR_DQ
%state COMMENT
%state GWT_FIELD_OR_METHOD
%state LAST_STATE

%%

<YYINITIAL,TAG,TAG_END,DIV_OR_GT,COMMENT,TAG_CONTENT,GENERIC_SIGNATURE,MAY_BE_GENERIC_SIGNATURE> {WHITE_SPACE_CHAR}+   { return JSTokenTypes.WHITE_SPACE; }
<TAG_ATTRIBUTES> {WHITE_SPACE_CHAR}+   { return isHighlightModeOn ? JSTokenTypes.XML_TAG_WHITE_SPACE:JSTokenTypes.WHITE_SPACE; }

<YYINITIAL> "<" {
  if (optionsHolder.hasE4X) {
    tagCount = 0;
    yybegin(TAG); yypushback(yylength());
  } else {
    return JSTokenTypes.LT;
  }
}

<TAG> "<>" { tagCount++; yybegin(TAG_CONTENT); return JSTokenTypes.XML_START_TAG_LIST; }
<TAG> "</>" { tagCount--; yybegin(YYINITIAL); return JSTokenTypes.XML_END_TAG_LIST; }

<TAG,TAG_CONTENT> "<" { tagCount++; yybegin(TAG); return JSTokenTypes.XML_START_TAG_START; }
<TAG_CONTENT> "</" { tagCount--; yybegin(TAG_END); return JSTokenTypes.XML_END_TAG_START; }

<TAG> {
  {XML_NAME} { yybegin(TAG_ATTRIBUTES); return JSTokenTypes.XML_TAG_NAME; }
  "{" [^}]* "}" { yybegin(TAG_ATTRIBUTES); return JSTokenTypes.XML_JS_SCRIPT; }
}

<TAG_END> {
  {XML_NAME} { return JSTokenTypes.XML_TAG_NAME; }
  "{" [^}]* "}" { return JSTokenTypes.XML_JS_SCRIPT; }
}

<TAG_ATTRIBUTES> {
  {XML_NAME} { return JSTokenTypes.XML_NAME; }
  "{" [^}]* "}" { return JSTokenTypes.XML_JS_SCRIPT; }
}

<TAG_CONTENT> "{" [^}]* "}" { return JSTokenTypes.XML_JS_SCRIPT; }
<TAG_CONTENT> ([^\?<&\{# \n\r\t\f])* { return JSTokenTypes.XML_TAG_CONTENT; }
<TAG_CONTENT> "<?" ([^\?]|(\?[^\>]))* "?>" { return JSTokenTypes.XML_TAG_CONTENT; }
<TAG_CONTENT> {XML_COMMENT} { return JSTokenTypes.XML_STYLE_COMMENT; }

<TAG_CONTENT,TAG_ATTR_SQ, TAG_ATTR_DQ>
  "&" {XML_NAME} ";" |
  "&#" {DIGIT}+ ";" |
  "&#x" ({DIGIT}|[a-fA-F])+ ";"
{
  return JSTokenTypes.XML_ENTITY_REF;
}

<TAG> "/>" { yybegin(--tagCount == 0 ? YYINITIAL:TAG_CONTENT); return JSTokenTypes.XML_EMPTY_TAG_END; }

<TAG_END> ">" { yybegin(tagCount == 0 ? YYINITIAL:TAG_CONTENT); return JSTokenTypes.XML_TAG_END; }
<TAG_ATTRIBUTES> "=" { return JSTokenTypes.XML_ATTR_EQUAL; }
<TAG_ATTRIBUTES> "\'" { yybegin(TAG_ATTR_SQ); return JSTokenTypes.XML_ATTR_VALUE_START; }
<TAG_ATTRIBUTES> "\"" { yybegin(TAG_ATTR_DQ); return JSTokenTypes.XML_ATTR_VALUE_START; }
<TAG_ATTRIBUTES> ">" { yybegin(TAG_CONTENT); return JSTokenTypes.XML_TAG_END; }
<TAG_ATTRIBUTES> [^] { yybegin(TAG); yypushback(yylength()); }

<TAG_ATTR_SQ> "\'" { yybegin(TAG_ATTRIBUTES); return JSTokenTypes.XML_ATTR_VALUE_END; }
<TAG_ATTR_SQ> [^\']* { return JSTokenTypes.XML_ATTR_VALUE; }
<TAG_ATTR_DQ> "\"" { yybegin(TAG_ATTRIBUTES); return JSTokenTypes.XML_ATTR_VALUE_END; }
<TAG_ATTR_DQ> [^\"]* { return JSTokenTypes.XML_ATTR_VALUE; }

<TAG,TAG_END, TAG_CONTENT> [^] { return JSTokenTypes.BAD_CHARACTER; }

<YYINITIAL> "<!--" (.|{CRLF}+)* "//" {WHITE_SPACE_CHAR}* "-->" {
  yybegin(COMMENT); yypushback(yylength());
}

<YYINITIAL> {XML_COMMENT} {
  yybegin(YYINITIAL); return JSTokenTypes.XML_STYLE_COMMENT;
}

<COMMENT> "<!--" { yybegin(YYINITIAL); return JSTokenTypes.XML_STYLE_COMMENT_START; }
<COMMENT> [^] { yybegin(YYINITIAL); yypushback(1); }

<YYINITIAL,DIV_OR_GT> {C_STYLE_COMMENT}     { return JSTokenTypes.C_STYLE_COMMENT; }
<YYINITIAL,DIV_OR_GT> {END_OF_LINE_COMMENT} { return JSTokenTypes.END_OF_LINE_COMMENT; }
<YYINITIAL,DIV_OR_GT> {DOC_COMMENT}         { return JSTokenTypes.DOC_COMMENT; }

<YYINITIAL,DIV_OR_GT> {INTEGER_LITERAL}     { yybegin(DIV_OR_GT); return JSTokenTypes.NUMERIC_LITERAL; }
<YYINITIAL,DIV_OR_GT> {FLOAT_LITERAL}       { yybegin(DIV_OR_GT); return JSTokenTypes.NUMERIC_LITERAL; }

<YYINITIAL,DIV_OR_GT> {QUOTED_LITERAL}      {
                        yybegin(DIV_OR_GT);
                        return isHighlightModeOn ?
                          JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL:
                          JSTokenTypes.STRING_LITERAL;
                      }

<YYINITIAL,DIV_OR_GT>  "\\${" [^\}]* "}" { return JSTokenTypes.JSP_TEXT; }
<YYINITIAL,DIV_OR_GT>  "${" [^\}]* "}" { return JSTokenTypes.JSP_TEXT; }                      
<YYINITIAL,DIV_OR_GT> {DOUBLE_QUOTED_LITERAL}      { yybegin(DIV_OR_GT); return JSTokenTypes.STRING_LITERAL; }

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

<YYINITIAL,DIV_OR_GT> "::"                  {
                          if (optionsHolder.isGwt) {
                            yypushback(yylength());
                            yybegin(GWT_FIELD_OR_METHOD);
                          } else if (optionsHolder.hasE4X) {
                            yybegin(YYINITIAL);
                            return JSTokenTypes.COLON_COLON;
                          }
                          else {
                            yypushback(yylength() - 1);
                            return JSTokenTypes.COLON;
                          }
                       }

 <GWT_FIELD_OR_METHOD> {FIELD_OR_METHOD} {
   yybegin(YYINITIAL);
   return JSTokenTypes.GWT_FIELD_OR_METHOD;
 }

 <GWT_FIELD_OR_METHOD> "::" {
   return JSTokenTypes.COLON_COLON;
 }

 <GWT_FIELD_OR_METHOD> [^] {
   yypushback(yylength());
   yybegin(YYINITIAL);
 }

<YYINITIAL,DIV_OR_GT> "..."                  {
                          if (optionsHolder.isECMAL4Level) {
                            yybegin(YYINITIAL);
                            return JSTokenTypes.DOT_DOT_DOT;
                          } else {
                            yypushback(yylength() - 1);
                            return JSTokenTypes.DOT;
                          }
                      }

<YYINITIAL,DIV_OR_GT> ".."                  {
                          if (optionsHolder.hasE4X) {
                            yybegin(YYINITIAL);
                            return JSTokenTypes.DOT_DOT;
                          } else {
                            yypushback(yylength() - 1);
                            return JSTokenTypes.DOT;
                          }
                      }
<YYINITIAL,DIV_OR_GT> "class"               { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.CLASS_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "virtual"             { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.VIRTUAL_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "interface"           { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.INTERFACE_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "extends"             { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.EXTENDS_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "implements"          { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.IMPLEMENTS_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "public"              { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.PUBLIC_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "static"              { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.STATIC_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "internal"            { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.INTERNAL_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "final"               { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.FINAL_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "dynamic"             { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.DYNAMIC_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "native"              { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.NATIVE_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "package"             { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.PACKAGE_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "import"              { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.IMPORT_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "use"                 { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.USE_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "namespace"           { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.NAMESPACE_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "override"            { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.OVERRIDE_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "private"             { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.PRIVATE_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "protected"           { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.PROTECTED_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "super"               { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.SUPER_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "include"             { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.INCLUDE_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "is"                  { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.IS_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "as"                  { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.AS_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "each"                { yybegin(YYINITIAL); return optionsHolder.hasE4X ? JSTokenTypes.EACH_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "uint"                { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.UINT_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "int"                 { yybegin(YYINITIAL); return optionsHolder.isECMAL4Level ? JSTokenTypes.INT_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "yield"               { yybegin(YYINITIAL); return optionsHolder.isJavaScript1_7_OrBetter ? JSTokenTypes.YIELD_KEYWORD:JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "let"                 { yybegin(YYINITIAL); return optionsHolder.isJavaScript1_7_OrBetter ? JSTokenTypes.LET_KEYWORD:JSTokenTypes.IDENTIFIER; }

<YYINITIAL,DIV_OR_GT> "@"                   {
  if (optionsHolder.hasE4X || optionsHolder.isGwt) {
    return JSTokenTypes.AT;
  } else {
    return JSTokenTypes.BAD_CHARACTER;
  }
}

<YYINITIAL,DIV_OR_GT> {IDENTIFIER}          { yybegin(DIV_OR_GT);       return JSTokenTypes.IDENTIFIER; }
<YYINITIAL,DIV_OR_GT> "#include" {
  if (optionsHolder.isECMAL4Level) {
    yybegin(YYINITIAL);
    return JSTokenTypes.INCLUDE_KEYWORD;
  }
  else {
    yypushback(yylength() - 1);
    return JSTokenTypes.BAD_CHARACTER;
  }
}

<YYINITIAL> "*"       { if (optionsHolder.hasE4X) {
                          yybegin(DIV_OR_GT);
                          return JSTokenTypes.ANY_IDENTIFIER;
                        } else {
                          return JSTokenTypes.MULT;
                        }
                      }

<YYINITIAL,DIV_OR_GT> "."                   { yybegin(optionsHolder.isECMAL4Level ? MAY_BE_GENERIC_SIGNATURE : YYINITIAL); return JSTokenTypes.DOT; }
<MAY_BE_GENERIC_SIGNATURE> "<"              {
                                               yybegin(GENERIC_SIGNATURE);
                                               tagCount = 1;
                                               return JSTokenTypes.LT;
                                            }
<MAY_BE_GENERIC_SIGNATURE> .                {
                                               yypushback(yylength());
                                               yybegin(YYINITIAL);
                                            }
<GENERIC_SIGNATURE> "<"                     {
                                               ++tagCount;
                                               return JSTokenTypes.LT;
                                            }
<GENERIC_SIGNATURE> ">"                     {
                                               --tagCount;
                                               if (tagCount == 0) {
                                                 yybegin(YYINITIAL);
                                               }
                                               return JSTokenTypes.GT;
                                            }
<GENERIC_SIGNATURE> "."                     { return JSTokenTypes.DOT; }
<GENERIC_SIGNATURE> "int"                   { return JSTokenTypes.INT_KEYWORD; }
<GENERIC_SIGNATURE> "uint"                  { return JSTokenTypes.UINT_KEYWORD; }
<GENERIC_SIGNATURE> {IDENTIFIER}            { return JSTokenTypes.IDENTIFIER; }
<GENERIC_SIGNATURE> .                       {
                                              yybegin(YYINITIAL);
                                              return JSTokenTypes.BAD_CHARACTER;
                                            }

<YYINITIAL,DIV_OR_GT> "==="                 { yybegin(YYINITIAL); return JSTokenTypes.EQEQEQ; }
<YYINITIAL,DIV_OR_GT> "!=="                 { yybegin(YYINITIAL); return JSTokenTypes.NEQEQ; }

<YYINITIAL,DIV_OR_GT> "++"                  { return JSTokenTypes.PLUSPLUS; }
<YYINITIAL,DIV_OR_GT> "--"                  { return JSTokenTypes.MINUSMINUS; }

<YYINITIAL,DIV_OR_GT> "=="                  { yybegin(YYINITIAL); return JSTokenTypes.EQEQ; }
<YYINITIAL,DIV_OR_GT> "!="                  { yybegin(YYINITIAL); return JSTokenTypes.NE; }
<DIV_OR_GT> "<"       { yybegin(YYINITIAL); return JSTokenTypes.LT; }
<YYINITIAL,DIV_OR_GT> "&lt;"                { yybegin(YYINITIAL); return JSTokenTypes.LT; }
<YYINITIAL,DIV_OR_GT> ">"                   { yybegin(YYINITIAL); return JSTokenTypes.GT; }
<YYINITIAL,DIV_OR_GT> "&gt;"                { yybegin(YYINITIAL); return JSTokenTypes.GT; }
<DIV_OR_GT> "<="      { yybegin(YYINITIAL); return JSTokenTypes.LE; }
<YYINITIAL,DIV_OR_GT> "&lt;="               { yybegin(YYINITIAL); return JSTokenTypes.LE; }
<YYINITIAL,DIV_OR_GT> ">="                  { yybegin(YYINITIAL); return JSTokenTypes.GE; }
<YYINITIAL,DIV_OR_GT> "&gt;="               { yybegin(YYINITIAL); return JSTokenTypes.GE; }
<DIV_OR_GT> "<<"      { yybegin(YYINITIAL); return JSTokenTypes.LTLT; }
<YYINITIAL,DIV_OR_GT> "&lt;&lt;"            { yybegin(YYINITIAL); return JSTokenTypes.LTLT; }
<YYINITIAL,DIV_OR_GT> ">>"                  { yybegin(YYINITIAL); return JSTokenTypes.GTGT; }
<YYINITIAL,DIV_OR_GT> "&gt;&gt;"            { yybegin(YYINITIAL); return JSTokenTypes.GTGT; }
<YYINITIAL,DIV_OR_GT> ">>>"                 { yybegin(YYINITIAL); return JSTokenTypes.GTGTGT; }
<YYINITIAL,DIV_OR_GT> "&gt;&gt;&gt;"        { yybegin(YYINITIAL); return JSTokenTypes.GTGTGT; }

<YYINITIAL,DIV_OR_GT> "&"                   { yybegin(YYINITIAL); return JSTokenTypes.AND; }
<YYINITIAL,DIV_OR_GT> "&amp;"               { yybegin(YYINITIAL); return JSTokenTypes.AND; }
<YYINITIAL,DIV_OR_GT> "&&"                  { yybegin(YYINITIAL); return JSTokenTypes.ANDAND; }
<YYINITIAL,DIV_OR_GT> "&amp;&amp;"          { yybegin(YYINITIAL); return JSTokenTypes.ANDAND; }
<YYINITIAL,DIV_OR_GT> "|"                   { yybegin(YYINITIAL); return JSTokenTypes.OR; }
<YYINITIAL,DIV_OR_GT> "||"                  { yybegin(YYINITIAL); return JSTokenTypes.OROR; }

<YYINITIAL,DIV_OR_GT> "+="                  { yybegin(YYINITIAL); return JSTokenTypes.PLUSEQ; }
<YYINITIAL,DIV_OR_GT> "-="                  { yybegin(YYINITIAL); return JSTokenTypes.MINUSEQ; }
<DIV_OR_GT> "*="      { yybegin(YYINITIAL); return JSTokenTypes.MULTEQ; }
<DIV_OR_GT> "/="      { yybegin(YYINITIAL); return JSTokenTypes.DIVEQ; }
<YYINITIAL,DIV_OR_GT> "&="                  { yybegin(YYINITIAL); return JSTokenTypes.ANDEQ; }
<YYINITIAL,DIV_OR_GT> "&amp;="              { yybegin(YYINITIAL); return JSTokenTypes.ANDEQ; }
<YYINITIAL,DIV_OR_GT> "|="                  { yybegin(YYINITIAL); return JSTokenTypes.OREQ; }
<YYINITIAL,DIV_OR_GT> "^="                  { yybegin(YYINITIAL); return JSTokenTypes.XOREQ; }
<YYINITIAL,DIV_OR_GT> "%="                  { yybegin(YYINITIAL); return JSTokenTypes.PERCEQ; }
<DIV_OR_GT> "<<="     { yybegin(YYINITIAL); return JSTokenTypes.LTLTEQ; }
<YYINITIAL,DIV_OR_GT> "&lt;&lt;="           { yybegin(YYINITIAL); return JSTokenTypes.LTLTEQ; }
<YYINITIAL,DIV_OR_GT> ">>="                 { yybegin(YYINITIAL); return JSTokenTypes.GTGTEQ; }
<YYINITIAL,DIV_OR_GT> "&gt;&gt;="           { yybegin(YYINITIAL); return JSTokenTypes.GTGTEQ; }
<YYINITIAL,DIV_OR_GT> ">>>="                { yybegin(YYINITIAL); return JSTokenTypes.GTGTGTEQ; }
<YYINITIAL,DIV_OR_GT> "&gt;&gt;&gt;="       { yybegin(YYINITIAL); return JSTokenTypes.GTGTGTEQ; }

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
<DIV_OR_GT> "*"       { yybegin(YYINITIAL); return JSTokenTypes.MULT; }
<DIV_OR_GT> "/"       { yybegin(YYINITIAL); return JSTokenTypes.DIV; }
<YYINITIAL,DIV_OR_GT> "^"                   { yybegin(YYINITIAL); return JSTokenTypes.XOR; }
<YYINITIAL,DIV_OR_GT> "%"                   { yybegin(YYINITIAL); return JSTokenTypes.PERC; }

<YYINITIAL> {REGEXP_LITERAL} { return JSTokenTypes.REGEXP_LITERAL; }

<YYINITIAL,DIV_OR_GT> "]]>" { yybegin(YYINITIAL); return JSTokenTypes.CDATA_END; }
<YYINITIAL,DIV_OR_GT> "<![CDATA[" { yybegin(YYINITIAL); return JSTokenTypes.CDATA_START; }
.                     { yybegin(YYINITIAL); return JSTokenTypes.BAD_CHARACTER; }