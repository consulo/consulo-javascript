package consulo.typescript.language.impl.syntax;

import consulo.language.ast.IElementType;
import consulo.language.ast.TokenType;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.typescript.language.TypeScriptTokens;

%%

%{
    private int templateDepth = 0;
    private consulo.util.collection.primitive.ints.IntStack templateBraceCount = new consulo.util.collection.primitive.ints.IntStack();
%}

%public
%class _TypeScriptLexer
%extends consulo.language.lexer.LexerBase
%unicode
%function advanceImpl
%type IElementType

DIGIT=[0-9]
HEX_DIGIT=[0-9A-Fa-f]
WHITE_SPACE_CHAR=[\ \n\r\t\f]

IDENTIFIER=[:jletter:] [:jletterdigit:]*

C_STYLE_COMMENT=("/*"[^"*"]{COMMENT_TAIL})|"/*"
DOC_COMMENT="/*""*"+("/"|([^"/""*"]{COMMENT_TAIL}))?
COMMENT_TAIL=([^"*"]*("*"+[^"*""/"])?)*("*"+"/")?
END_OF_LINE_COMMENT="/""/"[^\r\n]*

INTEGER_LITERAL={DECIMAL_INTEGER_LITERAL}|{HEX_INTEGER_LITERAL}|{BIN_INTEGER_LITERAL}|{OCTAL_INTEGER_LITERAL}
BIN_INTEGER_LITERAL=0[Bb][01]([01_]*[01])?
OCTAL_INTEGER_LITERAL=0[oO]{DIGIT}([0-7_]*[0-7])?
DECIMAL_INTEGER_LITERAL=(0|([1-9]([0-9_]*[0-9])?))
HEX_INTEGER_LITERAL=0[Xx]{HEX_DIGIT}({HEX_DIGIT}|_)*

BIGINT_LITERAL={INTEGER_LITERAL}[n]

FLOAT_LITERAL=({FLOATING_POINT_LITERAL1})|({FLOATING_POINT_LITERAL2})|({FLOATING_POINT_LITERAL3})|({FLOATING_POINT_LITERAL4})
FLOATING_POINT_LITERAL1=({DIGIT})+"."({DIGIT})*({EXPONENT_PART})?
FLOATING_POINT_LITERAL2="."({DIGIT})+({EXPONENT_PART})?
FLOATING_POINT_LITERAL3=({DIGIT})+({EXPONENT_PART})
FLOATING_POINT_LITERAL4=({DIGIT})+
EXPONENT_PART=[Ee]["+""-"]?({DIGIT})*

CRLF=[\ \t\f]*(\n|\r|\r\n)
QUOTED_LITERAL="'"([^\\\'\r\n]|{ESCAPE_SEQUENCE}|\\{CRLF})*("'"|\\)?
DOUBLE_QUOTED_LITERAL=\"([^\\\"\r\n]|{ESCAPE_SEQUENCE}|\\{CRLF})*(\"|\\)?
ESCAPE_SEQUENCE=\\[^\r\n]
GROUP="["[^\]]*"]"

REGEXP_LITERAL="/"([^\*\\/\r\n]|{ESCAPE_SEQUENCE}|{GROUP})([^\\/\r\n]|{ESCAPE_SEQUENCE}|{GROUP})*("/"[gimsuy]*)?

%state DIV_OR_GT
%state TEMPLATE_STRING
%state TEMPLATE_STRING_EXPRESSION

%%

<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> {WHITE_SPACE_CHAR}+ { return JSTokenTypes.WHITE_SPACE; }

<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> {C_STYLE_COMMENT}     { return JSTokenTypes.C_STYLE_COMMENT; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> {END_OF_LINE_COMMENT}  { return JSTokenTypes.END_OF_LINE_COMMENT; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> {DOC_COMMENT}          { return JSTokenTypes.DOC_COMMENT; }

// === Numeric literals ===
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> {BIGINT_LITERAL}    { yybegin(DIV_OR_GT); return JSTokenTypes.NUMERIC_LITERAL; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> {INTEGER_LITERAL}   { yybegin(DIV_OR_GT); return JSTokenTypes.NUMERIC_LITERAL; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> {FLOAT_LITERAL}     { yybegin(DIV_OR_GT); return JSTokenTypes.NUMERIC_LITERAL; }

// === String literals ===
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> {QUOTED_LITERAL}        { yybegin(DIV_OR_GT); return JSTokenTypes.STRING_LITERAL; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> {DOUBLE_QUOTED_LITERAL} { yybegin(DIV_OR_GT); return JSTokenTypes.STRING_LITERAL; }

// === Template literals ===
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "`" {
    yybegin(TEMPLATE_STRING);
    return TypeScriptTokens.TEMPLATE_HEAD;
}

<TEMPLATE_STRING> {
    "${"  {
        templateDepth++;
        templateBraceCount.push(0);
        yybegin(TEMPLATE_STRING_EXPRESSION);
        return TypeScriptTokens.TEMPLATE_HEAD;
    }
    "`"   {
        yybegin(DIV_OR_GT);
        return TypeScriptTokens.NO_SUBSTITUTION_TEMPLATE;
    }
    [^`$\\]+      { return TypeScriptTokens.NO_SUBSTITUTION_TEMPLATE; }
    "\\".         { return TypeScriptTokens.NO_SUBSTITUTION_TEMPLATE; }
    "$"           { return TypeScriptTokens.NO_SUBSTITUTION_TEMPLATE; }
}

<TEMPLATE_STRING_EXPRESSION> {
    "{"  {
        if (templateDepth > 0) {
            templateBraceCount.push(templateBraceCount.pop() + 1);
        }
        yybegin(YYINITIAL);
        return JSTokenTypes.LBRACE;
    }
    "}"  {
        if (templateDepth > 0) {
            int count = templateBraceCount.peek();
            if (count > 0) {
                templateBraceCount.push(templateBraceCount.pop() - 1);
                yybegin(YYINITIAL);
                return JSTokenTypes.RBRACE;
            } else {
                templateDepth--;
                templateBraceCount.pop();
                yybegin(TEMPLATE_STRING);
                return TypeScriptTokens.TEMPLATE_TAIL;
            }
        }
        yybegin(YYINITIAL);
        return JSTokenTypes.RBRACE;
    }
}

// === Keywords (true keywords - always keyword tokens) ===
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "break"       { yybegin(YYINITIAL); return JSTokenTypes.BREAK_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "case"        { yybegin(YYINITIAL); return JSTokenTypes.CASE_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "catch"       { yybegin(YYINITIAL); return JSTokenTypes.CATCH_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "class"       { yybegin(YYINITIAL); return JSTokenTypes.CLASS_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "const"       { yybegin(YYINITIAL); return JSTokenTypes.CONST_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "continue"    { yybegin(YYINITIAL); return JSTokenTypes.CONTINUE_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "debugger"    { yybegin(YYINITIAL); return TypeScriptTokens.DEBUGGER_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "default"     { yybegin(YYINITIAL); return JSTokenTypes.DEFAULT_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "delete"      { yybegin(YYINITIAL); return JSTokenTypes.DELETE_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "do"          { yybegin(YYINITIAL); return JSTokenTypes.DO_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "else"        { yybegin(YYINITIAL); return JSTokenTypes.ELSE_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "enum"        { yybegin(YYINITIAL); return JSTokenTypes.ENUM_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "export"      { yybegin(YYINITIAL); return JSTokenTypes.EXPORT_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "extends"     { yybegin(YYINITIAL); return JSTokenTypes.EXTENDS_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "false"       { yybegin(DIV_OR_GT); return JSTokenTypes.FALSE_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "finally"     { yybegin(YYINITIAL); return JSTokenTypes.FINALLY_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "for"         { yybegin(YYINITIAL); return JSTokenTypes.FOR_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "function"    { yybegin(YYINITIAL); return JSTokenTypes.FUNCTION_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "if"          { yybegin(YYINITIAL); return JSTokenTypes.IF_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "import"      { yybegin(YYINITIAL); return JSTokenTypes.IMPORT_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "in"          { yybegin(YYINITIAL); return JSTokenTypes.IN_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "instanceof"  { yybegin(YYINITIAL); return JSTokenTypes.INSTANCEOF_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "let"         { yybegin(YYINITIAL); return JSTokenTypes.LET_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "new"         { yybegin(YYINITIAL); return JSTokenTypes.NEW_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "null"        { yybegin(DIV_OR_GT); return JSTokenTypes.NULL_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "return"      { yybegin(YYINITIAL); return JSTokenTypes.RETURN_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "super"       { yybegin(YYINITIAL); return JSTokenTypes.SUPER_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "switch"      { yybegin(YYINITIAL); return JSTokenTypes.SWITCH_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "this"        { yybegin(DIV_OR_GT); return JSTokenTypes.THIS_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "throw"       { yybegin(YYINITIAL); return JSTokenTypes.THROW_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "true"        { yybegin(DIV_OR_GT); return JSTokenTypes.TRUE_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "try"         { yybegin(YYINITIAL); return JSTokenTypes.TRY_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "typeof"      { yybegin(YYINITIAL); return JSTokenTypes.TYPEOF_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "var"         { yybegin(YYINITIAL); return JSTokenTypes.VAR_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "void"        { yybegin(YYINITIAL); return JSTokenTypes.VOID_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "while"       { yybegin(YYINITIAL); return JSTokenTypes.WHILE_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "with"        { yybegin(YYINITIAL); return JSTokenTypes.WITH_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "yield"       { yybegin(YYINITIAL); return JSTokenTypes.YIELD_KEYWORD; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "await"       { yybegin(YYINITIAL); return TypeScriptTokens.AWAIT_KEYWORD; }

// Contextual keywords emitted as IDENTIFIER - parser resolves them:
// abstract, as, asserts, async, constructor, declare, defer, from, get, global,
// implements, infer, interface, is, keyof, module, namespace, never, of,
// override, private, protected, public, readonly, require, satisfies, set,
// static, type, undefined, unique, unknown, using

// === Identifiers ===
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> {IDENTIFIER}  { yybegin(DIV_OR_GT); return JSTokenTypes.IDENTIFIER; }

// === Multi-char operators (order matters: longer first) ===
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "..."   { yybegin(YYINITIAL); return JSTokenTypes.DOT_DOT_DOT; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "==="   { yybegin(YYINITIAL); return JSTokenTypes.EQEQEQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "!=="   { yybegin(YYINITIAL); return JSTokenTypes.NEQEQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> ">>>"   { yybegin(YYINITIAL); return JSTokenTypes.GTGTGT; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> ">>>="  { yybegin(YYINITIAL); return JSTokenTypes.GTGTGTEQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> ">>="   { yybegin(YYINITIAL); return JSTokenTypes.GTGTEQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "<<="   { yybegin(YYINITIAL); return JSTokenTypes.LTLTEQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "**="   { yybegin(YYINITIAL); return JSTokenTypes.MULT_MULT_EQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "||="   { yybegin(YYINITIAL); return JSTokenTypes.OR_OR_EQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "&&="   { yybegin(YYINITIAL); return JSTokenTypes.AND_AND_EQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "??="   { yybegin(YYINITIAL); return JSTokenTypes.QUEST_QUEST_EQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "=>"    { yybegin(YYINITIAL); return JSTokenTypes.DARROW; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "=="    { yybegin(YYINITIAL); return JSTokenTypes.EQEQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "!="    { yybegin(YYINITIAL); return JSTokenTypes.NE; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "++"    { return JSTokenTypes.PLUSPLUS; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "--"    { return JSTokenTypes.MINUSMINUS; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "**"    { yybegin(YYINITIAL); return JSTokenTypes.MULTMULT; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "<<"    { yybegin(YYINITIAL); return JSTokenTypes.LTLT; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> ">>"    { yybegin(YYINITIAL); return JSTokenTypes.GTGT; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "&&"    { yybegin(YYINITIAL); return JSTokenTypes.ANDAND; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "||"    { yybegin(YYINITIAL); return JSTokenTypes.OROR; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "??"    { yybegin(YYINITIAL); return TypeScriptTokens.QUEST_QUEST; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "?."    { yybegin(YYINITIAL); return JSTokenTypes.QUEST_DOT; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "+="    { yybegin(YYINITIAL); return JSTokenTypes.PLUSEQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "-="    { yybegin(YYINITIAL); return JSTokenTypes.MINUSEQ; }
<DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "*="               { yybegin(YYINITIAL); return JSTokenTypes.MULTEQ; }
<DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "/="               { yybegin(YYINITIAL); return JSTokenTypes.DIVEQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "%="    { yybegin(YYINITIAL); return JSTokenTypes.PERCEQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "&="    { yybegin(YYINITIAL); return JSTokenTypes.ANDEQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "|="    { yybegin(YYINITIAL); return JSTokenTypes.OREQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "^="    { yybegin(YYINITIAL); return JSTokenTypes.XOREQ; }
<DIV_OR_GT> "<="                                          { yybegin(YYINITIAL); return JSTokenTypes.LE; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> ">="    { yybegin(YYINITIAL); return JSTokenTypes.GE; }

// === Single-char operators ===
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "("     { yybegin(YYINITIAL); return JSTokenTypes.LPAR; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> ")"     { yybegin(DIV_OR_GT); return JSTokenTypes.RPAR; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "{"     { yybegin(YYINITIAL); return JSTokenTypes.LBRACE; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "}"     { yybegin(YYINITIAL); return JSTokenTypes.RBRACE; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "["     { yybegin(YYINITIAL); return JSTokenTypes.LBRACKET; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "]"     { yybegin(DIV_OR_GT); return JSTokenTypes.RBRACKET; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> ";"     { yybegin(YYINITIAL); return JSTokenTypes.SEMICOLON; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> ","     { yybegin(YYINITIAL); return JSTokenTypes.COMMA; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "."     { return JSTokenTypes.DOT; }
<DIV_OR_GT> "<"                                           { yybegin(YYINITIAL); return JSTokenTypes.LT; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> ">"     { yybegin(YYINITIAL); return JSTokenTypes.GT; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "="     { yybegin(YYINITIAL); return JSTokenTypes.EQ; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "!"     { yybegin(YYINITIAL); return JSTokenTypes.EXCL; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "~"     { yybegin(YYINITIAL); return JSTokenTypes.TILDE; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "?"     { yybegin(YYINITIAL); return JSTokenTypes.QUEST; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> ":"     { yybegin(YYINITIAL); return JSTokenTypes.COLON; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "+"     { yybegin(YYINITIAL); return JSTokenTypes.PLUS; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "-"     { yybegin(YYINITIAL); return JSTokenTypes.MINUS; }
<DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "*"                { yybegin(YYINITIAL); return JSTokenTypes.MULT; }
<DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "/"                { yybegin(YYINITIAL); return JSTokenTypes.DIV; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "^"     { yybegin(YYINITIAL); return JSTokenTypes.XOR; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "%"     { yybegin(YYINITIAL); return JSTokenTypes.PERC; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "&"     { yybegin(YYINITIAL); return JSTokenTypes.AND; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "|"     { yybegin(YYINITIAL); return JSTokenTypes.OR; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "@"     { return JSTokenTypes.AT; }
<YYINITIAL,DIV_OR_GT,TEMPLATE_STRING_EXPRESSION> "#"     { return TypeScriptTokens.HASH; }

// === Regexp (only in YYINITIAL - after operator, not after value) ===
<YYINITIAL> {REGEXP_LITERAL}  { yybegin(DIV_OR_GT); return JSTokenTypes.REGEXP_LITERAL; }

// === Fallback ===
[^] { yybegin(YYINITIAL); return TokenType.BAD_CHARACTER; }
