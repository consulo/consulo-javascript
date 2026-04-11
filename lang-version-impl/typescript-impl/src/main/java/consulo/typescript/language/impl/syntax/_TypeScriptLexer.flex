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

%%

<YYINITIAL,DIV_OR_GT> {WHITE_SPACE_CHAR}+ { return JSTokenTypes.WHITE_SPACE; }

<YYINITIAL,DIV_OR_GT> {C_STYLE_COMMENT}     { return JSTokenTypes.C_STYLE_COMMENT; }
<YYINITIAL,DIV_OR_GT> {END_OF_LINE_COMMENT}  { return JSTokenTypes.END_OF_LINE_COMMENT; }
<YYINITIAL,DIV_OR_GT> {DOC_COMMENT}          { return JSTokenTypes.DOC_COMMENT; }

// === Numeric literals ===
<YYINITIAL,DIV_OR_GT> {BIGINT_LITERAL}    { yybegin(DIV_OR_GT); return JSTokenTypes.NUMERIC_LITERAL; }
<YYINITIAL,DIV_OR_GT> {INTEGER_LITERAL}   { yybegin(DIV_OR_GT); return JSTokenTypes.NUMERIC_LITERAL; }
<YYINITIAL,DIV_OR_GT> {FLOAT_LITERAL}     { yybegin(DIV_OR_GT); return JSTokenTypes.NUMERIC_LITERAL; }

// === String literals ===
<YYINITIAL,DIV_OR_GT> {QUOTED_LITERAL}        { yybegin(DIV_OR_GT); return JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL; }
<YYINITIAL,DIV_OR_GT> {DOUBLE_QUOTED_LITERAL} { yybegin(DIV_OR_GT); return JSTokenTypes.STRING_LITERAL; }

// === Template literals ===
// Opening backtick — enters template string scanning mode
// Token type: TEMPLATE_HEAD (opening delimiter of template literal)
<YYINITIAL,DIV_OR_GT> "`" {
    yybegin(TEMPLATE_STRING);
    return TypeScriptTokens.TEMPLATE_HEAD;
}

// Template string state: scans text content between template delimiters
<TEMPLATE_STRING> {
    // Interpolation start: ${ — returns TEMPLATE_HEAD and enters expression mode
    "${"  {
        templateDepth++;
        templateBraceCount.push(0);
        yybegin(YYINITIAL);
        return TypeScriptTokens.TEMPLATE_HEAD;
    }
    // Closing backtick — returns TEMPLATE_TAIL and exits template
    "`"   {
        yybegin(DIV_OR_GT);
        return TypeScriptTokens.TEMPLATE_TAIL;
    }
    // Text content: any chars except backtick, dollar, backslash
    [^`$\\]+      { return TypeScriptTokens.NO_SUBSTITUTION_TEMPLATE; }
    // Escape sequence: backslash + any char (including newline for line continuation)
    "\\" [^]      { return TypeScriptTokens.NO_SUBSTITUTION_TEMPLATE; }
    // Dollar followed by non-brace, non-backtick, non-backslash char (not interpolation start)
    "$" [^{`\\]   { return TypeScriptTokens.NO_SUBSTITUTION_TEMPLATE; }
    // Dollar followed by escape sequence
    "$" "\\" [^]  { return TypeScriptTokens.NO_SUBSTITUTION_TEMPLATE; }
    // Standalone dollar (before ` or ${, or at EOF)
    "$"           { return TypeScriptTokens.NO_SUBSTITUTION_TEMPLATE; }
}

// === Keywords (true keywords - always keyword tokens) ===
<YYINITIAL,DIV_OR_GT> "break"       { yybegin(YYINITIAL); return JSTokenTypes.BREAK_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "case"        { yybegin(YYINITIAL); return JSTokenTypes.CASE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "catch"       { yybegin(YYINITIAL); return JSTokenTypes.CATCH_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "class"       { yybegin(YYINITIAL); return JSTokenTypes.CLASS_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "const"       { yybegin(YYINITIAL); return JSTokenTypes.CONST_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "continue"    { yybegin(YYINITIAL); return JSTokenTypes.CONTINUE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "debugger"    { yybegin(YYINITIAL); return TypeScriptTokens.DEBUGGER_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "default"     { yybegin(YYINITIAL); return JSTokenTypes.DEFAULT_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "delete"      { yybegin(YYINITIAL); return JSTokenTypes.DELETE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "do"          { yybegin(YYINITIAL); return JSTokenTypes.DO_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "else"        { yybegin(YYINITIAL); return JSTokenTypes.ELSE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "enum"        { yybegin(YYINITIAL); return JSTokenTypes.ENUM_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "export"      { yybegin(YYINITIAL); return JSTokenTypes.EXPORT_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "extends"     { yybegin(YYINITIAL); return JSTokenTypes.EXTENDS_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "false"       { yybegin(DIV_OR_GT); return JSTokenTypes.FALSE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "finally"     { yybegin(YYINITIAL); return JSTokenTypes.FINALLY_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "for"         { yybegin(YYINITIAL); return JSTokenTypes.FOR_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "function"    { yybegin(YYINITIAL); return JSTokenTypes.FUNCTION_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "if"          { yybegin(YYINITIAL); return JSTokenTypes.IF_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "import"      { yybegin(YYINITIAL); return JSTokenTypes.IMPORT_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "in"          { yybegin(YYINITIAL); return JSTokenTypes.IN_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "instanceof"  { yybegin(YYINITIAL); return JSTokenTypes.INSTANCEOF_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "let"         { yybegin(YYINITIAL); return JSTokenTypes.LET_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "new"         { yybegin(YYINITIAL); return JSTokenTypes.NEW_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "null"        { yybegin(DIV_OR_GT); return JSTokenTypes.NULL_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "return"      { yybegin(YYINITIAL); return JSTokenTypes.RETURN_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "super"       { yybegin(YYINITIAL); return JSTokenTypes.SUPER_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "switch"      { yybegin(YYINITIAL); return JSTokenTypes.SWITCH_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "this"        { yybegin(DIV_OR_GT); return JSTokenTypes.THIS_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "throw"       { yybegin(YYINITIAL); return JSTokenTypes.THROW_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "true"        { yybegin(DIV_OR_GT); return JSTokenTypes.TRUE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "try"         { yybegin(YYINITIAL); return JSTokenTypes.TRY_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "typeof"      { yybegin(YYINITIAL); return JSTokenTypes.TYPEOF_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "var"         { yybegin(YYINITIAL); return JSTokenTypes.VAR_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "void"        { yybegin(YYINITIAL); return JSTokenTypes.VOID_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "while"       { yybegin(YYINITIAL); return JSTokenTypes.WHILE_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "with"        { yybegin(YYINITIAL); return JSTokenTypes.WITH_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "yield"       { yybegin(YYINITIAL); return JSTokenTypes.YIELD_KEYWORD; }
<YYINITIAL,DIV_OR_GT> "await"       { yybegin(YYINITIAL); return TypeScriptTokens.AWAIT_KEYWORD; }

// Contextual keywords emitted as IDENTIFIER - parser resolves them:
// abstract, as, asserts, async, constructor, declare, defer, from, get, global,
// implements, infer, interface, is, keyof, module, namespace, never, of,
// override, private, protected, public, readonly, require, satisfies, set,
// static, type, undefined, unique, unknown, using

// === Identifiers ===
<YYINITIAL,DIV_OR_GT> {IDENTIFIER}  { yybegin(DIV_OR_GT); return JSTokenTypes.IDENTIFIER; }

// === Multi-char operators (order matters: longer first) ===
<YYINITIAL,DIV_OR_GT> "..."   { yybegin(YYINITIAL); return JSTokenTypes.DOT_DOT_DOT; }
<YYINITIAL,DIV_OR_GT> "==="   { yybegin(YYINITIAL); return JSTokenTypes.EQEQEQ; }
<YYINITIAL,DIV_OR_GT> "!=="   { yybegin(YYINITIAL); return JSTokenTypes.NEQEQ; }
<YYINITIAL,DIV_OR_GT> ">>>"   { yybegin(YYINITIAL); return JSTokenTypes.GTGTGT; }
<YYINITIAL,DIV_OR_GT> ">>>="  { yybegin(YYINITIAL); return JSTokenTypes.GTGTGTEQ; }
<YYINITIAL,DIV_OR_GT> ">>="   { yybegin(YYINITIAL); return JSTokenTypes.GTGTEQ; }
<YYINITIAL,DIV_OR_GT> "<<="   { yybegin(YYINITIAL); return JSTokenTypes.LTLTEQ; }
<YYINITIAL,DIV_OR_GT> "**="   { yybegin(YYINITIAL); return JSTokenTypes.MULT_MULT_EQ; }
<YYINITIAL,DIV_OR_GT> "||="   { yybegin(YYINITIAL); return JSTokenTypes.OR_OR_EQ; }
<YYINITIAL,DIV_OR_GT> "&&="   { yybegin(YYINITIAL); return JSTokenTypes.AND_AND_EQ; }
<YYINITIAL,DIV_OR_GT> "??="   { yybegin(YYINITIAL); return JSTokenTypes.QUEST_QUEST_EQ; }
<YYINITIAL,DIV_OR_GT> "=>"    { yybegin(YYINITIAL); return JSTokenTypes.DARROW; }
<YYINITIAL,DIV_OR_GT> "=="    { yybegin(YYINITIAL); return JSTokenTypes.EQEQ; }
<YYINITIAL,DIV_OR_GT> "!="    { yybegin(YYINITIAL); return JSTokenTypes.NE; }
<YYINITIAL,DIV_OR_GT> "++"    { return JSTokenTypes.PLUSPLUS; }
<YYINITIAL,DIV_OR_GT> "--"    { return JSTokenTypes.MINUSMINUS; }
<YYINITIAL,DIV_OR_GT> "**"    { yybegin(YYINITIAL); return JSTokenTypes.MULTMULT; }
<YYINITIAL,DIV_OR_GT> "<<"    { yybegin(YYINITIAL); return JSTokenTypes.LTLT; }
<YYINITIAL,DIV_OR_GT> ">>"    { yybegin(YYINITIAL); return JSTokenTypes.GTGT; }
<YYINITIAL,DIV_OR_GT> "&&"    { yybegin(YYINITIAL); return JSTokenTypes.ANDAND; }
<YYINITIAL,DIV_OR_GT> "||"    { yybegin(YYINITIAL); return JSTokenTypes.OROR; }
<YYINITIAL,DIV_OR_GT> "??"    { yybegin(YYINITIAL); return TypeScriptTokens.QUEST_QUEST; }
<YYINITIAL,DIV_OR_GT> "?."    { yybegin(YYINITIAL); return JSTokenTypes.QUEST_DOT; }
<YYINITIAL,DIV_OR_GT> "+="    { yybegin(YYINITIAL); return JSTokenTypes.PLUSEQ; }
<YYINITIAL,DIV_OR_GT> "-="    { yybegin(YYINITIAL); return JSTokenTypes.MINUSEQ; }
<DIV_OR_GT> "*="               { yybegin(YYINITIAL); return JSTokenTypes.MULTEQ; }
<DIV_OR_GT> "/="               { yybegin(YYINITIAL); return JSTokenTypes.DIVEQ; }
<YYINITIAL,DIV_OR_GT> "%="    { yybegin(YYINITIAL); return JSTokenTypes.PERCEQ; }
<YYINITIAL,DIV_OR_GT> "&="    { yybegin(YYINITIAL); return JSTokenTypes.ANDEQ; }
<YYINITIAL,DIV_OR_GT> "|="    { yybegin(YYINITIAL); return JSTokenTypes.OREQ; }
<YYINITIAL,DIV_OR_GT> "^="    { yybegin(YYINITIAL); return JSTokenTypes.XOREQ; }
<YYINITIAL,DIV_OR_GT> "<="    { yybegin(YYINITIAL); return JSTokenTypes.LE; }
<YYINITIAL,DIV_OR_GT> ">="    { yybegin(YYINITIAL); return JSTokenTypes.GE; }

// === Single-char operators ===
<YYINITIAL,DIV_OR_GT> "("     { yybegin(YYINITIAL); return JSTokenTypes.LPAR; }
<YYINITIAL,DIV_OR_GT> ")"     { yybegin(DIV_OR_GT); return JSTokenTypes.RPAR; }
// Brace handling with template depth tracking
<YYINITIAL,DIV_OR_GT> "{"     {
    if (templateDepth > 0) {
        templateBraceCount.push(templateBraceCount.pop() + 1);
    }
    yybegin(YYINITIAL);
    return JSTokenTypes.LBRACE;
}
<YYINITIAL,DIV_OR_GT> "}"     {
    if (templateDepth > 0) {
        int count = templateBraceCount.peek();
        if (count > 0) {
            // Nested brace inside template expression — just decrement count
            templateBraceCount.push(templateBraceCount.pop() - 1);
            yybegin(YYINITIAL);
            return JSTokenTypes.RBRACE;
        } else {
            // Closing brace of template interpolation — return to template text scanning
            templateDepth--;
            templateBraceCount.pop();
            yybegin(TEMPLATE_STRING);
            return TypeScriptTokens.TEMPLATE_MIDDLE;
        }
    }
    yybegin(YYINITIAL);
    return JSTokenTypes.RBRACE;
}
<YYINITIAL,DIV_OR_GT> "["     { yybegin(YYINITIAL); return JSTokenTypes.LBRACKET; }
<YYINITIAL,DIV_OR_GT> "]"     { yybegin(DIV_OR_GT); return JSTokenTypes.RBRACKET; }
<YYINITIAL,DIV_OR_GT> ";"     { yybegin(YYINITIAL); return JSTokenTypes.SEMICOLON; }
<YYINITIAL,DIV_OR_GT> ","     { yybegin(YYINITIAL); return JSTokenTypes.COMMA; }
<YYINITIAL,DIV_OR_GT> "."     { return JSTokenTypes.DOT; }
<YYINITIAL,DIV_OR_GT> "<"     { yybegin(YYINITIAL); return JSTokenTypes.LT; }
<YYINITIAL,DIV_OR_GT> ">"     { yybegin(YYINITIAL); return JSTokenTypes.GT; }
<YYINITIAL,DIV_OR_GT> "="     { yybegin(YYINITIAL); return JSTokenTypes.EQ; }
<YYINITIAL,DIV_OR_GT> "!"     { yybegin(YYINITIAL); return JSTokenTypes.EXCL; }
<YYINITIAL,DIV_OR_GT> "~"     { yybegin(YYINITIAL); return JSTokenTypes.TILDE; }
<YYINITIAL,DIV_OR_GT> "?"     { yybegin(YYINITIAL); return JSTokenTypes.QUEST; }
<YYINITIAL,DIV_OR_GT> ":"     { yybegin(YYINITIAL); return JSTokenTypes.COLON; }
<YYINITIAL,DIV_OR_GT> "+"     { yybegin(YYINITIAL); return JSTokenTypes.PLUS; }
<YYINITIAL,DIV_OR_GT> "-"     { yybegin(YYINITIAL); return JSTokenTypes.MINUS; }
<YYINITIAL,DIV_OR_GT> "*"     { yybegin(YYINITIAL); return JSTokenTypes.MULT; }
<DIV_OR_GT> "/"                { yybegin(YYINITIAL); return JSTokenTypes.DIV; }
<YYINITIAL,DIV_OR_GT> "^"     { yybegin(YYINITIAL); return JSTokenTypes.XOR; }
<YYINITIAL,DIV_OR_GT> "%"     { yybegin(YYINITIAL); return JSTokenTypes.PERC; }
<YYINITIAL,DIV_OR_GT> "&"     { yybegin(YYINITIAL); return JSTokenTypes.AND; }
<YYINITIAL,DIV_OR_GT> "|"     { yybegin(YYINITIAL); return JSTokenTypes.OR; }
<YYINITIAL,DIV_OR_GT> "@"     { return JSTokenTypes.AT; }
<YYINITIAL,DIV_OR_GT> "#"     { return TypeScriptTokens.HASH; }

// === Regexp (only in YYINITIAL - after operator, not after value) ===
<YYINITIAL> {REGEXP_LITERAL}  { yybegin(DIV_OR_GT); return JSTokenTypes.REGEXP_LITERAL; }

// === Fallback ===
[^] { yybegin(YYINITIAL); return TokenType.BAD_CHARACTER; }
