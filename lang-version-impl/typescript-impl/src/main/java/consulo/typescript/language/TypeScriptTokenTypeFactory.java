package consulo.typescript.language;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.lang.JavaScriptContextKeywordElementType;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenType;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Token type factory for Grammar-Kit generated parser.
 * Maps token names to existing JSTokenTypes/TypeScriptTokens instances
 * so the parser uses the same token type objects as the lexer.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public final class TypeScriptTokenTypeFactory {
    private static final Map<String, IElementType> TOKEN_MAP = new HashMap<>();

    static {
        // Whitespace & comments
        TOKEN_MAP.put("WHITE_SPACE", TokenType.WHITE_SPACE);
        TOKEN_MAP.put("END_OF_LINE_COMMENT", JSTokenTypes.END_OF_LINE_COMMENT);
        TOKEN_MAP.put("C_STYLE_COMMENT", JSTokenTypes.C_STYLE_COMMENT);
        TOKEN_MAP.put("DOC_COMMENT", JSTokenTypes.DOC_COMMENT);

        // Identifiers & literals
        TOKEN_MAP.put("IDENTIFIER", JSTokenTypes.IDENTIFIER);
        TOKEN_MAP.put("NUMERIC_LITERAL", JSTokenTypes.NUMERIC_LITERAL);
        TOKEN_MAP.put("STRING_LITERAL", JSTokenTypes.STRING_LITERAL);
        TOKEN_MAP.put("REGEXP_LITERAL", JSTokenTypes.REGEXP_LITERAL);

        // Template literals (TypeScript-specific tokens)
        TOKEN_MAP.put("NO_SUBSTITUTION_TEMPLATE", TypeScriptTokens.NO_SUBSTITUTION_TEMPLATE);
        TOKEN_MAP.put("TEMPLATE_HEAD", TypeScriptTokens.TEMPLATE_HEAD);
        TOKEN_MAP.put("TEMPLATE_MIDDLE", TypeScriptTokens.TEMPLATE_MIDDLE);
        TOKEN_MAP.put("TEMPLATE_TAIL", TypeScriptTokens.TEMPLATE_TAIL);

        // JS Keywords (by their string value - Grammar-Kit passes the 'xxx' value)
        TOKEN_MAP.put("break", JSTokenTypes.BREAK_KEYWORD);
        TOKEN_MAP.put("case", JSTokenTypes.CASE_KEYWORD);
        TOKEN_MAP.put("catch", JSTokenTypes.CATCH_KEYWORD);
        TOKEN_MAP.put("const", JSTokenTypes.CONST_KEYWORD);
        TOKEN_MAP.put("continue", JSTokenTypes.CONTINUE_KEYWORD);
        TOKEN_MAP.put("default", JSTokenTypes.DEFAULT_KEYWORD);
        TOKEN_MAP.put("delete", JSTokenTypes.DELETE_KEYWORD);
        TOKEN_MAP.put("do", JSTokenTypes.DO_KEYWORD);
        TOKEN_MAP.put("else", JSTokenTypes.ELSE_KEYWORD);
        TOKEN_MAP.put("export", JSTokenTypes.EXPORT_KEYWORD);
        TOKEN_MAP.put("extends", JSTokenTypes.EXTENDS_KEYWORD);
        TOKEN_MAP.put("false", JSTokenTypes.FALSE_KEYWORD);
        TOKEN_MAP.put("finally", JSTokenTypes.FINALLY_KEYWORD);
        TOKEN_MAP.put("for", JSTokenTypes.FOR_KEYWORD);
        TOKEN_MAP.put("function", JSTokenTypes.FUNCTION_KEYWORD);
        TOKEN_MAP.put("if", JSTokenTypes.IF_KEYWORD);
        TOKEN_MAP.put("import", JSTokenTypes.IMPORT_KEYWORD);
        TOKEN_MAP.put("in", JSTokenTypes.IN_KEYWORD);
        TOKEN_MAP.put("instanceof", JSTokenTypes.INSTANCEOF_KEYWORD);
        TOKEN_MAP.put("let", JSTokenTypes.LET_KEYWORD);
        TOKEN_MAP.put("new", JSTokenTypes.NEW_KEYWORD);
        TOKEN_MAP.put("null", JSTokenTypes.NULL_KEYWORD);
        TOKEN_MAP.put("return", JSTokenTypes.RETURN_KEYWORD);
        TOKEN_MAP.put("super", JSTokenTypes.SUPER_KEYWORD);
        TOKEN_MAP.put("switch", JSTokenTypes.SWITCH_KEYWORD);
        TOKEN_MAP.put("this", JSTokenTypes.THIS_KEYWORD);
        TOKEN_MAP.put("throw", JSTokenTypes.THROW_KEYWORD);
        TOKEN_MAP.put("true", JSTokenTypes.TRUE_KEYWORD);
        TOKEN_MAP.put("try", JSTokenTypes.TRY_KEYWORD);
        TOKEN_MAP.put("typeof", JSTokenTypes.TYPEOF_KEYWORD);
        TOKEN_MAP.put("var", JSTokenTypes.VAR_KEYWORD);
        TOKEN_MAP.put("void", JSTokenTypes.VOID_KEYWORD);
        TOKEN_MAP.put("while", JSTokenTypes.WHILE_KEYWORD);
        TOKEN_MAP.put("with", JSTokenTypes.WITH_KEYWORD);
        TOKEN_MAP.put("yield", JSTokenTypes.YIELD_KEYWORD);
        TOKEN_MAP.put("class", JSTokenTypes.CLASS_KEYWORD);
        TOKEN_MAP.put("enum", JSTokenTypes.ENUM_KEYWORD);

        // TS-specific hard keywords (lexer emits these as dedicated tokens)
        TOKEN_MAP.put("await", TypeScriptTokens.AWAIT_KEYWORD);
        TOKEN_MAP.put("debugger", TypeScriptTokens.DEBUGGER_KEYWORD);

        // Shared contextual keywords (reuse existing JSTokenTypes instances which are JavaScriptContextKeywordElementType)
        TOKEN_MAP.put("FROM_KEYWORD", JSTokenTypes.FROM_KEYWORD);
        TOKEN_MAP.put("STATIC_KEYWORD", JSTokenTypes.STATIC_KEYWORD);
        TOKEN_MAP.put("AS_KEYWORD", JSTokenTypes.AS_KEYWORD);
        TOKEN_MAP.put("GET_KEYWORD", JSTokenTypes.GET_KEYWORD);
        TOKEN_MAP.put("SET_KEYWORD", JSTokenTypes.SET_KEYWORD);
        TOKEN_MAP.put("OF_KEYWORD", JSTokenTypes.OF_KEYWORD);

        // Punctuators
        TOKEN_MAP.put("{", JSTokenTypes.LBRACE);
        TOKEN_MAP.put("}", JSTokenTypes.RBRACE);
        TOKEN_MAP.put("(", JSTokenTypes.LPAR);
        TOKEN_MAP.put(")", JSTokenTypes.RPAR);
        TOKEN_MAP.put("[", JSTokenTypes.LBRACKET);
        TOKEN_MAP.put("]", JSTokenTypes.RBRACKET);
        TOKEN_MAP.put(".", JSTokenTypes.DOT);
        TOKEN_MAP.put("...", JSTokenTypes.DOT_DOT_DOT);
        TOKEN_MAP.put(";", JSTokenTypes.SEMICOLON);
        TOKEN_MAP.put(",", JSTokenTypes.COMMA);
        TOKEN_MAP.put("<", JSTokenTypes.LT);
        TOKEN_MAP.put(">", JSTokenTypes.GT);
        TOKEN_MAP.put("<=", JSTokenTypes.LE);
        TOKEN_MAP.put(">=", JSTokenTypes.GE);
        TOKEN_MAP.put("==", JSTokenTypes.EQEQ);
        TOKEN_MAP.put("!=", JSTokenTypes.NE);
        TOKEN_MAP.put("===", JSTokenTypes.EQEQEQ);
        TOKEN_MAP.put("!==", JSTokenTypes.NEQEQ);
        TOKEN_MAP.put("+", JSTokenTypes.PLUS);
        TOKEN_MAP.put("-", JSTokenTypes.MINUS);
        TOKEN_MAP.put("*", JSTokenTypes.MULT);
        TOKEN_MAP.put("**", JSTokenTypes.MULTMULT);
        TOKEN_MAP.put("%", JSTokenTypes.PERC);
        TOKEN_MAP.put("++", JSTokenTypes.PLUSPLUS);
        TOKEN_MAP.put("--", JSTokenTypes.MINUSMINUS);
        TOKEN_MAP.put("<<", JSTokenTypes.LTLT);
        TOKEN_MAP.put(">>", JSTokenTypes.GTGT);
        TOKEN_MAP.put(">>>", JSTokenTypes.GTGTGT);
        TOKEN_MAP.put("&", JSTokenTypes.AND);
        TOKEN_MAP.put("|", JSTokenTypes.OR);
        TOKEN_MAP.put("^", JSTokenTypes.XOR);
        TOKEN_MAP.put("!", JSTokenTypes.EXCL);
        TOKEN_MAP.put("~", JSTokenTypes.TILDE);
        TOKEN_MAP.put("&&", JSTokenTypes.ANDAND);
        TOKEN_MAP.put("||", JSTokenTypes.OROR);
        TOKEN_MAP.put("?", JSTokenTypes.QUEST);
        TOKEN_MAP.put("?.", JSTokenTypes.QUEST_DOT);
        TOKEN_MAP.put("??", TypeScriptTokens.QUEST_QUEST);
        TOKEN_MAP.put(":", JSTokenTypes.COLON);
        TOKEN_MAP.put("=", JSTokenTypes.EQ);
        TOKEN_MAP.put("+=", JSTokenTypes.PLUSEQ);
        TOKEN_MAP.put("-=", JSTokenTypes.MINUSEQ);
        TOKEN_MAP.put("*=", JSTokenTypes.MULTEQ);
        TOKEN_MAP.put("**=", JSTokenTypes.MULT_MULT_EQ);
        TOKEN_MAP.put("%=", JSTokenTypes.PERCEQ);
        TOKEN_MAP.put("<<=", JSTokenTypes.LTLTEQ);
        TOKEN_MAP.put(">>=", JSTokenTypes.GTGTEQ);
        TOKEN_MAP.put(">>>=", JSTokenTypes.GTGTGTEQ);
        TOKEN_MAP.put("&=", JSTokenTypes.ANDEQ);
        TOKEN_MAP.put("|=", JSTokenTypes.OREQ);
        TOKEN_MAP.put("^=", JSTokenTypes.XOREQ);
        TOKEN_MAP.put("/=", JSTokenTypes.DIVEQ);
        TOKEN_MAP.put("||=", JSTokenTypes.OR_OR_EQ);
        TOKEN_MAP.put("&&=", JSTokenTypes.AND_AND_EQ);
        TOKEN_MAP.put("??=", JSTokenTypes.QUEST_QUEST_EQ);
        TOKEN_MAP.put("=>", JSTokenTypes.DARROW);
        TOKEN_MAP.put("/", JSTokenTypes.DIV);
        TOKEN_MAP.put("@", JSTokenTypes.AT);
        TOKEN_MAP.put("#", TypeScriptTokens.HASH);
    }

    public static IElementType getTokenType(String name) {
        IElementType existing = TOKEN_MAP.get(name);
        if (existing != null) {
            return existing;
        }
        // TS-specific contextual keywords — create JavaScriptContextKeywordElementType
        // so JavaScriptHighlightVisitor highlights them as keywords
        if (name.endsWith("_KEYWORD")) {
            String keyword = name.substring(0, name.length() - "_KEYWORD".length()).toLowerCase(Locale.ROOT);
            return new JavaScriptContextKeywordElementType(name, keyword);
        }
        return new TypeScriptTokenType(name);
    }
}
