package consulo.typescript.language.impl.syntax;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenType;
import consulo.language.impl.parser.GeneratedParserUtilBase;
import consulo.language.parser.PsiBuilder;
import consulo.typescript.language.psi.TypeScriptTypes;
import consulo.util.lang.StringUtil;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Custom parser utility for TypeScript contextual keywords.
 * <p>
 * TypeScript contextual keywords (interface, type, namespace, declare, etc.)
 * are emitted as IDENTIFIER by the lexer. This util overrides consumeToken/nextTokenIs
 * so the parser can match them as keyword tokens when expected by grammar rules,
 * while still allowing them to be used as identifiers elsewhere.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptParserUtil extends GeneratedParserUtilBase {
    /**
     * Maps contextual keyword token types to their text representation.
     */
    private static final Map<IElementType, String> CONTEXTUAL_KEYWORDS = new HashMap<>();

    /**
     * Set of all contextual keyword token types (for fast IDENTIFIER fallback check).
     */
    private static final Set<IElementType> CONTEXTUAL_KEYWORD_SET = new HashSet<>();

    static {
        register(TypeScriptTypes.INTERFACE_KEYWORD, "interface");
        register(TypeScriptTypes.TYPE_KEYWORD, "type");
        register(TypeScriptTypes.NAMESPACE_KEYWORD, "namespace");
        register(TypeScriptTypes.MODULE_KEYWORD, "module");
        register(TypeScriptTypes.DECLARE_KEYWORD, "declare");
        register(TypeScriptTypes.ABSTRACT_KEYWORD, "abstract");
        register(TypeScriptTypes.ASYNC_KEYWORD, "async");
        register(TypeScriptTypes.FROM_KEYWORD, "from");
        register(TypeScriptTypes.AS_KEYWORD, "as");
        register(TypeScriptTypes.SATISFIES_KEYWORD, "satisfies");
        register(TypeScriptTypes.KEYOF_KEYWORD, "keyof");
        register(TypeScriptTypes.INFER_KEYWORD, "infer");
        register(TypeScriptTypes.READONLY_KEYWORD, "readonly");
        register(TypeScriptTypes.GET_KEYWORD, "get");
        register(TypeScriptTypes.SET_KEYWORD, "set");
        register(TypeScriptTypes.OF_KEYWORD, "of");
        register(TypeScriptTypes.CONSTRUCTOR_KEYWORD, "constructor");
        register(TypeScriptTypes.GLOBAL_KEYWORD, "global");
        register(TypeScriptTypes.OVERRIDE_KEYWORD, "override");
        register(TypeScriptTypes.PUBLIC_KEYWORD, "public");
        register(TypeScriptTypes.PRIVATE_KEYWORD, "private");
        register(TypeScriptTypes.PROTECTED_KEYWORD, "protected");
        register(TypeScriptTypes.STATIC_KEYWORD, "static");
        register(TypeScriptTypes.IMPLEMENTS_KEYWORD, "implements");
        register(TypeScriptTypes.IS_KEYWORD, "is");
        register(TypeScriptTypes.ASSERTS_KEYWORD, "asserts");
        register(TypeScriptTypes.UNIQUE_KEYWORD, "unique");
    }

    /**
     * Reserved words that can name a member ({@code map.delete(key)}, {@code iterator.return?()}). Not {@code new}:
     * {@code new (): T} is a construct signature.
     */
    private static final Set<IElementType> KEYWORD_NAMES = Set.of(
        JSTokenTypes.BREAK_KEYWORD, JSTokenTypes.CASE_KEYWORD, JSTokenTypes.CATCH_KEYWORD, JSTokenTypes.CLASS_KEYWORD,
        JSTokenTypes.CONST_KEYWORD, JSTokenTypes.CONTINUE_KEYWORD, TypeScriptTypes.DEBUGGER_KEYWORD, JSTokenTypes.DEFAULT_KEYWORD,
        JSTokenTypes.DELETE_KEYWORD, JSTokenTypes.DO_KEYWORD, JSTokenTypes.ELSE_KEYWORD, JSTokenTypes.ENUM_KEYWORD,
        JSTokenTypes.EXPORT_KEYWORD, JSTokenTypes.EXTENDS_KEYWORD, JSTokenTypes.FALSE_KEYWORD, JSTokenTypes.FINALLY_KEYWORD,
        JSTokenTypes.FOR_KEYWORD, JSTokenTypes.FUNCTION_KEYWORD, JSTokenTypes.IF_KEYWORD, JSTokenTypes.IMPORT_KEYWORD,
        JSTokenTypes.IN_KEYWORD, JSTokenTypes.INSTANCEOF_KEYWORD, JSTokenTypes.LET_KEYWORD, JSTokenTypes.NULL_KEYWORD,
        JSTokenTypes.RETURN_KEYWORD, JSTokenTypes.SUPER_KEYWORD, JSTokenTypes.SWITCH_KEYWORD, JSTokenTypes.THIS_KEYWORD,
        JSTokenTypes.THROW_KEYWORD, JSTokenTypes.TRUE_KEYWORD, JSTokenTypes.TRY_KEYWORD, JSTokenTypes.TYPEOF_KEYWORD,
        JSTokenTypes.VAR_KEYWORD, JSTokenTypes.VOID_KEYWORD, JSTokenTypes.WHILE_KEYWORD, JSTokenTypes.WITH_KEYWORD,
        JSTokenTypes.YIELD_KEYWORD, TypeScriptTypes.AWAIT_KEYWORD
    );

    // the lexer emits every '>' alone, so `Promise<Array<T>>` closes two type argument lists: the shift operators are
    // joined from adjacent tokens

    public static boolean greaterOrEqual(PsiBuilder b, int level) {
        return joinAdjacent(b, JSTokenTypes.GE, JSTokenTypes.GT, JSTokenTypes.EQ);
    }

    public static boolean shiftRight(PsiBuilder b, int level) {
        return joinAdjacent(b, JSTokenTypes.GTGT, JSTokenTypes.GT, JSTokenTypes.GT);
    }

    public static boolean shiftRightUnsigned(PsiBuilder b, int level) {
        return joinAdjacent(b, JSTokenTypes.GTGTGT, JSTokenTypes.GT, JSTokenTypes.GT, JSTokenTypes.GT);
    }

    public static boolean shiftRightAssign(PsiBuilder b, int level) {
        return joinAdjacent(b, JSTokenTypes.GTGTEQ, JSTokenTypes.GT, JSTokenTypes.GT, JSTokenTypes.EQ);
    }

    public static boolean shiftRightUnsignedAssign(PsiBuilder b, int level) {
        return joinAdjacent(b, JSTokenTypes.GTGTGTEQ, JSTokenTypes.GT, JSTokenTypes.GT, JSTokenTypes.GT, JSTokenTypes.EQ);
    }

    /**
     * Joins adjacent tokens (no whitespace between) into one, unless they are the start of a longer operator:
     * like Java's {@code ExpressionParser.getGtTokenType}, the longest of {@code >}, {@code >=}, {@code >>},
     * {@code >>=}, {@code >>>}, {@code >>>=} wins.
     */
    private static boolean joinAdjacent(PsiBuilder b, IElementType joined, IElementType... tokens) {
        if (b.getTokenType() != tokens[0]) {
            return false;
        }
        for (int i = 1; i < tokens.length; i++) {
            if (b.rawLookup(i) != tokens[i]) {
                return false;
            }
        }
        IElementType next = b.rawLookup(tokens.length);
        if (tokens[tokens.length - 1] == JSTokenTypes.GT && (next == JSTokenTypes.GT || next == JSTokenTypes.EQ)) {
            return false;
        }
        PsiBuilder.Marker marker = b.mark();
        for (int i = 0; i < tokens.length; i++) {
            b.advanceLexer();
        }
        marker.collapse(joined);
        return true;
    }

    /**
     * Recovery of an interface member: skips the tokens of a member that doesn't parse up to a ';', the end of the
     * body or the first token of a line (members are often separated by line breaks only).
     */
    public static boolean memberRecover(PsiBuilder b, int level) {
        IElementType type = b.getTokenType();
        if (type == null || type == JSTokenTypes.SEMICOLON || type == JSTokenTypes.RBRACE) {
            return false;
        }
        return !isAtLineStart(b);
    }

    private static boolean isAtLineStart(PsiBuilder b) {
        for (int i = -1; ; i--) {
            IElementType previous = b.rawLookup(i);
            if (previous == null) {
                return true;
            }
            if (!JSTokenTypes.COMMENTS.contains(previous) && previous != TokenType.WHITE_SPACE) {
                return false;
            }
            CharSequence text = b.getOriginalText().subSequence(b.rawTokenTypeStart(i), b.rawTokenTypeStart(i + 1));
            if (StringUtil.contains(text, "\n")) {
                return true;
            }
        }
    }

    /**
     * A reserved word used as a member name: remapped to IDENTIFIER, so the declaration has a name like any other.
     */
    public static boolean keywordAsName(PsiBuilder b, int level) {
        if (!KEYWORD_NAMES.contains(b.getTokenType())) {
            return false;
        }
        b.remapCurrentToken(JSTokenTypes.IDENTIFIER);
        b.advanceLexer();
        return true;
    }

    private static void register(IElementType token, String text) {
        CONTEXTUAL_KEYWORDS.put(token, text);
        CONTEXTUAL_KEYWORD_SET.add(token);
    }

    /**
     * Checks if the given token type is a contextual keyword.
     */
    private static boolean isContextualKeyword(IElementType token) {
        return CONTEXTUAL_KEYWORDS.containsKey(token);
    }

    /**
     * Checks if the current lexer token matches the expected contextual keyword.
     */
    private static boolean isContextualKeywordMatch(PsiBuilder b, IElementType token) {
        String expectedText = CONTEXTUAL_KEYWORDS.get(token);
        if (expectedText == null) {
            return false;
        }
        // Match IDENTIFIER with the right text, or already-remapped token
        IElementType current = b.getTokenType();
        if (current == token) {
            return true; // already remapped from a previous attempt
        }
        return current == JSTokenTypes.IDENTIFIER && expectedText.equals(b.getTokenText());
    }

    /**
     * Override consumeToken to handle contextual keywords and IDENTIFIER fallback.
     */
    public static boolean consumeToken(PsiBuilder b, IElementType token) {
        // Case 1: Parser expects a contextual keyword
        if (isContextualKeyword(token)) {
            if (isContextualKeywordMatch(b, token)) {
                b.remapCurrentToken(token);
                b.advanceLexer();
                return true;
            }
            return false;
        }

        // Case 2: Parser expects IDENTIFIER - also match remapped contextual keywords. A remap survives the rollback of
        // a failed alternative: the token becomes an IDENTIFIER again, so the declaration it names has a name
        if (token == JSTokenTypes.IDENTIFIER) {
            IElementType current = b.getTokenType();
            if (current == JSTokenTypes.IDENTIFIER || CONTEXTUAL_KEYWORD_SET.contains(current)) {
                if (current != JSTokenTypes.IDENTIFIER) {
                    b.remapCurrentToken(JSTokenTypes.IDENTIFIER);
                }
                b.advanceLexer();
                return true;
            }
            return false;
        }

        // Case 3: Regular token - delegate to base
        return GeneratedParserUtilBase.consumeToken(b, token);
    }

    /**
     * Override consumeTokens to ensure contextual keyword handling is used for each token.
     * The base class calls its own consumeToken via invokestatic, bypassing our override.
     */
    public static boolean consumeTokens(PsiBuilder b, int pin, IElementType... tokens) {
        boolean result = true;
        boolean pinned = false;
        for (int i = 0; i < tokens.length; i++) {
            if (pin > 0 && i == pin) {
                pinned = result;
            }
            if (result || pinned) {
                if (!consumeToken(b, tokens[i])) {
                    result = false;
                    if (!pinned) break;
                }
            }
        }
        return result || pinned;
    }

    /**
     * Override nextTokenIs for quick prediction with contextual keywords.
     */
    public static boolean nextTokenIs(PsiBuilder b, IElementType token) {
        // Case 1: Contextual keyword check
        if (isContextualKeyword(token)) {
            return isContextualKeywordMatch(b, token);
        }

        // Case 2: IDENTIFIER also matches remapped contextual keywords
        if (token == JSTokenTypes.IDENTIFIER) {
            IElementType current = b.getTokenType();
            return current == JSTokenTypes.IDENTIFIER || CONTEXTUAL_KEYWORD_SET.contains(current);
        }

        return GeneratedParserUtilBase.nextTokenIs(b, token);
    }

    /**
     * Override nextTokenIs with error name for multi-token prediction.
     */
    public static boolean nextTokenIs(PsiBuilder b, String frameName, IElementType... tokens) {
        IElementType current = b.getTokenType();
        for (IElementType token : tokens) {
            if (isContextualKeyword(token)) {
                if (isContextualKeywordMatch(b, token)) {
                    return true;
                }
            }
            else if (token == JSTokenTypes.IDENTIFIER) {
                if (current == JSTokenTypes.IDENTIFIER || CONTEXTUAL_KEYWORD_SET.contains(current)) {
                    return true;
                }
            }
            else if (current == token) {
                return true;
            }
        }
        return false;
    }
}
