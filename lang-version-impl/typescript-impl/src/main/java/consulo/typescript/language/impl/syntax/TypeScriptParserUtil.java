package consulo.typescript.language.impl.syntax;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.language.ast.IElementType;
import consulo.language.impl.parser.GeneratedParserUtilBase;
import consulo.language.parser.PsiBuilder;
import consulo.typescript.language.psi.TypeScriptTypes;

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

        // Case 2: Parser expects IDENTIFIER - also match remapped contextual keywords
        if (token == JSTokenTypes.IDENTIFIER) {
            IElementType current = b.getTokenType();
            if (current == JSTokenTypes.IDENTIFIER || CONTEXTUAL_KEYWORD_SET.contains(current)) {
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
