package consulo.typescript.language;

import consulo.language.ast.IElementType;

/**
 * TypeScript-specific token types that don't exist in JSTokenTypes.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public interface TypeScriptTokens {
    IElementType QUEST_QUEST = new TypeScriptTokenType("QUEST_QUEST"); // ??
    IElementType TEMPLATE_HEAD = new TypeScriptTokenType("TEMPLATE_HEAD");
    IElementType TEMPLATE_MIDDLE = new TypeScriptTokenType("TEMPLATE_MIDDLE");
    IElementType TEMPLATE_TAIL = new TypeScriptTokenType("TEMPLATE_TAIL");
    IElementType NO_SUBSTITUTION_TEMPLATE = new TypeScriptTokenType("NO_SUBSTITUTION_TEMPLATE");
    IElementType HASH = new TypeScriptTokenType("HASH"); // #
    IElementType AWAIT_KEYWORD = new TypeScriptTokenType("AWAIT_KEYWORD");
    IElementType DEBUGGER_KEYWORD = new TypeScriptTokenType("DEBUGGER_KEYWORD");
}
