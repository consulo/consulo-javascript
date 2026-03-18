package consulo.typescript.completion;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.Language;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.editor.completion.CompletionContributor;
import consulo.language.editor.completion.CompletionParameters;
import consulo.language.editor.completion.CompletionResultSet;
import consulo.language.editor.completion.lookup.LookupElementBuilder;
import consulo.language.impl.ast.TreeUtil;
import consulo.language.impl.parser.GeneratedParserUtilBase;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiFileFactory;
import consulo.typescript.language.TypeScriptLanguage;
import consulo.typescript.language.TypeScriptTokens;
import consulo.typescript.language.psi.TypeScriptTypes;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * Grammar-based keyword completion for TypeScript.
 * Uses {@link GeneratedParserUtilBase.CompletionState} to collect expected keyword tokens
 * from the BNF-generated parser at the completion offset.
 *
 * @author VISTALL
 * @since 2026-03-18
 */
@ExtensionImpl
public class TypeScriptKeywordCompletionContributor extends CompletionContributor {
    private static final Map<IElementType, String> KEYWORD_MAP = new LinkedHashMap<>();

    private static final TokenSet KEYWORDS;

    static {
        // JS hard keywords
        KEYWORD_MAP.put(JSTokenTypes.BREAK_KEYWORD, "break");
        KEYWORD_MAP.put(JSTokenTypes.CASE_KEYWORD, "case");
        KEYWORD_MAP.put(JSTokenTypes.CATCH_KEYWORD, "catch");
        KEYWORD_MAP.put(JSTokenTypes.CONST_KEYWORD, "const");
        KEYWORD_MAP.put(JSTokenTypes.CONTINUE_KEYWORD, "continue");
        KEYWORD_MAP.put(JSTokenTypes.DEFAULT_KEYWORD, "default");
        KEYWORD_MAP.put(JSTokenTypes.DELETE_KEYWORD, "delete");
        KEYWORD_MAP.put(JSTokenTypes.DO_KEYWORD, "do");
        KEYWORD_MAP.put(JSTokenTypes.ELSE_KEYWORD, "else");
        KEYWORD_MAP.put(JSTokenTypes.EXPORT_KEYWORD, "export");
        KEYWORD_MAP.put(JSTokenTypes.EXTENDS_KEYWORD, "extends");
        KEYWORD_MAP.put(JSTokenTypes.FALSE_KEYWORD, "false");
        KEYWORD_MAP.put(JSTokenTypes.FINALLY_KEYWORD, "finally");
        KEYWORD_MAP.put(JSTokenTypes.FOR_KEYWORD, "for");
        KEYWORD_MAP.put(JSTokenTypes.FUNCTION_KEYWORD, "function");
        KEYWORD_MAP.put(JSTokenTypes.IF_KEYWORD, "if");
        KEYWORD_MAP.put(JSTokenTypes.IMPORT_KEYWORD, "import");
        KEYWORD_MAP.put(JSTokenTypes.IN_KEYWORD, "in");
        KEYWORD_MAP.put(JSTokenTypes.INSTANCEOF_KEYWORD, "instanceof");
        KEYWORD_MAP.put(JSTokenTypes.LET_KEYWORD, "let");
        KEYWORD_MAP.put(JSTokenTypes.NEW_KEYWORD, "new");
        KEYWORD_MAP.put(JSTokenTypes.NULL_KEYWORD, "null");
        KEYWORD_MAP.put(JSTokenTypes.RETURN_KEYWORD, "return");
        KEYWORD_MAP.put(JSTokenTypes.SUPER_KEYWORD, "super");
        KEYWORD_MAP.put(JSTokenTypes.SWITCH_KEYWORD, "switch");
        KEYWORD_MAP.put(JSTokenTypes.THIS_KEYWORD, "this");
        KEYWORD_MAP.put(JSTokenTypes.THROW_KEYWORD, "throw");
        KEYWORD_MAP.put(JSTokenTypes.TRUE_KEYWORD, "true");
        KEYWORD_MAP.put(JSTokenTypes.TRY_KEYWORD, "try");
        KEYWORD_MAP.put(JSTokenTypes.TYPEOF_KEYWORD, "typeof");
        KEYWORD_MAP.put(JSTokenTypes.VAR_KEYWORD, "var");
        KEYWORD_MAP.put(JSTokenTypes.VOID_KEYWORD, "void");
        KEYWORD_MAP.put(JSTokenTypes.WHILE_KEYWORD, "while");
        KEYWORD_MAP.put(JSTokenTypes.WITH_KEYWORD, "with");
        KEYWORD_MAP.put(JSTokenTypes.YIELD_KEYWORD, "yield");
        KEYWORD_MAP.put(JSTokenTypes.CLASS_KEYWORD, "class");
        KEYWORD_MAP.put(JSTokenTypes.ENUM_KEYWORD, "enum");

        // TS hard keywords
        KEYWORD_MAP.put(TypeScriptTokens.AWAIT_KEYWORD, "await");
        KEYWORD_MAP.put(TypeScriptTokens.DEBUGGER_KEYWORD, "debugger");

        // TS contextual keywords
        KEYWORD_MAP.put(TypeScriptTypes.INTERFACE_KEYWORD, "interface");
        KEYWORD_MAP.put(TypeScriptTypes.TYPE_KEYWORD, "type");
        KEYWORD_MAP.put(TypeScriptTypes.NAMESPACE_KEYWORD, "namespace");
        KEYWORD_MAP.put(TypeScriptTypes.MODULE_KEYWORD, "module");
        KEYWORD_MAP.put(TypeScriptTypes.DECLARE_KEYWORD, "declare");
        KEYWORD_MAP.put(TypeScriptTypes.ABSTRACT_KEYWORD, "abstract");
        KEYWORD_MAP.put(TypeScriptTypes.ASYNC_KEYWORD, "async");
        KEYWORD_MAP.put(TypeScriptTypes.FROM_KEYWORD, "from");
        KEYWORD_MAP.put(TypeScriptTypes.AS_KEYWORD, "as");
        KEYWORD_MAP.put(TypeScriptTypes.SATISFIES_KEYWORD, "satisfies");
        KEYWORD_MAP.put(TypeScriptTypes.KEYOF_KEYWORD, "keyof");
        KEYWORD_MAP.put(TypeScriptTypes.INFER_KEYWORD, "infer");
        KEYWORD_MAP.put(TypeScriptTypes.READONLY_KEYWORD, "readonly");
        KEYWORD_MAP.put(TypeScriptTypes.GET_KEYWORD, "get");
        KEYWORD_MAP.put(TypeScriptTypes.SET_KEYWORD, "set");
        KEYWORD_MAP.put(TypeScriptTypes.OF_KEYWORD, "of");
        KEYWORD_MAP.put(TypeScriptTypes.CONSTRUCTOR_KEYWORD, "constructor");
        KEYWORD_MAP.put(TypeScriptTypes.GLOBAL_KEYWORD, "global");
        KEYWORD_MAP.put(TypeScriptTypes.OVERRIDE_KEYWORD, "override");
        KEYWORD_MAP.put(TypeScriptTypes.PUBLIC_KEYWORD, "public");
        KEYWORD_MAP.put(TypeScriptTypes.PRIVATE_KEYWORD, "private");
        KEYWORD_MAP.put(TypeScriptTypes.PROTECTED_KEYWORD, "protected");
        KEYWORD_MAP.put(TypeScriptTypes.STATIC_KEYWORD, "static");
        KEYWORD_MAP.put(TypeScriptTypes.IMPLEMENTS_KEYWORD, "implements");

        KEYWORDS = TokenSet.create(KEYWORD_MAP.keySet().toArray(IElementType.EMPTY_ARRAY));
    }

    @Override
    public void fillCompletionVariants(@Nonnull CompletionParameters parameters, @Nonnull CompletionResultSet result) {
        PsiFile posFile = parameters.getOriginalFile();
        int completionOffset = parameters.getOffset();
        CharSequence text = posFile.getText();

        Set<IElementType> collected = new LinkedHashSet<>();

        GeneratedParserUtilBase.CompletionState state = new GeneratedParserUtilBase.CompletionState(completionOffset) {
            @Nullable
            @Override
            public String convertItem(Object o) {
                if (o instanceof IElementType && KEYWORDS.contains((IElementType) o)) {
                    collected.add((IElementType) o);
                }
                return null;
            }
        };

        PsiFile file = PsiFileFactory.getInstance(posFile.getProject())
            .createFileFromText("a.ts", TypeScriptLanguage.INSTANCE, text, true, false);
        file.putUserData(GeneratedParserUtilBase.COMPLETION_STATE_KEY, state);
        TreeUtil.ensureParsed(file.getNode());

        for (IElementType type : collected) {
            String keyword = KEYWORD_MAP.get(type);
            if (keyword != null) {
                result.addElement(LookupElementBuilder.create(keyword).bold());
            }
        }
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return TypeScriptLanguage.INSTANCE;
    }
}
