package consulo.typescript.html;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.annotation.component.ExtensionImpl;
import consulo.html.language.HtmlScriptContentProvider;
import consulo.language.Language;
import consulo.language.ast.IElementType;
import consulo.language.lexer.Lexer;
import consulo.language.ast.TokenSet;
import consulo.language.lexer.MergingLexerAdapter;
import consulo.typescript.language.impl.syntax.TypeScriptLexer;
import consulo.typescript.language.TypeScriptLanguage;
import consulo.typescript.language.impl.TypeScriptEmbeddedElementTypes;
import org.jspecify.annotations.Nullable;

/**
 * TypeScript in a script tag ({@code <script lang="ts">} of Vue, {@code <script type="application/typescript">}):
 * without it the JavaScript provider of the base language would parse it.
 *
 * @since 2026-10-31
 */
@ExtensionImpl
public class TypeScriptHtmlScriptContentProvider implements HtmlScriptContentProvider {
    @Override
    public IElementType getScriptElementType() {
        return TypeScriptEmbeddedElementTypes.EMBEDDED_CONTENT;
    }

    @Nullable
    @Override
    public Lexer getHighlightingLexer() {
        return new MergingLexerAdapter(new TypeScriptLexer(), TokenSet.create(JSTokenTypes.STRING_TEMPLATE_PART));
    }

    @Override
    public Language getLanguage() {
        return TypeScriptLanguage.INSTANCE;
    }
}
