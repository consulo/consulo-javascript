package consulo.javascript.impl.html;

import com.intellij.lang.javascript.JSElementTypes;
import consulo.annotation.component.ExtensionImpl;
import consulo.html.language.HtmlScriptContentProvider;
import consulo.javascript.lang.lexer.JavaScript15Lexer;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.ast.IElementType;
import consulo.language.lexer.Lexer;

import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2024-04-19
 */
@ExtensionImpl
public class JavaScriptHtmlScriptContentProvider implements HtmlScriptContentProvider {
    @Override
    public IElementType getScriptElementType() {
        return JSElementTypes.EMBEDDED_CONTENT;
    }

    @Nullable
    @Override
    public Lexer getHighlightingLexer() {
        return new JavaScript15Lexer();
    }

    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
