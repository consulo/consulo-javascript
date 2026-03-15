package consulo.javascript.ide.hightlight;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.highlight.LanguageVersionableSyntaxHighlighterFactory;
import consulo.language.editor.highlight.SyntaxHighlighter;
import consulo.language.version.LanguageVersion;


/**
 * @author VISTALL
 * @since 05.03.2015
 */
@ExtensionImpl
public class JavaScriptSyntaxHighlightFactory extends LanguageVersionableSyntaxHighlighterFactory {
    public JavaScriptSyntaxHighlightFactory() {
    }

    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }

    @Override
    public SyntaxHighlighter getSyntaxHighlighter(LanguageVersion languageVersion) {
        if (languageVersion instanceof BaseJavaScriptLanguageVersion baseJavaScriptLanguageVersion) {
            return baseJavaScriptLanguageVersion.getSyntaxHighlighter();
        }
        throw new IllegalArgumentException(languageVersion.toString());
    }
}
