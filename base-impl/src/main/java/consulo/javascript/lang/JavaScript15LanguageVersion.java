package consulo.javascript.lang;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import consulo.javascript.lang.lexer.JavaScript15Lexer;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.language.StandardJavaScriptVersion;
import consulo.language.editor.highlight.SyntaxHighlighter;
import consulo.language.lexer.Lexer;


import java.util.function.Supplier;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
@ExtensionImpl
public class JavaScript15LanguageVersion extends BaseJavaScriptLanguageVersion implements StandardJavaScriptVersion {
    private static final Supplier<Lexer> ourLexerFactory = () -> new JavaScript15Lexer();

    public static JavaScript15LanguageVersion getInstance() {
        return JavaScriptLanguage.INSTANCE.findVersionByClass(JavaScript15LanguageVersion.class);
    }

    public JavaScript15LanguageVersion() {
        super("JAVASCRIPT_1_5");
    }

    @Override
    public String getPresentableName() {
        return "JavaScript 1.5";
    }

    @Override
    public Lexer createLexer() {
        return ourLexerFactory.get();
    }

    @Override
    public SyntaxHighlighter getSyntaxHighlighter() {
        return new JavaScriptHighlighter(ourLexerFactory);
    }

    @Override
    public int getWeight() {
        return 0;
    }
}
