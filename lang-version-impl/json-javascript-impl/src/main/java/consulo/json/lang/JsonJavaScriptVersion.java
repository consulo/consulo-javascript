package consulo.json.lang;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.json.JsonFileType;
import consulo.json.lang.lexer.JsonLexer;
import consulo.language.editor.highlight.SyntaxHighlighter;
import consulo.language.lexer.Lexer;
import consulo.language.parser.PsiParser;
import consulo.virtualFileSystem.fileType.FileType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.function.Supplier;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
@ExtensionImpl
public class JsonJavaScriptVersion extends BaseJavaScriptLanguageVersion {
    private static final Supplier<Lexer> ourLexerFactory = JsonLexer::new;

    @Nonnull
    public static JsonJavaScriptVersion getInstance() {
        return JavaScriptLanguage.INSTANCE.findVersionByClass(JsonJavaScriptVersion.class);
    }

    public JsonJavaScriptVersion() {
        super("JSON", "application/json");
    }

    @Nullable
    @Override
    public FileType getAssociatedFileType() {
        return JsonFileType.INSTANCE;
    }

    @Nonnull
    @Override
    public SyntaxHighlighter getSyntaxHighlighter() {
        return new JavaScriptHighlighter(ourLexerFactory);
    }

    @Nonnull
    @Override
    public Lexer createLexer() {
        return ourLexerFactory.get();
    }

    @Nonnull
    @Override
    public PsiParser createParser() {
        return new JsonJavaScriptParser();
    }
}
