package consulo.actionscript.lang;

import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingFlexLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.ecmascript4.lang.parsing.EcmaScript4Parser;
import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.lexer.Lexer;
import consulo.language.parser.PsiParser;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 06.04.2015
 */
@ExtensionImpl
public class ActionScriptLanguageVersion extends BaseJavaScriptLanguageVersion {
    @Nonnull
    public static ActionScriptLanguageVersion getInstance() {
        return JavaScriptLanguage.INSTANCE.findVersionByClass(ActionScriptLanguageVersion.class);
    }

    private final DialectOptionHolder myDialectOptionHolder = new DialectOptionHolder(true, false);

    public ActionScriptLanguageVersion() {
        super("ACTIONSCRIPT");
    }

    @Nonnull
    @Override
    public String getPresentableName() {
        return "ActionScript";
    }

    @Nonnull
    @Override
    public PsiParser createParser() {
        return new EcmaScript4Parser();
    }

    @Nonnull
    @Override
    public JSHighlighter getSyntaxHighlighter() {
        return new JSHighlighter(myDialectOptionHolder);
    }

    @Nonnull
    @Override
    public Lexer createLexer() {
        return new JavaScriptParsingFlexLexer(myDialectOptionHolder);
    }
}
