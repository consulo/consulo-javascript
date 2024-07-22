package consulo.javascript.ecmascript4.lang.parsing;

import consulo.javascript.lang.parsing.JavaScriptParser;
import consulo.javascript.lang.parsing.JavaScriptParsingContext;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class EcmaScript4Parser extends JavaScriptParser {
    @Nonnull
    @Override
    public JavaScriptParsingContext createParsingContext() {
        return new EcmaScript4ParsingContext();
    }
}
