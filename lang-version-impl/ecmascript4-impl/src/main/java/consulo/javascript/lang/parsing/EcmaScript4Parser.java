package consulo.javascript.lang.parsing;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class EcmaScript4Parser extends JavaScriptParser
{
	@Nonnull
	@Override
	public JavaScriptParsingContext createParsingContext()
	{
		return new EcmaScript4ParsingContext();
	}
}
