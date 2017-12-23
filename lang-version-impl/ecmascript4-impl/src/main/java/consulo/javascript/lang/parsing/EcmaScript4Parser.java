package consulo.javascript.lang.parsing;

import org.jetbrains.annotations.NotNull;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class EcmaScript4Parser extends JavaScriptParser
{
	@NotNull
	@Override
	public JavaScriptParsingContext createParsingContext()
	{
		return new EcmaScript4ParsingContext();
	}
}
