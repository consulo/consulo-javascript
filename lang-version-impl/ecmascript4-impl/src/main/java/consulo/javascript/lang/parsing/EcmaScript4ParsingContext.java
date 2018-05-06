package consulo.javascript.lang.parsing;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class EcmaScript4ParsingContext extends JavaScriptParsingContext
{
	@Nonnull
	@Override
	protected StatementParsing createStatementParsing()
	{
		return new EcmaScript4StatementParsing(this);
	}

	@Nonnull
	@Override
	protected FunctionParsing createFunctionParsing()
	{
		return new EcmaScript4FunctionParsing(this);
	}

	@Nonnull
	@Override
	protected ExpressionParsing createExpressionParsing()
	{
		return new EcmaScript4ExpressionParsing(this);
	}
}
