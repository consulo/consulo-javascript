package consulo.javascript.ecmascript4.lang.parsing;

import consulo.javascript.lang.parsing.ExpressionParsing;
import consulo.javascript.lang.parsing.FunctionParsing;
import consulo.javascript.lang.parsing.JavaScriptParsingContext;
import consulo.javascript.lang.parsing.StatementParsing;
import jakarta.annotation.Nonnull;

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
