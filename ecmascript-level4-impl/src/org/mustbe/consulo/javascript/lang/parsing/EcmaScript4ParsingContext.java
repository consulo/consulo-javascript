package org.mustbe.consulo.javascript.lang.parsing;

import org.jetbrains.annotations.NotNull;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class EcmaScript4ParsingContext extends JavaScriptParsingContext
{
	@NotNull
	@Override
	protected StatementParsing createStatementParsing()
	{
		return new EcmaScript4StatementParsing(this);
	}

	@NotNull
	@Override
	protected FunctionParsing createFunctionParsing()
	{
		return new EcmaScript4FunctionParsing(this);
	}

	@NotNull
	@Override
	protected ExpressionParsing createExpressionParsing()
	{
		return new EcmaScript4ExpressionParsing(this);
	}
}
