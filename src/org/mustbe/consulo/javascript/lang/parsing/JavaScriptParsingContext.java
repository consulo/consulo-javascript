package org.mustbe.consulo.javascript.lang.parsing;

import org.jetbrains.annotations.NotNull;

/**
 * @author VISTALL
 * @since 29.08.13.
 */
public class JavaScriptParsingContext
{
	private final ExpressionParsing myExpressionParsing;
	private final StatementParsing myStatementParsing;
	private final FunctionParsing myFunctionParsing;

	public JavaScriptParsingContext()
	{
		myExpressionParsing = createExpressionParsing();
		myStatementParsing = createStatementParsing();
		myFunctionParsing = createFunctionParsing();
	}

	@NotNull
	protected FunctionParsing createFunctionParsing()
	{
		return new FunctionParsing(this);
	}

	@NotNull
	protected StatementParsing createStatementParsing()
	{
		return new StatementParsing(this);
	}

	@NotNull
	protected ExpressionParsing createExpressionParsing()
	{
		return new ExpressionParsing(this);
	}

	@NotNull
	public FunctionParsing getFunctionParsing()
	{
		return myFunctionParsing;
	}

	@NotNull
	public ExpressionParsing getExpressionParsing()
	{
		return myExpressionParsing;
	}

	@NotNull
	public StatementParsing getStatementParsing()
	{
		return myStatementParsing;
	}
}