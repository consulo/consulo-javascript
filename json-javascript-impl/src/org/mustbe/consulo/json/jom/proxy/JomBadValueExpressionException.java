package org.mustbe.consulo.json.jom.proxy;

/**
 * @author VISTALL
 * @since 13.11.2015
 */
public class JomBadValueExpressionException extends RuntimeException
{
	public JomBadValueExpressionException()
	{
	}

	public JomBadValueExpressionException(String message)
	{
		super(message);
	}

	public JomBadValueExpressionException(String message, Throwable cause)
	{
		super(message, cause);
	}

	public JomBadValueExpressionException(Throwable cause)
	{
		super(cause);
	}
}
