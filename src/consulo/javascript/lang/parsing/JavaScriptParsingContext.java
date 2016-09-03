package consulo.javascript.lang.parsing;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.psi.tree.IElementType;

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

	public boolean isIdentifierToken(PsiBuilder builder, IElementType tokenType)
	{
		if(isIdentifierName(builder, tokenType))
		{
			return true;
		}

		if(JSTokenTypes.KEYWORDS.contains(tokenType))
		{
			builder.remapCurrentToken(JSTokenTypes.IDENTIFIER);
			return true;
		}
		return false;
	}

	public boolean isIdentifierName(PsiBuilder builder, IElementType tokenType)
	{
		return JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(tokenType);
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