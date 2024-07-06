package consulo.javascript.ecmascript4.lang.parsing;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.lang.parsing.ExpressionParsing;
import consulo.javascript.lang.parsing.JavaScriptParsingContext;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.parser.PsiBuilder;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class EcmaScript4ExpressionParsing extends ExpressionParsing
{
	public EcmaScript4ExpressionParsing(JavaScriptParsingContext context)
	{
		super(context);
	}

	@Override
	protected boolean parseMemberExpression(PsiBuilder builder, boolean allowCallSyntax)
	{
		PsiBuilder.Marker expr = builder.mark();
		boolean isNew;

		final IElementType type = builder.getTokenType();

		if(type == JSTokenTypes.NEW_KEYWORD)
		{
			isNew = true;
			final boolean isfunction = parseNewExpression(builder);

			if(isfunction)
			{
				expr.done(JSElementTypes.NEW_EXPRESSION);
				if(builder.getTokenType() != JSTokenTypes.LPAR)
				{
					return true;
				}
				expr = expr.precede();
				isNew = false;
			}
		}
		else
		{
			isNew = false;
			if(!parsePrimaryExpression(builder))
			{
				expr.drop();
				return false;
			}
		}

		boolean currentlyAllowCallSyntax = allowCallSyntax && (type != JSTokenTypes.LBRACE && type != JSTokenTypes.LBRACKET);

		while(true)
		{
			IElementType tokenType = builder.getTokenType();
			if(tokenType == JSTokenTypes.DOT || (tokenType == JSTokenTypes.COLON_COLON || tokenType == JSTokenTypes.DOT_DOT))
			{
				currentlyAllowCallSyntax = allowCallSyntax;
				builder.advanceLexer();

				if(builder.getTokenType() == JSTokenTypes.AT)
				{
					builder.advanceLexer();
				}

				tokenType = builder.getTokenType();

				if(tokenType == JSTokenTypes.LT)
				{
					parseGenericSignature(builder);
					expr.done(JSElementTypes.REFERENCE_EXPRESSION);
					expr = expr.precede();
					continue;
				}

				if(tokenType == JSTokenTypes.LBRACKET || tokenType == JSTokenTypes.LPAR)
				{
					continue;
				}

				if(tokenType == JSTokenTypes.ANY_IDENTIFIER || isIdentifierToken(builder, tokenType))
				{
					builder.advanceLexer();
				}
				else
				{
					builder.error(JavaScriptLocalize.javascriptParserMessageExpectedName());
				}

				expr.done(JSElementTypes.REFERENCE_EXPRESSION);
				expr = expr.precede();
			}
			else if(tokenType == JSTokenTypes.LBRACKET)
			{
				builder.advanceLexer();
				parseExpression(builder);
				checkMatches(builder, JSTokenTypes.RBRACKET, JavaScriptLocalize.javascriptParserMessageExpectedRbracket().get());
				expr.done(JSElementTypes.INDEXED_PROPERTY_ACCESS_EXPRESSION);
				expr = expr.precede();
			}
			else if(currentlyAllowCallSyntax && tokenType == JSTokenTypes.LPAR)
			{
				parseArgumentList(builder);
				expr.done(isNew ? JSElementTypes.NEW_EXPRESSION : JSElementTypes.CALL_EXPRESSION);
				expr = expr.precede();
				isNew = false;
			}
			else
			{
				if(isNew)
				{
					expr.done(JSElementTypes.NEW_EXPRESSION);
				}
				else
				{
					expr.drop();
				}
				break;
			}
		}

		return true;
	}

	@Override
	public boolean parseQualifiedTypeName(PsiBuilder builder, boolean allowStar, TokenSet separatorsSet)
	{
		if(!JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(builder.getTokenType()))
		{
			return false;
		}
		PsiBuilder.Marker expr = builder.mark();
		buildTokenElement(JSElementTypes.REFERENCE_EXPRESSION, builder);
		boolean hasGenerics = false;

		while(separatorsSet.contains(builder.getTokenType()))
		{
			boolean stop = false;
			builder.advanceLexer();

			final IElementType tokenType = builder.getTokenType();
			if(tokenType == JSTokenTypes.ANY_IDENTIFIER && allowStar)
			{
				builder.advanceLexer();
				stop = true;
			}
			else if(tokenType == JSTokenTypes.LT)
			{
				parseGenericSignature(builder);
				stop = true;
				hasGenerics = true;
			}
			else if(tokenType == JSTokenTypes.DEFAULT_KEYWORD || (tokenType != JSTokenTypes.IDENTIFIER && JSTokenTypes.IDENTIFIER_TOKENS_SET
					.contains(tokenType)))
			{
				builder.advanceLexer(); // TODO: allow any keyword
			}
			else
			{
				checkMatches(builder, JSTokenTypes.IDENTIFIER, JavaScriptLocalize.javascriptParserMessageExpectedName().get());
			}
			expr.done(JSElementTypes.REFERENCE_EXPRESSION);
			expr = expr.precede();

			if(stop)
			{
				break;
			}
		}

		if(!hasGenerics && builder.getTokenType() == JSTokenTypes.LT)
		{
			parseGenericSignature(builder);
			expr.done(JSElementTypes.REFERENCE_EXPRESSION);
		}
		else
		{
			expr.drop();
		}
		return true;
	}

	@Override
	public boolean tryParseType(final PsiBuilder builder)
	{
		if(builder.getTokenType() == JSTokenTypes.COLON)
		{
			builder.advanceLexer();

			return parseType(builder);
		}

		return false;
	}

	private void parseGenericSignature(final PsiBuilder builder)
	{
		assert builder.getTokenType() == JSTokenTypes.LT;
		PsiBuilder.Marker genericTypeSignature = builder.mark();
		builder.advanceLexer();
		parseType(builder);
		checkMatches(builder, JSTokenTypes.GT, JavaScriptLocalize.javascriptParserMessageExpectedLt().get());
		genericTypeSignature.done(JSElementTypes.GENERIC_SIGNATURE);
	}
}
