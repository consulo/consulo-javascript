package consulo.json.lang;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.psi.tree.IElementType;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.lang.parsing.ExpressionParsing;
import consulo.javascript.lang.parsing.Parsing;
import consulo.lang.LanguageVersion;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JsonJavaScriptParser implements PsiParser
{
	public static final Logger LOGGER = Logger.getInstance(JsonJavaScriptParser.class);

	private int myPropertyDepth;

	@NotNull
	@Override
	public ASTNode parse(@NotNull IElementType root, @NotNull PsiBuilder builder, @NotNull LanguageVersion languageVersion)
	{
		final PsiBuilder.Marker rootMarker = builder.mark();
		parseRoot(builder);
		rootMarker.done(root);
		return builder.getTreeBuilt();
	}

	private void parseRoot(PsiBuilder builder)
	{
		if(builder.getTokenType() == JSTokenTypes.LBRACKET)
		{
			parseArrayLiteralExpression(builder);
			if(builder.getTokenType() != null)
			{
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.eof"));
			}
		}
		else if(builder.getTokenType() == JSTokenTypes.LBRACE)
		{
			parseObjectLiteralExpression(builder);
			if(builder.getTokenType() != null)
			{
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.eof"));
			}
		}
		else
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.lbrace.or.lbracket"));
		}

		while(builder.getTokenType() != null)
		{
			builder.advanceLexer();
		}
	}

	private void parseProperty(final PsiBuilder builder)
	{
		if(myPropertyDepth > 1000)
		{
			builder.error("Too big depth for property");
			int braceCount = 0;
			int bracketCount = 0;
			while(!builder.eof())
			{
				IElementType tokenType = builder.getTokenType();
				if(tokenType == JSTokenTypes.LBRACE)
				{
					braceCount++;
				}
				else if(tokenType == JSTokenTypes.LBRACKET)
				{
					bracketCount++;
				}
				else if(tokenType == JSTokenTypes.RBRACE)
				{
					braceCount--;
					if(braceCount < 0)
					{
						break;
					}
				}
				else if(tokenType == JSTokenTypes.RBRACKET)
				{
					bracketCount--;
					if(bracketCount < 0)
					{
						break;
					}
				}
				builder.advanceLexer();
			}

			return;
		}

		final IElementType nameToken = builder.getTokenType();
		final PsiBuilder.Marker property = builder.mark();
		myPropertyDepth++;

		if(isNotPropertyStart(nameToken))
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.identifier.string.literal.or.numeric.literal"));
		}
		builder.advanceLexer();
		Parsing.checkMatches(builder, JSTokenTypes.COLON, JavaScriptBundle.message("javascript.parser.message.expected.colon"));

		if(!parseValue(builder))
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.expression"));
		}
		myPropertyDepth--;

		property.done(JSElementTypes.PROPERTY);
	}

	public void parseObjectLiteralExpression(final PsiBuilder builder)
	{
		LOGGER.assertTrue(builder.getTokenType() == JSTokenTypes.LBRACE);
		final PsiBuilder.Marker expr = builder.mark();
		builder.advanceLexer();

		IElementType elementType = builder.getTokenType();

		while(elementType != JSTokenTypes.RBRACE && elementType != null)
		{
			parseProperty(builder);

			elementType = builder.getTokenType();
			if(elementType == JSTokenTypes.RBRACE)
			{
				break;
			}
			else if(elementType == JSTokenTypes.COMMA)
			{
				builder.advanceLexer();
			}
			else
			{
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.comma"));
			}

			elementType = builder.getTokenType();
			if(elementType == JSTokenTypes.RBRACE)
			{
				builder.error(JavaScriptBundle.message("javascript.parser.property.expected"));
			}
			else if(isNotPropertyStart(elementType))
			{
				break;
			}
		}

		Parsing.checkMatches(builder, JSTokenTypes.RBRACE, JavaScriptBundle.message("javascript.parser.message.expected.rbrace"));
		expr.done(JSElementTypes.OBJECT_LITERAL_EXPRESSION);
	}

	public static boolean isNotPropertyStart(final IElementType elementType)
	{
		return !JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(elementType) && !JavaScriptTokenSets.STRING_LITERALS.contains(elementType) && elementType != JSTokenTypes.NUMERIC_LITERAL;
	}

	public void parseArrayLiteralExpression(final PsiBuilder builder)
	{
		JsonJavaScriptParser.LOGGER.assertTrue(builder.getTokenType() == JSTokenTypes.LBRACKET);
		final PsiBuilder.Marker expr = builder.mark();
		builder.advanceLexer();
		boolean commaExpected = false;

		while(builder.getTokenType() != JSTokenTypes.RBRACKET)
		{
			if(commaExpected)
			{
				final boolean b = Parsing.checkMatches(builder, JSTokenTypes.COMMA, JavaScriptBundle.message("javascript.parser.message.expected.comma"));
				if(!b)
				{
					break;
				}
			}

			while(builder.getTokenType() == JSTokenTypes.COMMA)
			{
				builder.advanceLexer();
			}

			commaExpected = false;
			if(builder.getTokenType() != JSTokenTypes.RBRACKET)
			{
				if(!parseValue(builder))
				{
					builder.error(JavaScriptBundle.message("javascript.parser.message.expected.expression"));
					break;
				}
				else
				{
					commaExpected = true;
				}
			}
		}
		Parsing.checkMatches(builder, JSTokenTypes.RBRACKET, JavaScriptBundle.message("javascript.parser.message.expected.rbracket"));
		expr.done(JSElementTypes.ARRAY_LITERAL_EXPRESSION);
	}

	private boolean parseValue(PsiBuilder builder)
	{
		final IElementType firstToken = builder.getTokenType();
		if(firstToken == JSTokenTypes.NUMERIC_LITERAL ||
				firstToken == JSTokenTypes.STRING_LITERAL ||
				firstToken == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL ||
				firstToken == JSTokenTypes.NULL_KEYWORD ||
				firstToken == JSTokenTypes.FALSE_KEYWORD ||
				firstToken == JSTokenTypes.TRUE_KEYWORD)
		{
			String errorMessage = ExpressionParsing.validateLiteral(builder);
			Parsing.buildTokenElement(JSElementTypes.LITERAL_EXPRESSION, builder);
			if(errorMessage != null)
			{
				builder.error(errorMessage);
			}
			return true;
		}
		if(firstToken == JSTokenTypes.LBRACKET)
		{
			parseArrayLiteralExpression(builder);
			return true;
		}
		else if(firstToken == JSTokenTypes.MINUS)
		{
			PsiBuilder.Marker marker = builder.mark();
			builder.advanceLexer();
			if(builder.getTokenType() == JSTokenTypes.NUMERIC_LITERAL)
			{
				builder.advanceLexer();
				marker.done(JSElementTypes.PREFIX_EXPRESSION);
			}
			else
			{
				marker.error("Unexpected token");
			}
			return true;
		}
		else if(firstToken == JSTokenTypes.LBRACE)
		{
			parseObjectLiteralExpression(builder);
			return true;
		}
		else
		{
			return false;
		}
	}
}
