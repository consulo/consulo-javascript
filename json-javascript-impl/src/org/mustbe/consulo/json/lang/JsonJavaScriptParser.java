package org.mustbe.consulo.json.lang;

import org.consulo.lombok.annotations.Logger;
import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.javascript.lang.parsing.ExpressionParsing;
import org.mustbe.consulo.javascript.lang.parsing.Parsing;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageVersion;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.Stack;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
@Logger
public class JsonJavaScriptParser implements PsiParser
{
	private final Stack<IElementType> myPropertiesNamesStack = new Stack<IElementType>();

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
				builder.error(JSBundle.message("javascript.parser.message.expected.eof"));
			}
		}
		else if(builder.getTokenType() == JSTokenTypes.LBRACE)
		{
			parseObjectLiteralExpression(builder);
			if(builder.getTokenType() != null)
			{
				builder.error(JSBundle.message("javascript.parser.message.expected.eof"));
			}
		}
		else
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.lbrace.or.lbracket"));
		}

		while(builder.getTokenType() != null)
		{
			builder.advanceLexer();
		}
	}

	private void parseProperty(final PsiBuilder builder)
	{
		if(myPropertiesNamesStack.size() > 1000)
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
		myPropertiesNamesStack.push(nameToken);

		if(ExpressionParsing.isNotPropertyStart(nameToken))
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.identifier.string.literal.or.numeric.literal"));
		}
		builder.advanceLexer();
		Parsing.checkMatches(builder, JSTokenTypes.COLON, JSBundle.message("javascript.parser.message.expected.colon"));

		if(!parseValue(builder))
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.expression"));
		}
		myPropertiesNamesStack.pop();

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
				builder.error(JSBundle.message("javascript.parser.message.expected.comma"));
			}

			elementType = builder.getTokenType();
			if(elementType == JSTokenTypes.RBRACE)
			{
				builder.error(JSBundle.message("javascript.parser.property.expected"));
			}
			else if(ExpressionParsing.isNotPropertyStart(elementType))
			{
				break;
			}
		}

		Parsing.checkMatches(builder, JSTokenTypes.RBRACE, JSBundle.message("javascript.parser.message.expected.rbrace"));
		expr.done(JSElementTypes.OBJECT_LITERAL_EXPRESSION);
	}

	public void parseArrayLiteralExpression(final PsiBuilder builder)
	{
		LOGGER.assertTrue(builder.getTokenType() == JSTokenTypes.LBRACKET);
		final PsiBuilder.Marker expr = builder.mark();
		builder.advanceLexer();
		boolean commaExpected = false;

		while(builder.getTokenType() != JSTokenTypes.RBRACKET)
		{
			if(commaExpected)
			{
				final boolean b = Parsing.checkMatches(builder, JSTokenTypes.COMMA, JSBundle.message("javascript.parser.message.expected.comma"));
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
					builder.error(JSBundle.message("javascript.parser.message.expected.expression"));
					break;
				}
				else
				{
					commaExpected = true;
				}
			}
		}
		Parsing.checkMatches(builder, JSTokenTypes.RBRACKET, JSBundle.message("javascript.parser.message.expected.rbracket"));
		expr.done(JSElementTypes.ARRAY_LITERAL_EXPRESSION);
	}

	private boolean parseValue(PsiBuilder builder)
	{
		final IElementType firstToken = builder.getTokenType();
		if(firstToken == JSTokenTypes.NUMERIC_LITERAL ||
				firstToken == JSTokenTypes.STRING_LITERAL ||
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
