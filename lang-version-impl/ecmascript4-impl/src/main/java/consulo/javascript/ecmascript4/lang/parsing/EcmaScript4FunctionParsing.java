package consulo.javascript.ecmascript4.lang.parsing;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.lang.parsing.FunctionParsing;
import consulo.javascript.lang.parsing.JavaScriptParsingContext;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.IElementType;
import consulo.language.parser.PsiBuilder;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class EcmaScript4FunctionParsing extends FunctionParsing
{
	public EcmaScript4FunctionParsing(JavaScriptParsingContext context)
	{
		super(context);
	}

	public void parseAttributeWithoutBrackets(final PsiBuilder builder)
	{
		final PsiBuilder.Marker attribute = builder.mark();
		if (!checkMatches(builder, JSTokenTypes.IDENTIFIER, JavaScriptLocalize.javascriptParserMessageExpectedIdentifier().get()))
		{
			attribute.drop();
			return;
		}
		parseAttributeBody(builder);
		attribute.done(JSElementTypes.ATTRIBUTE);
	}

	void parseAttributesList(final PsiBuilder builder)
	{
		final PsiBuilder.Marker modifierList = builder.mark();

		boolean seenNs = false;
		boolean seenAnyAttributes = false;

		try
		{
			boolean hasSomethingInAttrList = true;

			while(hasSomethingInAttrList)
			{
				hasSomethingInAttrList = false;

				while(builder.getTokenType() == JSTokenTypes.LBRACKET)
				{
					seenAnyAttributes = true;
					PsiBuilder.Marker attribute = builder.mark();

					builder.advanceLexer();

					if (builder.eof() || (
						!checkMatches(builder, JSTokenTypes.IDENTIFIER, JavaScriptLocalize.javascriptParserMessageExpectedIdentifier().get())
							&& builder.getTokenType() != JSTokenTypes.RBRACKET
					))
					{
						attribute.drop();
						return;
					}

					while(builder.getTokenType() != JSTokenTypes.RBRACKET)
					{
						parseAttributeBody(builder);

						if(builder.eof())
						{
							attribute.done(JSElementTypes.ATTRIBUTE);
							builder.error(JavaScriptLocalize.javascriptParserMessageExpectedRbracket().get());
							return;
						}
					}

					builder.advanceLexer();
					attribute.done(JSElementTypes.ATTRIBUTE);
					hasSomethingInAttrList = true;
				}

				if(builder.getTokenType() == JSTokenTypes.INCLUDE_KEYWORD)
				{
					hasSomethingInAttrList = true;
					getStatementParsing().parseIncludeDirective(builder);
				}

				if(builder.getTokenType() == JSTokenTypes.IDENTIFIER && !seenNs)
				{
					hasSomethingInAttrList = true;
					seenNs = true;
					PsiBuilder.Marker marker = builder.mark();
					builder.advanceLexer();
					marker.done(JSElementTypes.REFERENCE_EXPRESSION);
				}

				while(JSTokenTypes.MODIFIERS.contains(builder.getTokenType()))
				{
					seenAnyAttributes = true;
					hasSomethingInAttrList = true;
					builder.advanceLexer();
				}

				if(builder.eof())
				{
					return;
				}
			}
		}
		finally
		{
			final IElementType currentTokenType = builder.getTokenType();

			if(seenNs &&
					!seenAnyAttributes &&
					(currentTokenType != JSTokenTypes.VAR_KEYWORD &&
							currentTokenType != JSTokenTypes.FUNCTION_KEYWORD &&
							currentTokenType != JSTokenTypes.CLASS_KEYWORD &&
							currentTokenType != JSTokenTypes.INTERFACE_KEYWORD))
			{
				modifierList.drop();
			}
			else
			{
				modifierList.done(JSElementTypes.ATTRIBUTE_LIST);
			}
		}
	}

	private void parseAttributeBody(final PsiBuilder builder)
	{
		final boolean haveLParen = checkMatches(builder, JSTokenTypes.LPAR, JavaScriptLocalize.javascriptParserMessageExpectedLparen().get());
		boolean hasName;

		while(haveLParen)
		{
			PsiBuilder.Marker attributeNameValuePair;
			hasName = builder.getTokenType() == JSTokenTypes.IDENTIFIER;

			if(builder.getTokenType() == JSTokenTypes.COMMA)
			{
				builder.error(JavaScriptLocalize.javascriptParserMessageExpectedIdentiferOrValue().get());
				break;
			}
			if(builder.getTokenType() == JSTokenTypes.RBRACKET)
			{
				break;
			}

			attributeNameValuePair = builder.mark();
			builder.advanceLexer();

			if(hasName && builder.getTokenType() != JSTokenTypes.COMMA && builder.getTokenType() != JSTokenTypes.RPAR)
			{
				checkMatches(builder, JSTokenTypes.EQ, JavaScriptLocalize.javascriptParserMessageExpectedEqual().get());

				if(builder.getTokenType() != JSTokenTypes.COMMA && builder.getTokenType() != JSTokenTypes.RBRACKET && builder.getTokenType() !=
						JSTokenTypes.RPAR)
				{
					builder.advanceLexer();
				}
				else
				{
					builder.error(JavaScriptLocalize.javascriptParserMessageExpectedValue().get());
				}
			}

			if(attributeNameValuePair != null)
			{
				attributeNameValuePair.done(JSElementTypes.ATTRIBUTE_NAME_VALUE_PAIR);
			}
			if(builder.getTokenType() != JSTokenTypes.COMMA)
			{
				break;
			}
			builder.advanceLexer();

			if(builder.eof())
			{
				builder.error(JavaScriptLocalize.javascriptParserMessageExpectedRparen().get());
				return;
			}
		}

		if (haveLParen)
		{
			checkMatches(builder, JSTokenTypes.RPAR, JavaScriptLocalize.javascriptParserMessageExpectedRparen().get());
		}
		else
		{
			builder.advanceLexer();
		}
	}
}
