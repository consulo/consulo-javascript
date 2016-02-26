package org.mustbe.consulo.javascript.lang.parsing;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.psi.tree.IElementType;

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
		if(!checkMatches(builder, JSTokenTypes.IDENTIFIER, JavaScriptBundle.message("javascript.parser.message.expected.identifier")))
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

					if(builder.eof() || (!checkMatches(builder, JSTokenTypes.IDENTIFIER, JavaScriptBundle.message("javascript.parser.message.expected.identifier")) &&
							builder.getTokenType() != JSTokenTypes.RBRACKET))
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
							builder.error(JavaScriptBundle.message("javascript.parser.message.expected.rbracket"));
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
		final boolean haveLParen = checkMatches(builder, JSTokenTypes.LPAR, JavaScriptBundle.message("javascript.parser.message.expected.lparen"));
		boolean hasName;

		while(haveLParen)
		{
			PsiBuilder.Marker attributeNameValuePair;
			hasName = builder.getTokenType() == JSTokenTypes.IDENTIFIER;

			if(builder.getTokenType() == JSTokenTypes.COMMA)
			{
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.identifer.or.value"));
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
				checkMatches(builder, JSTokenTypes.EQ, JavaScriptBundle.message("javascript.parser.message.expected.equal"));

				if(builder.getTokenType() != JSTokenTypes.COMMA && builder.getTokenType() != JSTokenTypes.RBRACKET && builder.getTokenType() !=
						JSTokenTypes.RPAR)
				{
					builder.advanceLexer();
				}
				else
				{
					builder.error(JavaScriptBundle.message("javascript.parser.message.expected.value"));
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
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.rparen"));
				return;
			}
		}

		if(haveLParen)
		{
			checkMatches(builder, JSTokenTypes.RPAR, JavaScriptBundle.message("javascript.parser.message.expected.rparen"));
		}
		else
		{
			builder.advanceLexer();
		}
	}
}
