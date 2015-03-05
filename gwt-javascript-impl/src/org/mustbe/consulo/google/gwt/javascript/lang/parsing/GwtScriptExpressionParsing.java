package org.mustbe.consulo.google.gwt.javascript.lang.parsing;

import org.mustbe.consulo.javascript.lang.parsing.ExpressionParsing;
import org.mustbe.consulo.javascript.lang.parsing.JavaScriptParsingContext;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.psi.tree.IElementType;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class GwtScriptExpressionParsing extends ExpressionParsing
{
	public GwtScriptExpressionParsing(JavaScriptParsingContext context)
	{
		super(context);
	}

	@Override
	protected boolean parseMemberExpression(PsiBuilder builder, boolean allowCallSyntax)
	{
		PsiBuilder.Marker gwtExprMark = null;

		PsiBuilder.Marker expr = builder.mark();
		boolean isNew;

		try
		{
			final IElementType type = builder.getTokenType();
			if(type == JSTokenTypes.AT)
			{
				gwtExprMark = builder.mark();
			}

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
				if(gwtExprMark != null)
				{
					builder.advanceLexer();
					if(isIdentifierToken(builder.getTokenType()))
					{
						builder.advanceLexer();
					}
				}
				else if(!parsePrimaryExpression(builder))
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
						if(gwtExprMark == null)
						{
							gwtExprMark = builder.mark();
						}

						builder.advanceLexer();
					}

					tokenType = builder.getTokenType();

					if(tokenType == JSTokenTypes.LBRACKET || tokenType == JSTokenTypes.LPAR)
					{
						continue;
					}

					if(tokenType == JSTokenTypes.ANY_IDENTIFIER || isIdentifierToken(tokenType))
					{
						builder.advanceLexer();
					}
					else
					{
						builder.error(JSBundle.message("javascript.parser.message.expected.name"));
					}

					if(gwtExprMark == null)
					{
						expr.done(JSElementTypes.REFERENCE_EXPRESSION);
						expr = expr.precede();
					}
					else if(tokenType == JSTokenTypes.GWT_FIELD_OR_METHOD)
					{
						gwtExprMark.done(JSElementTypes.GWT_REFERENCE_EXPRESSION);
						gwtExprMark = null;
						expr.done(JSElementTypes.REFERENCE_EXPRESSION);
						expr = expr.precede();
					}
				}
				else if(tokenType == JSTokenTypes.LBRACKET)
				{
					builder.advanceLexer();
					parseExpression(builder);
					checkMatches(builder, JSTokenTypes.RBRACKET, JSBundle.message("javascript.parser.message.expected.rbracket"));
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
		}
		finally
		{
			if(gwtExprMark != null)
			{
				gwtExprMark.done(JSElementTypes.GWT_REFERENCE_EXPRESSION);
			}
		}

		return true;
	}
}
