/*
 * Copyright 2013-2016 must-be.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package consulo.javascript.lang.parsing;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.psi.tree.IElementType;

/**
 * @author VISTALL
 * @since 03.03.2016
 */
public class EcmaScript6ExpressionParsing extends ExpressionParsing
{
	public EcmaScript6ExpressionParsing(JavaScriptParsingContext context)
	{
		super(context);
	}

	@Override
	protected boolean parsePrimaryExpression(PsiBuilder builder)
	{
		if(builder.getTokenType() == JSTokenTypes.DOT_DOT_DOT)
		{
			PsiBuilder.Marker mark = builder.mark();
			builder.advanceLexer();

			if(!parseExpressionOptional(builder))
			{
				mark.error("Expression expected");
			}
			else
			{
				mark.done(JSElementTypes.SPREAD_EXPRESSION);
			}

			return true;
		}
		if(canParseLambdaExpression(builder))
		{
			return parseLambdaExpression(builder);
		}
		return super.parsePrimaryExpression(builder);
	}

	private boolean parseLambdaExpression(PsiBuilder builder)
	{
		PsiBuilder.Marker mark = builder.mark();

		IElementType tokenType = builder.getTokenType();
		if(tokenType == JSTokenTypes.IDENTIFIER)
		{
			PsiBuilder.Marker parameterList = builder.mark();
			getFunctionParsing().parseParameter(builder, null);
			parameterList.done(JSElementTypes.PARAMETER_LIST);
		}
		else
		{
			getFunctionParsing().parseParameterList(builder);
		}

		if(builder.getTokenType() == JSTokenTypes.DARROW)
		{
			builder.advanceLexer();
		}
		else
		{
			mark.rollbackTo();
			return false;
		}

		if(builder.getTokenType() == JSTokenTypes.LBRACE)
		{
			getStatementParsing().parseBlock(builder);
		}
		else
		{
			parseAssignmentExpression(builder);
		}

		mark.done(JSElementTypes.LAMBDA_EXPRESSION);
		return true;
	}

	private boolean canParseLambdaExpression(PsiBuilder builder)
	{
		PsiBuilder.Marker marker = builder.mark();
		try
		{
			IElementType tokenType = builder.getTokenType();
			if(tokenType == JSTokenTypes.IDENTIFIER)
			{
				builder.advanceLexer();
				if(builder.getTokenType() == JSTokenTypes.DARROW)
				{
					return true;
				}
			}
			else if(tokenType == JSTokenTypes.LPAR)
			{
				getFunctionParsing().parseParameterList(builder);

				if(builder.getTokenType() == JSTokenTypes.DARROW)
				{
					return true;
				}
			}
			return false;
		}
		finally
		{
			marker.rollbackTo();
		}
	}

	@Override
	protected void parseProperty(PsiBuilder builder)
	{
		final IElementType nameTokenType = builder.getTokenType();
		PsiBuilder.Marker propertyMark = builder.mark();

		if(nameTokenType == JSTokenTypes.LBRACKET)
		{
			PsiBuilder.Marker mark = builder.mark();
			builder.advanceLexer();
			parseExpression(builder);
			checkMatches(builder, JSTokenTypes.RBRACKET, JavaScriptBundle.message("javascript.parser.message.expected.rbracket"));
			mark.done(JSElementTypes.COMPUTED_NAME);
		}
		else
		{
			if(isNotPropertyStart(builder, nameTokenType))
			{
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.identifier.string.literal.or.numeric.literal"));
			}
			builder.advanceLexer();
		}

		IElementType nextTokenType = builder.getTokenType();
		// we finished property
		if((nextTokenType == JSTokenTypes.COMMA || nextTokenType == JSTokenTypes.RBRACE) && nameTokenType == JSTokenTypes.IDENTIFIER)
		{
			propertyMark.rollbackTo(); // rollback it

			propertyMark = builder.mark();

			PsiBuilder.Marker referenceMark = builder.mark();
			builder.advanceLexer();
			referenceMark.done(JSElementTypes.REFERENCE_EXPRESSION);
		}
		else
		{
			checkMatches(builder, JSTokenTypes.COLON, JavaScriptBundle.message("javascript.parser.message.expected.colon"));

			builder.putUserData(WITHIN_OBJECT_LITERAL_EXPRESSION, Boolean.TRUE);
			if(!parseAssignmentExpression(builder))
			{
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.expression"));
			}
			builder.putUserData(WITHIN_OBJECT_LITERAL_EXPRESSION, null);
		}

		propertyMark.done(JSElementTypes.PROPERTY);
	}

	@Override
	public boolean isNotPropertyStart(PsiBuilder builder, IElementType elementType)
	{
		return super.isNotPropertyStart(builder, elementType) && elementType != JSTokenTypes.LBRACKET;
	}
}
