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

package consulo.javascript.ecmascript.lang.parsing;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.lang.parsing.ExpressionParsing;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.IElementType;
import consulo.language.parser.PsiBuilder;

/**
 * @author VISTALL
 * @since 03.03.2016
 */
public class EcmaScriptExpressionParsing extends ExpressionParsing<EcmaScriptParsingContext>
{
	public EcmaScriptExpressionParsing(EcmaScriptParsingContext context)
	{
		super(context);
	}

	@Override
	public EcmaScriptStatementParsing getStatementParsing()
	{
		return (EcmaScriptStatementParsing) super.getStatementParsing();
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
				mark.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
			}
			else
			{
				mark.done(JSElementTypes.SPREAD_EXPRESSION);
			}

			return true;
		}
		else if(builder.getTokenType() == JSTokenTypes.CLASS_KEYWORD)
		{
			PsiBuilder.Marker mark = builder.mark();
			getStatementParsing().parseClassWithMarker(builder, builder.mark(), false);
			mark.done(JSElementTypes.CLASS_EXPRESSION);
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

		IElementType isSetterOrGetterType = null;

		if(nameTokenType == JSTokenTypes.LBRACKET)
		{
			PsiBuilder.Marker mark = builder.mark();
			builder.advanceLexer();
			parseExpression(builder);
			checkMatches(builder, JSTokenTypes.RBRACKET, JavaScriptLocalize.javascriptParserMessageExpectedRbracket());
			mark.done(JSElementTypes.COMPUTED_NAME);
		}
		else
		{
			if(isNotPropertyStart(builder, nameTokenType))
			{
				builder.error(JavaScriptLocalize.javascriptParserMessageExpectedIdentifierStringLiteralOrNumericLiteral());
			}

			IElementType setOrGetToken = isContextKeyword(builder, JSTokenTypes.GET_SET_TOKEN_SET);

			if(setOrGetToken != null && builder.lookAhead(1) == JSTokenTypes.IDENTIFIER)
			{
				isSetterOrGetterType = setOrGetToken;

				advanceContextKeyword(builder, isSetterOrGetterType);
			}
			else
			{
				// just advance identifier
				builder.advanceLexer();
			}
		}

		IElementType doneElement = JSElementTypes.PROPERTY;

		if(isSetterOrGetterType != null)
		{
			// advance name - see isSetterOrGetterType set
			builder.advanceLexer();

			getFunctionParsing().parseParameterList(builder);

			getStatementParsing().parseFunctionBody(builder);

			doneElement = JSElementTypes.FUNCTION_PROPERTY;
		}
		else
		{
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
			else if(nextTokenType == JSTokenTypes.LPAR)
			{
				getFunctionParsing().parseParameterList(builder);

				getStatementParsing().parseFunctionBody(builder);

				doneElement = JSElementTypes.FUNCTION_PROPERTY;
			}
			else
			{
				checkMatches(builder, JSTokenTypes.COLON, JavaScriptLocalize.javascriptParserMessageExpectedColon());

				builder.putUserData(WITHIN_OBJECT_LITERAL_EXPRESSION, Boolean.TRUE);
				if(!parseAssignmentExpression(builder))
				{
					builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
				}
				builder.putUserData(WITHIN_OBJECT_LITERAL_EXPRESSION, null);
			}
		}

		propertyMark.done(doneElement);
	}

	@Override
	public boolean isNotPropertyStart(PsiBuilder builder, IElementType elementType)
	{
		return super.isNotPropertyStart(builder, elementType) && elementType != JSTokenTypes.LBRACKET;
	}
}
