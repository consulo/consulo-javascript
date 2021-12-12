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
import com.intellij.psi.tree.TokenSet;
import consulo.javascript.ecmascript6.psi.impl.EcmaScript6ElementTypes;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.logging.Logger;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 15.02.2016
 */
public class EcmaScriptStatementParsing extends StatementParsing
{
	private static final Logger LOGGER = Logger.getInstance(EcmaScriptStatementParsing.class);

	public EcmaScriptStatementParsing(EcmaScriptParsingContext context)
	{
		super(context);
	}

	@Override
	public void parseSourceElement(PsiBuilder builder)
	{
		final IElementType tokenType = builder.getTokenType();
		if(tokenType == JSTokenTypes.FUNCTION_KEYWORD)
		{
			getFunctionParsing().parseFunctionDeclaration(builder);
		}
		else if(tokenType == JSTokenTypes.EXPORT_KEYWORD && builder.lookAhead(1) == JSTokenTypes.DEFAULT_KEYWORD)
		{
			PsiBuilder.Marker mark = builder.mark();
			builder.advanceLexer();
			builder.advanceLexer();

			parseSourceElement(builder);

			mark.done(EcmaScript6ElementTypes.EXPORT_DEFAULT_ASSIGMENT);
		}
		else if(tokenType == JSTokenTypes.EXPORT_KEYWORD)
		{
			PsiBuilder.Marker mark = builder.mark();
			PsiBuilder.Marker attributeMark = builder.mark();
			builder.advanceLexer();
			attributeMark.done(JSElementTypes.ATTRIBUTE_LIST);

			IElementType nextType = builder.getTokenType();
			if(nextType == JSTokenTypes.FUNCTION_KEYWORD)
			{
				getFunctionParsing().parseFunctionNoMarker(builder, false, mark);
			}
			else if(nextType == JSTokenTypes.CLASS_KEYWORD)
			{
				parseClassWithMarker(builder, mark, false);
			}
			else if(nextType == JSTokenTypes.VAR_KEYWORD ||
					nextType == JSTokenTypes.CONST_KEYWORD ||
					nextType == JSTokenTypes.LET_KEYWORD)
			{
				parseVarStatementWithMarker(builder, false, mark);
			}
			else
			{
				mark.error("Expected function or variable");
			}
		}
		else
		{
			doParseStatement(builder, true);
		}
	}

	@Override
	protected boolean parseForLoopHeader(final PsiBuilder builder)
	{
		builder.advanceLexer();

		Parsing.checkMatches(builder, JSTokenTypes.LPAR, JavaScriptBundle.message("javascript.parser.message.expected.lparen"));
		final boolean empty;
		if(builder.getTokenType() == JSTokenTypes.VAR_KEYWORD || builder.getTokenType() == JSTokenTypes.LET_KEYWORD || builder.getTokenType() == JSTokenTypes.CONST_KEYWORD)
		{
			parseVarStatement(builder, true);
			empty = false;
		}
		else
		{
			empty = !getExpressionParsing().parseExpressionOptional(builder, false);
		}

		boolean forin = false;
		if(builder.getTokenType() == JSTokenTypes.SEMICOLON)
		{
			builder.advanceLexer();
			getExpressionParsing().parseExpressionOptional(builder);

			if(builder.getTokenType() == JSTokenTypes.SEMICOLON)
			{
				builder.advanceLexer();
			}
			else
			{
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.semicolon"));
			}
			getExpressionParsing().parseExpressionOptional(builder);
		}
		else if(isContextKeyword(builder, JSTokenTypes.OF_KEYWORD) || builder.getTokenType() == JSTokenTypes.IN_KEYWORD)
		{
			forin = true;
			if(empty)
			{
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.forloop.left.hand.side.expression.or.variable.declaration"));
			}

			if(builder.getTokenType() == JSTokenTypes.IN_KEYWORD)
			{
				builder.advanceLexer();
			}
			else
			{
				advanceContextKeyword(builder, JSTokenTypes.OF_KEYWORD);
			}

			getExpressionParsing().parseExpression(builder);
		}
		else
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.forloop.in.of.or.semicolon"));
		}

		Parsing.checkMatches(builder, JSTokenTypes.RPAR, JavaScriptBundle.message("javascript.parser.message.expected.rparen"));
		return forin;
	}

	@Override
	protected void doParseStatement(PsiBuilder builder, boolean canHaveClasses)
	{
		if(canHaveClasses)
		{
			if(builder.getTokenType() == JSTokenTypes.IMPORT_KEYWORD)
			{
				parseImportStatement(builder);
				return;
			}
		}
		super.doParseStatement(builder, canHaveClasses);
	}

	private void parseImportStatement(final PsiBuilder builder)
	{
		final PsiBuilder.Marker importStatement = builder.mark();
		try
		{
			builder.advanceLexer();

			boolean wantFrom = true;

			if(builder.getTokenType() == JSTokenTypes.ASTERISK)
			{
				PsiBuilder.Marker bindingMark = builder.mark();

				builder.advanceLexer();

				if(isContextKeyword(builder, JSTokenTypes.AS_KEYWORD))
				{
					advanceContextKeyword(builder, JSTokenTypes.AS_KEYWORD);

					if(builder.getTokenType() == JSTokenTypes.IDENTIFIER)
					{
						builder.advanceLexer();
					}
					else
					{
						builder.error(JavaScriptLocalize.javascriptParserMessageExpectedIdentifier());
					}
				}

				bindingMark.done(EcmaScript6ElementTypes.IMPORTED_BINDING);
			}
			else if(JavaScriptTokenSets.STRING_LITERALS.contains(builder.getTokenType()))
			{
				builder.advanceLexer();
				wantFrom = false;
			}
			else
			{
				boolean firstBinding = true;

				while(!builder.eof())
				{
					if(builder.getTokenType() != JSTokenTypes.COMMA && !isIdentifierToken(builder) && builder.getTokenType() != JSTokenTypes.LBRACE)
					{
						break;
					}

					if(isContextKeyword(builder, JSTokenTypes.FROM_KEYWORD))
					{
						break;
					}

					if(!firstBinding)
					{
						Parsing.checkMatches(builder, JSTokenTypes.COMMA, "Comma expected");
					}

					if(builder.getTokenType() == JSTokenTypes.LBRACE)
					{
						PsiBuilder.Marker namedImportsMark = builder.mark();
						builder.advanceLexer();

						boolean first = true;
						while(!builder.eof())
						{
							if(builder.getTokenType() == JSTokenTypes.RBRACE)
							{
								break;
							}

							if(!first)
							{
								if(builder.getTokenType() == JSTokenTypes.COMMA)
								{
									builder.advanceLexer();
								}
								else
								{
									builder.error("Comma expected");
								}
							}

							first = false;

							if(builder.getTokenType() == JSTokenTypes.IDENTIFIER)
							{
								PsiBuilder.Marker importBindingMark = builder.mark();
								builder.advanceLexer();

								if(isContextKeyword(builder, JSTokenTypes.AS_KEYWORD))
								{
									PsiBuilder.Marker importSpecificMark = builder.mark();
									advanceContextKeyword(builder, JSTokenTypes.AS_KEYWORD);

									if(builder.getTokenType() == JSTokenTypes.IDENTIFIER)
									{
										builder.advanceLexer();

										importSpecificMark.done(EcmaScript6ElementTypes.IMPORT_SPECIFIER);
									}
									else
									{
										importSpecificMark.error("Expected identifier");
									}
								}

								importBindingMark.done(EcmaScript6ElementTypes.IMPORTED_BINDING);
							}
							else if(builder.getTokenType() != JSTokenTypes.RBRACE)
							{
								builder.error("Expected identifier");
								break;
							}
						}

						Parsing.checkMatches(builder, JSTokenTypes.RBRACE, "'}' expected");

						namedImportsMark.done(EcmaScript6ElementTypes.NAMED_IMPORTS);
					}
					else if(builder.getTokenType() == JSTokenTypes.IDENTIFIER)
					{
						PsiBuilder.Marker importBindingMark = builder.mark();
						builder.advanceLexer();
						importBindingMark.done(EcmaScript6ElementTypes.IMPORTED_BINDING);
					}
					else
					{
						builder.error(JavaScriptBundle.message("javascript.parser.message.expected.typename"));
						break;
					}

					firstBinding = false;
				}
			}

			if(wantFrom && isContextKeyword(builder, JSTokenTypes.FROM_KEYWORD))
			{
				advanceContextKeyword(builder, JSTokenTypes.FROM_KEYWORD);

				if(JavaScriptTokenSets.STRING_LITERALS.contains(builder.getTokenType()))
				{
					builder.advanceLexer();
				}
				else
				{
					builder.error("Expecte from target");
				}
			}

			checkForSemicolon(builder);
		}
		finally
		{
			importStatement.done(EcmaScript6ElementTypes.IMPORT_DECLARATION);
		}
	}

	@Override
	protected boolean doParseStatementSub(PsiBuilder builder, boolean canHaveClasses)
	{
		IElementType tokenType = builder.getTokenType();
		if(canHaveClasses)
		{
			if(tokenType == JSTokenTypes.CLASS_KEYWORD)
			{
				parseClass(builder, true);
				return true;
			}
		}
		return false;
	}

	private void parseClass(final PsiBuilder builder, boolean nameRequired)
	{
		parseClassWithMarker(builder, builder.mark(), nameRequired);
	}

	public void parseClassWithMarker(final PsiBuilder builder, final @Nonnull PsiBuilder.Marker clazz, boolean nameRequired)
	{
		builder.advanceLexer();

		if(builder.getTokenType() != JSTokenTypes.IDENTIFIER)
		{
			if(nameRequired)
			{
				builder.error(JavaScriptLocalize.javascriptParserMessageExpectedIdentifier());
			}
		}
		else
		{
			builder.advanceLexer();
		}

		if(builder.getTokenType() == JSTokenTypes.EXTENDS_KEYWORD)
		{
			parseReferenceList(builder);
		}

		parseClassBody(builder, BlockType.PACKAGE_OR_CLASS_BODY);
		clazz.done(JSElementTypes.CLASS);
	}

	protected void parseClassBody(final PsiBuilder builder, BlockType type)
	{
		if(builder.getTokenType() != JSTokenTypes.LBRACE)
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.lbrace"));
			return;
		}

		builder.advanceLexer();
		while(builder.getTokenType() != JSTokenTypes.RBRACE)
		{
			if(builder.eof())
			{
				builder.error(JavaScriptLocalize.javascriptParserMessageMissingRbrace());
				return;
			}

			PsiBuilder.Marker attributeListMarker = builder.mark();
			if(expectStaticKeywordExpected(builder))
			{
				attributeListMarker.done(JSElementTypes.ATTRIBUTE_LIST);
			}
			else
			{
				attributeListMarker.drop();
				attributeListMarker = null;
			}

			IElementType tokenType = builder.getTokenType();
			if(tokenType != JSTokenTypes.IDENTIFIER && tokenType != JSTokenTypes.MULT)
			{
				if(attributeListMarker != null)
				{
					attributeListMarker.rollbackTo();
				}

				PsiBuilder.Marker mark = builder.mark();

				if(attributeListMarker != null)
				{
					PsiBuilder.Marker temp = builder.mark();
					expectStaticKeywordExpected(builder);
					temp.done(JSElementTypes.ATTRIBUTE_LIST);
				}

				builder.advanceLexer();
				mark.error("Expected identifier or *");
			}
			else
			{
				if(attributeListMarker != null)
				{
					attributeListMarker.rollbackTo();
				}

				PsiBuilder.Marker mark = builder.mark();

				if(attributeListMarker != null)
				{
					PsiBuilder.Marker temp = builder.mark();
					expectStaticKeywordExpected(builder);
					temp.done(JSElementTypes.ATTRIBUTE_LIST);
				}

				if(builder.getTokenType() == JSTokenTypes.MULT)
				{
					builder.advanceLexer();
				}
				else
				{
					if(isContextKeyword(builder, JSTokenTypes.GET_SET_TOKEN_SET) != null && builder.lookAhead(1) == JSTokenTypes.IDENTIFIER)
					{
						advanceContextKeyword(builder, JSTokenTypes.GET_SET_TOKEN_SET);
					}
				}

				if(!JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(builder.getTokenType()))
				{
					builder.error(JavaScriptBundle.message("javascript.parser.message.expected.identifier"));
				}
				else
				{
					builder.advanceLexer();
				}

				getFunctionParsing().parseParameterList(builder);
				getStatementParsing().parseFunctionBody(builder);

				checkForSemicolon(builder);

				mark.done(JSElementTypes.FUNCTION_DECLARATION);
			}
		}

		builder.advanceLexer();
	}

	private boolean expectStaticKeywordExpected(PsiBuilder builder)
	{
		IElementType tokenType = builder.getTokenType();
		if(tokenType == JSTokenTypes.STATIC_KEYWORD)
		{
			builder.advanceLexer();
			return true;
		}
		else if(isContextKeyword(builder, TokenSet.create(JSTokenTypes.STATIC_KEYWORD)) != null)
		{
			if(builder.lookAhead(1) == JSTokenTypes.IDENTIFIER)
			{
				advanceContextKeyword(builder, TokenSet.create(JSTokenTypes.STATIC_KEYWORD));
				return true;
			}
		}
		return false;
	}

	private void parseReferenceList(final PsiBuilder builder)
	{
		final IElementType tokenType = builder.getTokenType();
		LOGGER.assertTrue(tokenType == JSTokenTypes.EXTENDS_KEYWORD || tokenType == JSTokenTypes.IMPLEMENTS_KEYWORD);
		final PsiBuilder.Marker referenceList = builder.mark();
		builder.advanceLexer();

		if(getExpressionParsing().parseQualifiedTypeName(builder))
		{
			while(builder.getTokenType() == JSTokenTypes.COMMA)
			{
				builder.advanceLexer();
				if(!getExpressionParsing().parseQualifiedTypeName(builder))
				{
					break;
				}
			}
		}
		referenceList.done(JSElementTypes.EXTENDS_LIST);
	}
}
