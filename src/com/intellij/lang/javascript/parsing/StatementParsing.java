/*
 * Copyright 2000-2005 JetBrains s.r.o.
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

package com.intellij.lang.javascript.parsing;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.Key;
import com.intellij.psi.tree.IElementType;

/**
 * @author max
 */
public class StatementParsing extends Parsing
{
	private static final Logger LOG = Logger.getInstance("#com.intellij.lang.javascript.parsing.StatementParsing");
	private static Key<String> withinInterfaceKey = Key.create("within.interface");

	private StatementParsing()
	{
	}

	public static void parseSourceElement(PsiBuilder builder)
	{
		final IElementType tokenType = builder.getTokenType();
		if(tokenType == JSTokenTypes.FUNCTION_KEYWORD)
		{
			FunctionParsing.parseFunctionDeclaration(builder);
		}
		else if(tokenType == JSTokenTypes.PACKAGE_KEYWORD && isECMAL4(builder))
		{
			parsePackage(builder);
		}
		else if(tokenType == JSTokenTypes.AT && isECMAL4(builder))
		{
			builder.advanceLexer();
			FunctionParsing.parseAttributeWithoutBrackets(builder);
		}
		else
		{
			doParseStatement(builder, true);
		}
	}

	private static void parsePackageBodyStatement(PsiBuilder builder)
	{
		doParseStatement(builder, true);
	}

	private static void parsePackage(final PsiBuilder builder)
	{
		final PsiBuilder.Marker _package = builder.mark();
		builder.advanceLexer();
		if(builder.getTokenType() == JSTokenTypes.IDENTIFIER)
		{
			ExpressionParsing.parseQualifiedTypeName(builder);
		}

		if(builder.getTokenType() != JSTokenTypes.LBRACE)
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.name.or.lbrace"));
		}
		else
		{
			parseBlockOrFunctionBody(builder, BlockType.PACKAGE_OR_CLASS_BODY);
		}
		_package.done(JSElementTypes.PACKAGE_STATEMENT);
	}

	public static void parseStatement(PsiBuilder builder)
	{
		doParseStatement(builder, false);
	}

	private static void doParseStatement(final PsiBuilder builder, boolean canHaveClasses)
	{
		final IElementType firstToken = builder.getTokenType();

		if(firstToken == null)
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.statement"));
			return;
		}

		if(firstToken == JSTokenTypes.LBRACE)
		{
			parseBlock(builder);
			return;
		}

		if(firstToken == JSTokenTypes.VAR_KEYWORD ||
				firstToken == JSTokenTypes.CONST_KEYWORD ||
				firstToken == JSTokenTypes.LET_KEYWORD)
		{
			parseVarStatement(builder, false);
			return;
		}

		if(firstToken == JSTokenTypes.SEMICOLON)
		{
			parseEmptyStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.IF_KEYWORD)
		{
			parseIfStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.DO_KEYWORD ||
				firstToken == JSTokenTypes.WHILE_KEYWORD ||
				firstToken == JSTokenTypes.FOR_KEYWORD)
		{
			parseIterationStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.CONTINUE_KEYWORD)
		{
			parseContinueStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.BREAK_KEYWORD)
		{
			parseBreakStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.RETURN_KEYWORD)
		{
			parseReturnStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.WITH_KEYWORD)
		{
			parseWithStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.YIELD_KEYWORD)
		{
			parseYieldStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.LET_KEYWORD)
		{
			parseLetStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.SWITCH_KEYWORD)
		{
			parseSwitchStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.THROW_KEYWORD)
		{
			parseThrowStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.TRY_KEYWORD)
		{
			parseTryStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.DEFAULT_KEYWORD && isECMAL4(builder))
		{
			parseDefaultNsStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.FUNCTION_KEYWORD)
		{
			FunctionParsing.parseFunctionDeclaration(builder);
			return;
		}

		if(canHaveClasses)
		{
			if(firstToken == JSTokenTypes.IMPORT_KEYWORD && isECMAL4(builder))
			{
				parseImportStatement(builder);
				return;
			}

			if((firstToken == JSTokenTypes.CLASS_KEYWORD || firstToken == JSTokenTypes.INTERFACE_KEYWORD) && isECMAL4(builder))
			{
				parseClass(builder);
				return;
			}

			if(firstToken == JSTokenTypes.USE_KEYWORD && isECMAL4(builder))
			{
				parseUseNamespaceDirective(builder);
				return;
			}

			if(firstToken == JSTokenTypes.INCLUDE_KEYWORD && isECMAL4(builder))
			{
				parseIncludeDirective(builder);
				return;
			}

			if(firstToken == JSTokenTypes.NAMESPACE_KEYWORD && isECMAL4(builder))
			{
				if(parseNamespaceNoMarker(builder, builder.mark()))
				{
					return;
				}
			}

			if((JSTokenTypes.IDENTIFIER == firstToken ||
					JSTokenTypes.MODIFIERS.contains(firstToken) ||
					JSTokenTypes.LBRACKET == firstToken) && isECMAL4(builder))
			{
				PsiBuilder.Marker marker = builder.mark();
				boolean wasNativeStatus = builder.getUserData(FunctionParsing.allowEmptyMethodsKey) != null;
				FunctionParsing.parseAttributesList(builder);
				boolean isNativeStatus = builder.getUserData(FunctionParsing.allowEmptyMethodsKey) != null;

				try
				{
					if(builder.eof())
					{
						marker.drop();
						return;
					}

					final IElementType tokenType = builder.getTokenType();
					if(tokenType == JSTokenTypes.FUNCTION_KEYWORD)
					{
						FunctionParsing.parseFunctionNoMarker(builder, false, marker);
						return;
					}
					else if(tokenType == JSTokenTypes.VAR_KEYWORD || tokenType == JSTokenTypes.CONST_KEYWORD)
					{
						parseVarStatementNoMarker(builder, false, marker);
						return;
					}
					else if(tokenType == JSTokenTypes.NAMESPACE_KEYWORD)
					{
						if(parseNamespaceNoMarker(builder, marker))
						{
							return;
						}
						else
						{
							builder.advanceLexer();
						}
					}
					else if(tokenType == JSTokenTypes.CLASS_KEYWORD || tokenType == JSTokenTypes.INTERFACE_KEYWORD)
					{
						parseClassNoMarker(builder, marker);
						return;
					}
					else
					{
						builder.putUserData(FunctionParsing.allowEmptyMethodsKey, null);

						if(firstToken == JSTokenTypes.IDENTIFIER)
						{
							marker.rollbackTo();
						}
						else if(JSTokenTypes.COLON_COLON == builder.getTokenType())
						{
							marker.rollbackTo();
							if(parseExpressionStatement(builder))
							{
								return;
							}
						}
						else
						{
							builder.error(JSBundle.message("javascript.parser.message.expected.function.var.class.interface.namespace"));
							marker.drop();
						}
					}
				}
				finally
				{
					if(!wasNativeStatus && isNativeStatus)
					{
						builder.putUserData(FunctionParsing.allowEmptyMethodsKey, null);
					}
				}
			}
		}

		if(firstToken == JSTokenTypes.IDENTIFIER)
		{
			// Try labeled statement:
			final PsiBuilder.Marker labeledStatement = builder.mark();
			builder.advanceLexer();
			if(builder.getTokenType() == JSTokenTypes.COLON)
			{
				builder.advanceLexer();
				parseStatement(builder);
				labeledStatement.done(JSElementTypes.LABELED_STATEMENT);
				return;
			}
			else
			{
				labeledStatement.rollbackTo();
			}
		}

		if(firstToken != JSTokenTypes.LBRACE && firstToken != JSTokenTypes.FUNCTION_KEYWORD)
		{
			if(parseExpressionStatement(builder))
			{
				return;
			}
		}

		if(firstToken == JSTokenTypes.SEMANTIC_LINEFEED)
		{
			builder.advanceLexer();
			return;
		}

		builder.error(JSBundle.message("javascript.parser.message.expected.statement"));
		builder.advanceLexer();
	}

	private static boolean parseExpressionStatement(PsiBuilder builder)
	{
		// Try expression statement
		final PsiBuilder.Marker exprStatement = builder.mark();
		if(ExpressionParsing.parseExpressionOptional(builder))
		{
			checkForSemicolon(builder);
			exprStatement.done(JSElementTypes.EXPRESSION_STATEMENT);
			return true;
		}
		else
		{
			exprStatement.drop();
		}
		return false;
	}

	private static void parseYieldStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.YIELD_KEYWORD);
		final PsiBuilder.Marker marker = builder.mark();
		builder.advanceLexer();

		ExpressionParsing.parseExpression(builder);
		checkForSemicolon(builder);
		marker.done(JSElementTypes.YIELD_STATEMENT);
	}

	private static void parseLetStatement(final PsiBuilder builder)
	{
		final PsiBuilder.Marker marker = parseLetExpressionStart(builder);

		parseBlock(builder);
		marker.done(JSElementTypes.LET_STATEMENT);
	}

	static PsiBuilder.Marker parseLetExpressionStart(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.LET_KEYWORD);
		final PsiBuilder.Marker marker = builder.mark();
		builder.advanceLexer();

		if(builder.getTokenType() == JSTokenTypes.LPAR)
		{
			builder.advanceLexer();

			if(ExpressionParsing.parseAssignmentExpression(builder))
			{
				while(builder.getTokenType() == JSTokenTypes.COMMA)
				{
					builder.advanceLexer();
					if(!ExpressionParsing.parseAssignmentExpression(builder))
					{
						break;
					}
				}
			}
			checkMatches(builder, JSTokenTypes.RPAR, JSBundle.message("javascript.parser.message.expected.rparen"));
		}
		return marker;
	}

	private static void parseDefaultNsStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.DEFAULT_KEYWORD);
		final PsiBuilder.Marker marker = builder.mark();
		builder.advanceLexer();

		if(builder.getTokenType() == JSTokenTypes.IDENTIFIER && "xml".equals(builder.getTokenText()))
		{
			builder.advanceLexer();

			if(checkMatches(builder, JSTokenTypes.NAMESPACE_KEYWORD, JSBundle.message("javascript.parser.message.expected.namespace")))
			{
				if(checkMatches(builder, JSTokenTypes.EQ, JSBundle.message("javascript.parser.message.expected.equal")))
				{
					ExpressionParsing.parseExpression(builder);
				}
			}
		}
		else
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.xml"));
		}
		marker.done(JSElementTypes.ASSIGNMENT_EXPRESSION);
	}

	private static boolean parseNamespaceNoMarker(final PsiBuilder builder, final @NotNull PsiBuilder.Marker useNSStatement)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.NAMESPACE_KEYWORD);

		builder.advanceLexer();
		if(builder.getTokenType() == JSTokenTypes.LPAR)
		{
			useNSStatement.rollbackTo();
			return false;
		}

		if(!ExpressionParsing.parseQualifiedTypeName(builder))
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.typename"));
		}
		if(builder.getTokenType() == JSTokenTypes.EQ)
		{
			builder.advanceLexer();
			checkMatches(builder, JSTokenTypes.STRING_LITERAL, JSBundle.message("javascript.parser.message.expected.string.literal"));
		}
		checkForSemicolon(builder);
		useNSStatement.done(JSElementTypes.NAMESPACE_DECLARATION);
		return true;
	}

	static void parseIncludeDirective(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.INCLUDE_KEYWORD);
		final PsiBuilder.Marker useNSStatement = builder.mark();
		builder.advanceLexer();
		checkMatches(builder, JSTokenTypes.STRING_LITERAL, JSBundle.message("javascript.parser.message.expected.string.literal"));
		checkForSemicolon(builder);

		useNSStatement.done(JSElementTypes.INCLUDE_DIRECTIVE);
	}

	private static void parseUseNamespaceDirective(final PsiBuilder builder)
	{
		final PsiBuilder.Marker useNSStatement = builder.mark();
		builder.advanceLexer();

		if(builder.getTokenType() != JSTokenTypes.NAMESPACE_KEYWORD)
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.namespace"));
		}
		else
		{
			builder.advanceLexer();

			if(!ExpressionParsing.parseQualifiedTypeName(builder))
			{
				builder.error(JSBundle.message("javascript.parser.message.expected.typename"));
			}

			while(builder.getTokenType() == JSTokenTypes.COMMA)
			{
				builder.advanceLexer();
				if(!ExpressionParsing.parseQualifiedTypeName(builder))
				{
					builder.error(JSBundle.message("javascript.parser.message.expected.typename"));
					break;
				}
			}
		}
		checkForSemicolon(builder);
		useNSStatement.done(JSElementTypes.USE_NAMESPACE_DIRECTIVE);
	}

	private static void parseImportStatement(final PsiBuilder builder)
	{
		final PsiBuilder.Marker importStatement = builder.mark();
		try
		{
			builder.advanceLexer();

			final PsiBuilder.Marker nsAssignment = builder.mark();
			if(!ExpressionParsing.parseQualifiedTypeName(builder, true))
			{
				builder.error(JSBundle.message("javascript.parser.message.expected.typename"));
				nsAssignment.drop();
				return;
			}

			if(builder.getTokenType() == JSTokenTypes.EQ)
			{
				builder.advanceLexer();
				if(!ExpressionParsing.parseQualifiedTypeName(builder))
				{
					builder.error(JSBundle.message("javascript.parser.message.expected.typename"));
				}

				nsAssignment.done(JSElementTypes.ASSIGNMENT_EXPRESSION);
			}
			else
			{
				nsAssignment.drop();
			}

			checkForSemicolon(builder);
		}
		finally
		{
			importStatement.done(JSElementTypes.IMPORT_STATEMENT);
		}
	}

	private static void parseClass(final PsiBuilder builder)
	{
		parseClassNoMarker(builder, builder.mark());
	}

	private static void parseClassNoMarker(final PsiBuilder builder, final @NotNull PsiBuilder.Marker clazz)
	{
		try
		{
			final IElementType tokenType = builder.getTokenType();
			LOG.assertTrue(JSTokenTypes.CLASS_KEYWORD == tokenType || JSTokenTypes.INTERFACE_KEYWORD == tokenType);
			if(builder.getTokenType() == JSTokenTypes.INTERFACE_KEYWORD)
			{
				builder.putUserData(FunctionParsing.allowEmptyMethodsKey, "");
				builder.putUserData(withinInterfaceKey, "");
			}

			builder.advanceLexer();
			if(!ExpressionParsing.parseQualifiedTypeName(builder))
			{
				builder.error(JSBundle.message("javascript.parser.message.expected.typename"));
			}

			if(builder.getTokenType() == JSTokenTypes.EXTENDS_KEYWORD)
			{
				parseReferenceList(builder);
			}

			if(builder.getTokenType() == JSTokenTypes.IMPLEMENTS_KEYWORD)
			{
				parseReferenceList(builder);
			}

			parseBlockOrFunctionBody(builder, BlockType.PACKAGE_OR_CLASS_BODY);
			clazz.done(JSElementTypes.CLASS);
		}
		finally
		{
			builder.putUserData(FunctionParsing.allowEmptyMethodsKey, null);
			builder.putUserData(withinInterfaceKey, null);
		}
	}

	private static void parseReferenceList(final PsiBuilder builder)
	{
		final IElementType tokenType = builder.getTokenType();
		LOG.assertTrue(tokenType == JSTokenTypes.EXTENDS_KEYWORD || tokenType == JSTokenTypes.IMPLEMENTS_KEYWORD);
		final PsiBuilder.Marker referenceList = builder.mark();
		builder.advanceLexer();

		if(ExpressionParsing.parseQualifiedTypeName(builder))
		{
			while(builder.getTokenType() == JSTokenTypes.COMMA)
			{
				builder.advanceLexer();
				if(!ExpressionParsing.parseQualifiedTypeName(builder))
				{
					break;
				}
			}
		}
		referenceList.done(tokenType == JSTokenTypes.EXTENDS_KEYWORD ? JSElementTypes.EXTENDS_LIST : JSElementTypes.IMPLEMENTS_LIST);
	}

	private static void parseTryStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.TRY_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();
		parseBlock(builder);

		while(builder.getTokenType() == JSTokenTypes.CATCH_KEYWORD)
		{
			parseCatchBlock(builder);
		}

		if(builder.getTokenType() == JSTokenTypes.FINALLY_KEYWORD)
		{
			builder.advanceLexer();
			parseBlock(builder);
		}

		statement.done(JSElementTypes.TRY_STATEMENT);
	}

	private static void parseCatchBlock(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.CATCH_KEYWORD);
		final PsiBuilder.Marker block = builder.mark();
		builder.advanceLexer();
		checkMatches(builder, JSTokenTypes.LPAR, JSBundle.message("javascript.parser.message.expected.lparen"));

		final IElementType identifierType = builder.getTokenType();

		if(JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(identifierType))
		{
			final PsiBuilder.Marker param = builder.mark();
			builder.advanceLexer();

			if(!ExpressionParsing.tryParseType(builder))
			{
				if(builder.getTokenType() == JSTokenTypes.IF_KEYWORD)
				{
					builder.advanceLexer();
					checkMatches(builder, identifierType, JSBundle.message("javascript.parser.message.expected.identifier"));
					checkMatches(builder, JSTokenTypes.INSTANCEOF_KEYWORD, JSBundle.message("javascript.parser.message.expected.instanceof"));
					checkMatches(builder, JSTokenTypes.IDENTIFIER, JSBundle.message("javascript.parser.message.expected.identifier"));
				}
			}
			param.done(JSElementTypes.FORMAL_PARAMETER);
		}
		else
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.parameter.name"));
		}

		checkMatches(builder, JSTokenTypes.RPAR, JSBundle.message("javascript.parser.message.expected.rparen"));

		parseBlock(builder);

		block.done(JSElementTypes.CATCH_BLOCK);
	}

	private static void parseThrowStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.THROW_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();

		ExpressionParsing.parseExpressionOptional(builder);

		checkForSemicolon(builder);
		statement.done(JSElementTypes.THROW_STATEMENT);
	}

	private static void parseSwitchStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.SWITCH_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();

		checkMatches(builder, JSTokenTypes.LPAR, JSBundle.message("javascript.parser.message.expected.lparen"));
		ExpressionParsing.parseExpression(builder);
		checkMatches(builder, JSTokenTypes.RPAR, JSBundle.message("javascript.parser.message.expected.rparen"));

		checkMatches(builder, JSTokenTypes.LBRACE, JSBundle.message("javascript.parser.message.expected.lbrace"));
		while(builder.getTokenType() != JSTokenTypes.RBRACE)
		{
			if(builder.eof())
			{
				builder.error(JSBundle.message("javascript.parser.message.unexpected.end.of.file"));
				statement.done(JSElementTypes.SWITCH_STATEMENT);
				return;
			}
			parseCaseOrDefaultClause(builder);
		}

		builder.advanceLexer();
		statement.done(JSElementTypes.SWITCH_STATEMENT);
	}

	private static void parseCaseOrDefaultClause(final PsiBuilder builder)
	{
		final IElementType firstToken = builder.getTokenType();
		final PsiBuilder.Marker clause = builder.mark();
		if(firstToken != JSTokenTypes.CASE_KEYWORD && firstToken != JSTokenTypes.DEFAULT_KEYWORD)
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.catch.or.default"));
		}
		builder.advanceLexer();
		if(firstToken == JSTokenTypes.CASE_KEYWORD)
		{
			ExpressionParsing.parseExpression(builder);
		}
		checkMatches(builder, JSTokenTypes.COLON, JSBundle.message("javascript.parser.message.expected.colon"));
		while(true)
		{
			IElementType token = builder.getTokenType();
			if(token == null || token == JSTokenTypes.CASE_KEYWORD || token == JSTokenTypes.DEFAULT_KEYWORD || token == JSTokenTypes.RBRACE)
			{
				break;
			}
			parseStatement(builder);
		}
		clause.done(JSElementTypes.CASE_CLAUSE);
	}

	private static void parseWithStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.WITH_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();

		checkMatches(builder, JSTokenTypes.LPAR, JSBundle.message("javascript.parser.message.expected.lparen"));
		ExpressionParsing.parseExpression(builder);
		checkMatches(builder, JSTokenTypes.RPAR, JSBundle.message("javascript.parser.message.expected.rparen"));

		parseStatement(builder);

		statement.done(JSElementTypes.WITH_STATEMENT);
	}

	private static void parseReturnStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.RETURN_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();
		boolean hasNewLine = builder.getTokenType() == JSTokenTypes.SEMANTIC_LINEFEED;

		if(!hasNewLine)
		{
			ExpressionParsing.parseExpressionOptional(builder);

			checkForSemicolon(builder);
		}
		statement.done(JSElementTypes.RETURN_STATEMENT);

		if(hasNewLine)
		{
			builder.advanceLexer();
		}
	}

	private static void parseBreakStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.BREAK_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();

		if(builder.getTokenType() == JSTokenTypes.IDENTIFIER)
		{
			builder.advanceLexer();
		}

		if(builder.getTokenType() == JSTokenTypes.SEMICOLON || builder.getTokenType() == JSTokenTypes.SEMANTIC_LINEFEED)
		{
			builder.advanceLexer();
		}

		statement.done(JSElementTypes.BREAK_STATEMENT);
	}

	private static void parseContinueStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.CONTINUE_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();
		if(builder.getTokenType() == JSTokenTypes.IDENTIFIER)
		{
			builder.advanceLexer();
		}

		if(builder.getTokenType() == JSTokenTypes.SEMICOLON || builder.getTokenType() == JSTokenTypes.SEMANTIC_LINEFEED)
		{
			builder.advanceLexer();
		}

		statement.done(JSElementTypes.CONTINUE_STATEMENT);
	}

	private static void parseIterationStatement(final PsiBuilder builder)
	{
		final IElementType tokenType = builder.getTokenType();
		if(tokenType == JSTokenTypes.DO_KEYWORD)
		{
			parseDoWhileStatement(builder);
		}
		else if(tokenType == JSTokenTypes.WHILE_KEYWORD)
		{
			parseWhileStatement(builder);
		}
		else if(tokenType == JSTokenTypes.FOR_KEYWORD)
		{
			parseForStatement(builder);
		}
		else
		{
			LOG.error("Unknown iteration statement");
		}
	}

	private static void parseForStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.FOR_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		final boolean forin = parseForLoopHeader(builder);

		parseStatement(builder);
		statement.done(forin ? JSElementTypes.FOR_IN_STATEMENT : JSElementTypes.FOR_STATEMENT);
	}

	static boolean parseForLoopHeader(final PsiBuilder builder)
	{
		builder.advanceLexer();
		if(builder.getTokenType() == JSTokenTypes.EACH_KEYWORD)
		{
			builder.advanceLexer();
		}

		checkMatches(builder, JSTokenTypes.LPAR, JSBundle.message("javascript.parser.message.expected.lparen"));
		final boolean empty;
		if(builder.getTokenType() == JSTokenTypes.VAR_KEYWORD || builder.getTokenType() == JSTokenTypes.LET_KEYWORD)
		{
			parseVarStatement(builder, true);
			empty = false;
		}
		else
		{
			empty = !ExpressionParsing.parseExpressionOptional(builder, false);
		}

		boolean forin = false;
		if(builder.getTokenType() == JSTokenTypes.SEMICOLON)
		{
			builder.advanceLexer();
			ExpressionParsing.parseExpressionOptional(builder);

			if(builder.getTokenType() == JSTokenTypes.SEMICOLON)
			{
				builder.advanceLexer();
			}
			else
			{
				builder.error(JSBundle.message("javascript.parser.message.expected.semicolon"));
			}
			ExpressionParsing.parseExpressionOptional(builder);
		}
		else if(builder.getTokenType() == JSTokenTypes.IN_KEYWORD)
		{
			forin = true;
			if(empty)
			{
				builder.error(JSBundle.message("javascript.parser.message.expected.forloop.left.hand.side.expression.or.variable.declaration"));
			}
			builder.advanceLexer();
			ExpressionParsing.parseExpression(builder);
		}
		else
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.forloop.in.or.semicolon"));
		}

		checkMatches(builder, JSTokenTypes.RPAR, JSBundle.message("javascript.parser.message.expected.rparen"));
		return forin;
	}

	private static void parseWhileStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.WHILE_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();

		checkMatches(builder, JSTokenTypes.LPAR, JSBundle.message("javascript.parser.message.expected.lparen"));
		ExpressionParsing.parseExpression(builder);
		checkMatches(builder, JSTokenTypes.RPAR, JSBundle.message("javascript.parser.message.expected.rparen"));

		parseStatement(builder);
		statement.done(JSElementTypes.WHILE_STATEMENT);
	}

	private static void parseDoWhileStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.DO_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();

		parseStatement(builder);
		checkMatches(builder, JSTokenTypes.WHILE_KEYWORD, JSBundle.message("javascript.parser.message.expected.while.keyword"));
		checkMatches(builder, JSTokenTypes.LPAR, JSBundle.message("javascript.parser.message.expected.lparen"));
		ExpressionParsing.parseExpression(builder);
		checkMatches(builder, JSTokenTypes.RPAR, JSBundle.message("javascript.parser.message.expected.rparen"));
		checkForSemicolon(builder);

		statement.done(JSElementTypes.DOWHILE_STATEMENT);
	}

	private static void parseIfStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.IF_KEYWORD);
		final PsiBuilder.Marker ifStatement = builder.mark();
		builder.advanceLexer();

		checkMatches(builder, JSTokenTypes.LPAR, JSBundle.message("javascript.parser.message.expected.lparen"));
		ExpressionParsing.parseExpression(builder);

		// handle empty expressions inside
		while(builder.getTokenType() == JSTokenTypes.OROR || builder.getTokenType() == JSTokenTypes.EQEQ)
		{
			builder.advanceLexer();
		}

		checkMatches(builder, JSTokenTypes.RPAR, JSBundle.message("javascript.parser.message.expected.rparen"));

		parseStatement(builder);

		if(builder.getTokenType() == JSTokenTypes.ELSE_KEYWORD)
		{
			builder.advanceLexer();
			parseStatement(builder);
		}

		ifStatement.done(JSElementTypes.IF_STATEMENT);
	}

	private static void parseEmptyStatement(final PsiBuilder builder)
	{
		LOG.assertTrue(builder.getTokenType() == JSTokenTypes.SEMICOLON);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();
		statement.done(JSElementTypes.EMPTY_STATEMENT);
	}

	private static void parseVarStatement(final PsiBuilder builder, final boolean inForInitializationContext)
	{

		parseVarStatementNoMarker(builder, inForInitializationContext, builder.mark());
	}

	private static void parseVarStatementNoMarker(final PsiBuilder builder, final boolean inForInitializationContext,
			final @NotNull PsiBuilder.Marker var)
	{
		final IElementType declType = builder.getTokenType();
		LOG.assertTrue(declType == JSTokenTypes.VAR_KEYWORD ||
				declType == JSTokenTypes.CONST_KEYWORD ||
				declType == JSTokenTypes.LET_KEYWORD);
		if(builder.getUserData(withinInterfaceKey) != null)
		{
			builder.error(JSBundle.message("interface.should.have.no.variable.declarations"));
		}

		builder.advanceLexer();
		boolean first = true;
		while(true)
		{
			if(first)
			{
				first = false;
			}
			else
			{
				checkMatches(builder, JSTokenTypes.COMMA, JSBundle.message("javascript.parser.message.expected.comma"));
			}

			parseVarDeclaration(builder, !inForInitializationContext);

			if(builder.getTokenType() != JSTokenTypes.COMMA)
			{
				break;
			}
		}

		if(!inForInitializationContext)
		{
			checkForSemicolon(builder);
		}

		var.done(JSElementTypes.VAR_STATEMENT);
	}

	private static void checkForSemicolon(final PsiBuilder builder)
	{
		if(builder.getTokenType() == JSTokenTypes.SEMICOLON)
		{
			builder.advanceLexer();
		}
	}

	private static void parseVarDeclaration(final PsiBuilder builder, boolean allowIn)
	{
		if(!JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(builder.getTokenType()))
		{
			builder.error(JSBundle.message("javascript.parser.message.expected.variable.name"));
			builder.advanceLexer();
			return;
		}

		PsiBuilder.Marker var = builder.mark();

		if(isECMAL4(builder))
		{
			ExpressionParsing.parseQualifiedTypeName(builder, false);
		}
		else
		{
			builder.advanceLexer();
		}

		ExpressionParsing.tryParseType(builder);

		if(builder.getTokenType() == JSTokenTypes.EQ)
		{
			builder.advanceLexer();
			if(allowIn)
			{
				if(!ExpressionParsing.parseAssignmentExpression(builder))
				{
					builder.error(JSBundle.message("javascript.parser.message.expected.expression"));
				}
			}
			else
			{
				if(!ExpressionParsing.parseAssignmentExpressionNoIn(builder))
				{
					builder.error(JSBundle.message("javascript.parser.message.expected.expression"));
				}
			}
		}
		var.done(JSElementTypes.VARIABLE);
	}

	public static void parseBlock(final PsiBuilder builder)
	{
		parseBlockOrFunctionBody(builder, BlockType.BLOCK);
	}

	public static void parseFunctionBody(final PsiBuilder builder)
	{
		parseBlockOrFunctionBody(builder, BlockType.FUNCTION_BODY);
	}

	static enum BlockType
	{
		FUNCTION_BODY, BLOCK, PACKAGE_OR_CLASS_BODY
	}

	private static void parseBlockOrFunctionBody(final PsiBuilder builder, BlockType type)
	{
		final PsiBuilder.Marker block = type != BlockType.PACKAGE_OR_CLASS_BODY ? builder.mark() : null;
		if(builder.getTokenType() != JSTokenTypes.LBRACE)
		{
			if(block != null)
			{
				block.rollbackTo();
			}
			builder.error(JSBundle.message("javascript.parser.message.expected.lbrace"));
			return;
		}

		builder.advanceLexer();
		while(builder.getTokenType() != JSTokenTypes.RBRACE)
		{
			if(builder.eof())
			{
				builder.error(JSBundle.message("javascript.parser.message.missing.rbrace"));
				if(block != null)
				{
					block.done(JSElementTypes.BLOCK_STATEMENT);
				}
				return;
			}

			if(type == BlockType.FUNCTION_BODY)
			{
				parseSourceElement(builder);
			}
			else if(type == BlockType.BLOCK)
			{
				parseStatement(builder);
			}
			else
			{
				parsePackageBodyStatement(builder);
			}
		}

		builder.advanceLexer();
		if(block != null)
		{
			block.done(JSElementTypes.BLOCK_STATEMENT);
		}
	}
}
