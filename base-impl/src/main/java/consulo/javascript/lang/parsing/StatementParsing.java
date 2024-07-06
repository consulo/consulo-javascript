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

package consulo.javascript.lang.parsing;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.IElementType;
import consulo.language.parser.PsiBuilder;
import consulo.logging.Logger;

import javax.annotation.Nonnull;

/**
 * @author max
 */
public class StatementParsing extends Parsing
{
	private static final Logger LOG = Logger.getInstance(StatementParsing.class);

	public StatementParsing(JavaScriptParsingContext context)
	{
		super(context);
	}

	public void parseSourceElement(PsiBuilder builder)
	{
		final IElementType tokenType = builder.getTokenType();
		if(tokenType == JSTokenTypes.FUNCTION_KEYWORD)
		{
			getFunctionParsing().parseFunctionDeclaration(builder);
		}
		else
		{
			parseStatement(builder);
		}
	}

	private void parsePackageBodyStatement(PsiBuilder builder)
	{
		doParseStatement(builder, true);
	}

	public void parseStatement(PsiBuilder builder)
	{
		doParseStatement(builder, false);
	}

	protected void doParseStatement(final PsiBuilder builder, boolean canHaveClasses)
	{
		final IElementType firstToken = builder.getTokenType();

		if(firstToken == null)
		{
			builder.error(JavaScriptLocalize.javascriptParserMessageExpectedStatement());
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

		if(firstToken == JSTokenTypes.FUNCTION_KEYWORD)
		{
			getFunctionParsing().parseFunctionDeclaration(builder);
			return;
		}

		if(doParseStatementSub(builder, canHaveClasses))
		{
			return;
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

		builder.error(JavaScriptLocalize.javascriptParserMessageExpectedStatement());
		builder.advanceLexer();
	}

	protected boolean doParseStatementSub(PsiBuilder builder, boolean canHaveClasses)
	{
		return false;
	}

	protected boolean parseExpressionStatement(PsiBuilder builder)
	{
		// Try expression statement
		final PsiBuilder.Marker exprStatement = builder.mark();
		if(getExpressionParsing().parseExpressionOptional(builder))
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

	protected void parseYieldStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.YIELD_KEYWORD);
		final PsiBuilder.Marker marker = builder.mark();
		builder.advanceLexer();

		getExpressionParsing().parseExpressionOptional(builder);
		checkForSemicolon(builder);
		marker.done(JSElementTypes.YIELD_STATEMENT);
	}

	protected void parseLetStatement(final PsiBuilder builder)
	{
		final PsiBuilder.Marker marker = parseLetExpressionStart(builder);

		parseBlock(builder);
		marker.done(JSElementTypes.LET_STATEMENT);
	}

	PsiBuilder.Marker parseLetExpressionStart(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.LET_KEYWORD);
		final PsiBuilder.Marker marker = builder.mark();
		builder.advanceLexer();

		if(builder.getTokenType() == JSTokenTypes.LPAR)
		{
			builder.advanceLexer();

			if(getExpressionParsing().parseAssignmentExpression(builder))
			{
				while(builder.getTokenType() == JSTokenTypes.COMMA)
				{
					builder.advanceLexer();
					if(!getExpressionParsing().parseAssignmentExpression(builder))
					{
						break;
					}
				}
			}
			Parsing.checkMatches(builder, JSTokenTypes.RPAR, JavaScriptLocalize.javascriptParserMessageExpectedRparen());
		}
		return marker;
	}


	public void parseIncludeDirective(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.INCLUDE_KEYWORD);
		final PsiBuilder.Marker useNSStatement = builder.mark();
		builder.advanceLexer();
		Parsing.checkMatches(builder, JSTokenTypes.STRING_LITERAL, JavaScriptLocalize.javascriptParserMessageExpectedStringLiteral());
		checkForSemicolon(builder);

		useNSStatement.done(JSElementTypes.INCLUDE_DIRECTIVE);
	}

	protected void parseTryStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.TRY_KEYWORD);
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

	private void parseCatchBlock(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.CATCH_KEYWORD);
		final PsiBuilder.Marker block = builder.mark();
		builder.advanceLexer();
		Parsing.checkMatches(builder, JSTokenTypes.LPAR, JavaScriptLocalize.javascriptParserMessageExpectedLparen());

		final IElementType identifierType = builder.getTokenType();

		if(JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(identifierType))
		{
			final PsiBuilder.Marker param = builder.mark();
			builder.advanceLexer();

			if(!getExpressionParsing().tryParseType(builder))
			{
				if(builder.getTokenType() == JSTokenTypes.IF_KEYWORD)
				{
					builder.advanceLexer();
					Parsing.checkMatches(builder, identifierType, JavaScriptLocalize.javascriptParserMessageExpectedIdentifier());
					Parsing.checkMatches(builder, JSTokenTypes.INSTANCEOF_KEYWORD, JavaScriptLocalize.javascriptParserMessageExpectedInstanceof());
					Parsing.checkMatches(builder, JSTokenTypes.IDENTIFIER, JavaScriptLocalize.javascriptParserMessageExpectedIdentifier());
				}
			}
			param.done(JSElementTypes.FORMAL_PARAMETER);
		}
		else
		{
			builder.error(JavaScriptLocalize.javascriptParserMessageExpectedParameterName());
		}

		Parsing.checkMatches(builder, JSTokenTypes.RPAR, JavaScriptLocalize.javascriptParserMessageExpectedRparen());

		parseBlock(builder);

		block.done(JSElementTypes.CATCH_BLOCK);
	}

	protected void parseThrowStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.THROW_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();

		getExpressionParsing().parseExpressionOptional(builder);

		checkForSemicolon(builder);
		statement.done(JSElementTypes.THROW_STATEMENT);
	}

	protected void parseSwitchStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.SWITCH_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();

		Parsing.checkMatches(builder, JSTokenTypes.LPAR, JavaScriptLocalize.javascriptParserMessageExpectedLparen());
		getExpressionParsing().parseExpression(builder);
		Parsing.checkMatches(builder, JSTokenTypes.RPAR, JavaScriptLocalize.javascriptParserMessageExpectedRparen());

		Parsing.checkMatches(builder, JSTokenTypes.LBRACE, JavaScriptLocalize.javascriptParserMessageExpectedLbrace());
		while(builder.getTokenType() != JSTokenTypes.RBRACE)
		{
			if(builder.eof())
			{
				builder.error(JavaScriptLocalize.javascriptParserMessageUnexpectedEndOfFile());
				statement.done(JSElementTypes.SWITCH_STATEMENT);
				return;
			}
			parseCaseOrDefaultClause(builder);
		}

		builder.advanceLexer();
		statement.done(JSElementTypes.SWITCH_STATEMENT);
	}

	private void parseCaseOrDefaultClause(final PsiBuilder builder)
	{
		final IElementType firstToken = builder.getTokenType();
		final PsiBuilder.Marker clause = builder.mark();
		if(firstToken != JSTokenTypes.CASE_KEYWORD && firstToken != JSTokenTypes.DEFAULT_KEYWORD)
		{
			builder.error(JavaScriptLocalize.javascriptParserMessageExpectedCatchOrDefault());
		}
		builder.advanceLexer();
		if(firstToken == JSTokenTypes.CASE_KEYWORD)
		{
			getExpressionParsing().parseExpression(builder);
		}
		Parsing.checkMatches(builder, JSTokenTypes.COLON, JavaScriptLocalize.javascriptParserMessageExpectedColon());
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

	protected void parseWithStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.WITH_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();

		Parsing.checkMatches(builder, JSTokenTypes.LPAR, JavaScriptLocalize.javascriptParserMessageExpectedLparen());
		getExpressionParsing().parseExpression(builder);
		Parsing.checkMatches(builder, JSTokenTypes.RPAR, JavaScriptLocalize.javascriptParserMessageExpectedRparen());

		parseStatement(builder);

		statement.done(JSElementTypes.WITH_STATEMENT);
	}

	protected void parseReturnStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.RETURN_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		boolean hasNewLine = hasSemanticLineBeforeNextToken(builder);
		builder.advanceLexer();
		if(!hasNewLine)
		{
			getExpressionParsing().parseExpressionOptional(builder);

			checkForSemicolon(builder);
		}
		statement.done(JSElementTypes.RETURN_STATEMENT);
	}

	protected void parseBreakStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.BREAK_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();

		boolean hasSemanticLineBeforeNextToken = hasSemanticLineBeforeNextToken(builder);
		builder.advanceLexer();

		if(!hasSemanticLineBeforeNextToken && builder.getTokenType() == JSTokenTypes.IDENTIFIER)
		{
			builder.advanceLexer();
		}

		if(builder.getTokenType() == JSTokenTypes.SEMICOLON)
		{
			builder.advanceLexer();
		}

		statement.done(JSElementTypes.BREAK_STATEMENT);
	}

	protected void parseContinueStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.CONTINUE_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		boolean hasSemanticLineBeforeNextToken = hasSemanticLineBeforeNextToken(builder);
		builder.advanceLexer();

		if(!hasSemanticLineBeforeNextToken && builder.getTokenType() == JSTokenTypes.IDENTIFIER)
		{
			builder.advanceLexer();
		}

		if(builder.getTokenType() == JSTokenTypes.SEMICOLON)
		{
			builder.advanceLexer();
		}

		statement.done(JSElementTypes.CONTINUE_STATEMENT);
	}

	protected boolean hasSemanticLineBeforeNextToken(final PsiBuilder builder)
	{
		IElementType tokenType = builder.getTokenType();
		if(tokenType == null)
		{
			return true;
		}
		// force end
		if(tokenType == JSTokenTypes.RBRACE)
		{
			return true;
		}

		int step = 1;
		IElementType rawElementType;
		while((rawElementType = builder.rawLookup(step)) != null)
		{
			if(rawElementType == JSTokenTypes.WHITE_SPACE)
			{
				int tokenStart = builder.rawTokenTypeStart(step);
				CharSequence originalText = builder.getOriginalText();
				for(int i = tokenStart; i < originalText.length(); i++)
				{
					char c = originalText.charAt(i);
					if(c == '\n')
					{
						return true;
					}
					else if(!Character.isWhitespace(c))
					{
						return false;
					}
					else
					{
						step ++;
					}
				}
			}
			else
			{
				break;
			}
		}
		return false;
	}

	protected void parseIterationStatement(final PsiBuilder builder)
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
			StatementParsing.LOG.error("Unknown iteration statement");
		}
	}

	private void parseForStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.FOR_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		final boolean forin = parseForLoopHeader(builder);

		parseStatement(builder);
		statement.done(forin ? JSElementTypes.FOR_IN_STATEMENT : JSElementTypes.FOR_STATEMENT);
	}

	protected boolean parseForLoopHeader(final PsiBuilder builder)
	{
		builder.advanceLexer();
		if(builder.getTokenType() == JSTokenTypes.EACH_KEYWORD)
		{
			builder.advanceLexer();
		}

		Parsing.checkMatches(builder, JSTokenTypes.LPAR, JavaScriptLocalize.javascriptParserMessageExpectedLparen());
		final boolean empty;
		if(builder.getTokenType() == JSTokenTypes.VAR_KEYWORD || builder.getTokenType() == JSTokenTypes.LET_KEYWORD)
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
				builder.error(JavaScriptLocalize.javascriptParserMessageExpectedSemicolon());
			}
			getExpressionParsing().parseExpressionOptional(builder);
		}
		else if(builder.getTokenType() == JSTokenTypes.IN_KEYWORD)
		{
			forin = true;
			if(empty)
			{
				builder.error(JavaScriptLocalize.javascriptParserMessageExpectedForloopLeftHandSideExpressionOrVariableDeclaration());
			}
			builder.advanceLexer();
			getExpressionParsing().parseExpression(builder);
		}
		else
		{
			builder.error(JavaScriptLocalize.javascriptParserMessageExpectedForloopInOrSemicolon());
		}

		Parsing.checkMatches(builder, JSTokenTypes.RPAR, JavaScriptLocalize.javascriptParserMessageExpectedRparen());
		return forin;
	}

	private void parseWhileStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.WHILE_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();

		Parsing.checkMatches(builder, JSTokenTypes.LPAR, JavaScriptLocalize.javascriptParserMessageExpectedLparen());
		getExpressionParsing().parseExpression(builder);
		Parsing.checkMatches(builder, JSTokenTypes.RPAR, JavaScriptLocalize.javascriptParserMessageExpectedRparen());

		parseStatement(builder);
		statement.done(JSElementTypes.WHILE_STATEMENT);
	}

	private void parseDoWhileStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.DO_KEYWORD);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();

		parseStatement(builder);
		Parsing.checkMatches(builder, JSTokenTypes.WHILE_KEYWORD, JavaScriptLocalize.javascriptParserMessageExpectedWhileKeyword());
		Parsing.checkMatches(builder, JSTokenTypes.LPAR, JavaScriptLocalize.javascriptParserMessageExpectedLparen());
		getExpressionParsing().parseExpression(builder);
		Parsing.checkMatches(builder, JSTokenTypes.RPAR, JavaScriptLocalize.javascriptParserMessageExpectedRparen());
		checkForSemicolon(builder);

		statement.done(JSElementTypes.DOWHILE_STATEMENT);
	}

	protected void parseIfStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.IF_KEYWORD);
		final PsiBuilder.Marker ifStatement = builder.mark();
		builder.advanceLexer();

		Parsing.checkMatches(builder, JSTokenTypes.LPAR, JavaScriptLocalize.javascriptParserMessageExpectedLparen());
		getExpressionParsing().parseExpression(builder);

		// handle empty expressions inside
		while(builder.getTokenType() == JSTokenTypes.OROR || builder.getTokenType() == JSTokenTypes.EQEQ)
		{
			builder.advanceLexer();
		}

		Parsing.checkMatches(builder, JSTokenTypes.RPAR, JavaScriptLocalize.javascriptParserMessageExpectedRparen());

		parseStatement(builder);

		if(builder.getTokenType() == JSTokenTypes.ELSE_KEYWORD)
		{
			builder.advanceLexer();
			parseStatement(builder);
		}

		ifStatement.done(JSElementTypes.IF_STATEMENT);
	}

	protected void parseEmptyStatement(final PsiBuilder builder)
	{
		StatementParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.SEMICOLON);
		final PsiBuilder.Marker statement = builder.mark();
		builder.advanceLexer();
		statement.done(JSElementTypes.EMPTY_STATEMENT);
	}

	protected void parseVarStatement(final PsiBuilder builder, final boolean inForInitializationContext)
	{
		parseVarStatementWithMarker(builder, inForInitializationContext, builder.mark());
	}

	protected void parseVarStatementWithMarker(final PsiBuilder builder, final boolean inForInitializationContext, final @Nonnull PsiBuilder.Marker var)
	{
		final IElementType declType = builder.getTokenType();
		LOG.assertTrue(declType == JSTokenTypes.VAR_KEYWORD ||
				declType == JSTokenTypes.CONST_KEYWORD ||
				declType == JSTokenTypes.LET_KEYWORD);

		builder.advanceLexer();

		if(builder.getTokenType() == JSTokenTypes.LBRACE)
		{
			parseDeconstructionElement(builder);
		}
		else
		{
			boolean first = true;
			while(true)
			{
				if(first)
				{
					first = false;
				}
				else
				{
					Parsing.checkMatches(builder, JSTokenTypes.COMMA, JavaScriptLocalize.javascriptParserMessageExpectedComma());
				}

				parseVarDeclaration(builder, !inForInitializationContext);

				if(builder.getTokenType() != JSTokenTypes.COMMA)
				{
					break;
				}
			}
		}

		if(!inForInitializationContext)
		{
			checkForSemicolon(builder);
		}

		var.done(JSElementTypes.VAR_STATEMENT);
	}

	protected void parseDeconstructionElement(PsiBuilder builder)
	{
		PsiBuilder.Marker mark = builder.mark();

		PsiBuilder.Marker desctructionObjectMarker = builder.mark();
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
				Parsing.checkMatches(builder, JSTokenTypes.COMMA, "Comma expected");
			}

			first = false;

			if(isIdentifierToken(builder))
			{
				PsiBuilder.Marker propertyMarker = builder.mark();
				PsiBuilder.Marker varMarker = builder.mark();
				builder.advanceLexer();
				varMarker.done(JSElementTypes.VARIABLE);
				propertyMarker.done(JSElementTypes.DESTRUCTURING_SHORTHANDED_PROPERTY);
			}
			else
			{
				PsiBuilder.Marker err = builder.mark();
				builder.advanceLexer();
				err.error("Expected identifier");
			}
		}

		Parsing.checkMatches(builder, JSTokenTypes.RBRACE, "'}' expected");

		desctructionObjectMarker.done(JSElementTypes.DESTRUCTURING_OBJECT);

		parseVarInitializer(builder, false);

		mark.done(JSElementTypes.DESTRUCTURING_ELEMENT);
	}

	protected void checkForSemicolon(final PsiBuilder builder)
	{
		if(builder.getTokenType() == JSTokenTypes.SEMICOLON)
		{
			builder.advanceLexer();
		}
	}

	protected void parseVarDeclaration(final PsiBuilder builder, boolean allowIn)
	{
		if(!JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(builder.getTokenType()))
		{
			builder.error(JavaScriptLocalize.javascriptParserMessageExpectedVariableName());
			builder.advanceLexer();
			return;
		}

		PsiBuilder.Marker var = builder.mark();

		builder.advanceLexer();

		getExpressionParsing().tryParseType(builder);

		parseVarInitializer(builder, allowIn);

		var.done(JSElementTypes.VARIABLE);
	}

	protected void parseVarInitializer(PsiBuilder builder, boolean allowIn)
	{
		if(builder.getTokenType() == JSTokenTypes.EQ)
		{
			builder.advanceLexer();
			if(allowIn)
			{
				if(!getExpressionParsing().parseAssignmentExpression(builder))
				{
					builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
				}
			}
			else
			{
				if(!getExpressionParsing().parseAssignmentExpressionNoIn(builder))
				{
					builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
				}
			}
		}
	}

	public void parseBlock(final PsiBuilder builder)
	{
		parseBlockOrFunctionBody(builder, BlockType.BLOCK);
	}

	public void parseFunctionBody(final PsiBuilder builder)
	{
		parseBlockOrFunctionBody(builder, BlockType.FUNCTION_BODY);
	}

	public enum BlockType
	{
		FUNCTION_BODY, BLOCK, PACKAGE_OR_CLASS_BODY
	}

	protected void parseBlockOrFunctionBody(final PsiBuilder builder, BlockType type)
	{
		final PsiBuilder.Marker block = type != BlockType.PACKAGE_OR_CLASS_BODY ? builder.mark() : null;
		if(builder.getTokenType() != JSTokenTypes.LBRACE)
		{
			if(block != null)
			{
				block.rollbackTo();
			}
			builder.error(JavaScriptLocalize.javascriptParserMessageExpectedLbrace());
			return;
		}

		builder.advanceLexer();
		while(builder.getTokenType() != JSTokenTypes.RBRACE)
		{
			if(builder.eof())
			{
				builder.error(JavaScriptLocalize.javascriptParserMessageMissingRbrace());
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
