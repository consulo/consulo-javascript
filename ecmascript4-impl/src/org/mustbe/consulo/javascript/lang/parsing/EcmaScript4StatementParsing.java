package org.mustbe.consulo.javascript.lang.parsing;

import consulo.lombok.annotations.Logger;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.psi.tree.IElementType;

/**
 * @author VISTALL
 * @since 24.08.14
 */
@Logger
public class EcmaScript4StatementParsing extends StatementParsing
{
	public EcmaScript4StatementParsing(EcmaScript4ParsingContext context)
	{
		super(context);
	}

	@Override
	public EcmaScript4FunctionParsing getFunctionParsing()
	{
		return (EcmaScript4FunctionParsing) super.getFunctionParsing();
	}

	@Override
	public void parseSourceElement(PsiBuilder builder)
	{
		final IElementType tokenType = builder.getTokenType();
		if(tokenType == JSTokenTypes.FUNCTION_KEYWORD)
		{
			getFunctionParsing().parseFunctionDeclaration(builder);
		}
		else if(tokenType == JSTokenTypes.PACKAGE_KEYWORD)
		{
			parsePackage(builder);
		}
		else if(tokenType == JSTokenTypes.AT)
		{
			builder.advanceLexer();
			getFunctionParsing().parseAttributeWithoutBrackets(builder);
		}
		else
		{
			doParseStatement(builder, true);
		}
	}

	@Override
	protected void parseVarDeclaration(final PsiBuilder builder, boolean allowIn)
	{
		if(!JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(builder.getTokenType()))
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.variable.name"));
			builder.advanceLexer();
			return;
		}

		PsiBuilder.Marker var = builder.mark();

		getExpressionParsing().parseQualifiedTypeName(builder, false);

		getExpressionParsing().tryParseType(builder);

		if(builder.getTokenType() == JSTokenTypes.EQ)
		{
			builder.advanceLexer();
			if(allowIn)
			{
				if(!getExpressionParsing().parseAssignmentExpression(builder))
				{
					builder.error(JavaScriptBundle.message("javascript.parser.message.expected.expression"));
				}
			}
			else
			{
				if(!getExpressionParsing().parseAssignmentExpressionNoIn(builder))
				{
					builder.error(JavaScriptBundle.message("javascript.parser.message.expected.expression"));
				}
			}
		}
		var.done(JSElementTypes.VARIABLE);
	}

	@Override
	protected void doParseStatement(final PsiBuilder builder, boolean canHaveClasses)
	{
		final IElementType firstToken = builder.getTokenType();

		if(firstToken == null)
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.statement"));
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

		if(firstToken == JSTokenTypes.DEFAULT_KEYWORD)
		{
			parseDefaultNsStatement(builder);
			return;
		}

		if(firstToken == JSTokenTypes.FUNCTION_KEYWORD)
		{
			getFunctionParsing().parseFunctionDeclaration(builder);
			return;
		}

		if(canHaveClasses)
		{
			if(firstToken == JSTokenTypes.IMPORT_KEYWORD)
			{
				parseImportStatement(builder);
				return;
			}

			if((firstToken == JSTokenTypes.CLASS_KEYWORD || firstToken == JSTokenTypes.INTERFACE_KEYWORD))
			{
				parseClass(builder);
				return;
			}

			if(firstToken == JSTokenTypes.USE_KEYWORD)
			{
				parseUseNamespaceDirective(builder);
				return;
			}

			if(firstToken == JSTokenTypes.INCLUDE_KEYWORD)
			{
				parseIncludeDirective(builder);
				return;
			}

			if(firstToken == JSTokenTypes.NAMESPACE_KEYWORD)
			{
				if(parseNamespaceNoMarker(builder, builder.mark()))
				{
					return;
				}
			}

			if((JSTokenTypes.IDENTIFIER == firstToken ||
					JSTokenTypes.MODIFIERS.contains(firstToken) ||
					JSTokenTypes.LBRACKET == firstToken))
			{
				PsiBuilder.Marker marker = builder.mark();
				getFunctionParsing().parseAttributesList(builder);

				if(builder.eof())
				{
					marker.drop();
					return;
				}

				final IElementType tokenType = builder.getTokenType();
				if(tokenType == JSTokenTypes.FUNCTION_KEYWORD)
				{
					getFunctionParsing().parseFunctionNoMarker(builder, false, marker);
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
						builder.error(JavaScriptBundle.message("javascript.parser.message.expected.function.var.class.interface.namespace"));
						marker.drop();
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

		builder.error(JavaScriptBundle.message("javascript.parser.message.expected.statement"));
		builder.advanceLexer();
	}

	private void parseDefaultNsStatement(final PsiBuilder builder)
	{
		LOGGER.assertTrue(builder.getTokenType() == JSTokenTypes.DEFAULT_KEYWORD);
		final PsiBuilder.Marker marker = builder.mark();
		builder.advanceLexer();

		if(builder.getTokenType() == JSTokenTypes.IDENTIFIER && "xml".equals(builder.getTokenText()))
		{
			builder.advanceLexer();

			if(checkMatches(builder, JSTokenTypes.NAMESPACE_KEYWORD, JavaScriptBundle.message("javascript.parser.message.expected.namespace")))
			{
				if(checkMatches(builder, JSTokenTypes.EQ, JavaScriptBundle.message("javascript.parser.message.expected.equal")))
				{
					getExpressionParsing().parseExpression(builder);
				}
			}
		}
		else
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.xml"));
		}
		marker.done(JSElementTypes.ASSIGNMENT_EXPRESSION);
	}


	private void parseImportStatement(final PsiBuilder builder)
	{
		final PsiBuilder.Marker importStatement = builder.mark();
		try
		{
			builder.advanceLexer();

			final PsiBuilder.Marker nsAssignment = builder.mark();
			if(!getExpressionParsing().parseQualifiedTypeName(builder, true))
			{
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.typename"));
				nsAssignment.drop();
				return;
			}

			if(builder.getTokenType() == JSTokenTypes.EQ)
			{
				builder.advanceLexer();
				if(!getExpressionParsing().parseQualifiedTypeName(builder))
				{
					builder.error(JavaScriptBundle.message("javascript.parser.message.expected.typename"));
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

	private boolean parseNamespaceNoMarker(final PsiBuilder builder, final @NotNull PsiBuilder.Marker useNSStatement)
	{
		LOGGER.assertTrue(builder.getTokenType() == JSTokenTypes.NAMESPACE_KEYWORD);

		builder.advanceLexer();
		if(builder.getTokenType() == JSTokenTypes.LPAR)
		{
			useNSStatement.rollbackTo();
			return false;
		}

		if(!getExpressionParsing().parseQualifiedTypeName(builder))
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.typename"));
		}
		if(builder.getTokenType() == JSTokenTypes.EQ)
		{
			builder.advanceLexer();
			checkMatches(builder, JSTokenTypes.STRING_LITERAL, JavaScriptBundle.message("javascript.parser.message.expected.string.literal"));
		}
		checkForSemicolon(builder);
		useNSStatement.done(JSElementTypes.NAMESPACE_DECLARATION);
		return true;
	}

	private void parseUseNamespaceDirective(final PsiBuilder builder)
	{
		final PsiBuilder.Marker useNSStatement = builder.mark();
		builder.advanceLexer();

		if(builder.getTokenType() != JSTokenTypes.NAMESPACE_KEYWORD)
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.namespace"));
		}
		else
		{
			builder.advanceLexer();

			if(!getExpressionParsing().parseQualifiedTypeName(builder))
			{
				builder.error(JavaScriptBundle.message("javascript.parser.message.expected.typename"));
			}

			while(builder.getTokenType() == JSTokenTypes.COMMA)
			{
				builder.advanceLexer();
				if(!getExpressionParsing().parseQualifiedTypeName(builder))
				{
					builder.error(JavaScriptBundle.message("javascript.parser.message.expected.typename"));
					break;
				}
			}
		}
		checkForSemicolon(builder);
		useNSStatement.done(JSElementTypes.USE_NAMESPACE_DIRECTIVE);
	}

	private void parseClass(final PsiBuilder builder)
	{
		parseClassNoMarker(builder, builder.mark());
	}

	private void parseClassNoMarker(final PsiBuilder builder, final @NotNull PsiBuilder.Marker clazz)
	{
		final IElementType tokenType = builder.getTokenType();
		LOGGER.assertTrue(JSTokenTypes.CLASS_KEYWORD == tokenType || JSTokenTypes.INTERFACE_KEYWORD == tokenType);

		builder.advanceLexer();
		if(!getExpressionParsing().parseQualifiedTypeName(builder))
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.typename"));
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
		referenceList.done(tokenType == JSTokenTypes.EXTENDS_KEYWORD ? JSElementTypes.EXTENDS_LIST : JSElementTypes.IMPLEMENTS_LIST);
	}

	private void parsePackage(final PsiBuilder builder)
	{
		final PsiBuilder.Marker packageMarker = builder.mark();
		builder.advanceLexer();
		if(builder.getTokenType() == JSTokenTypes.IDENTIFIER)
		{
			getExpressionParsing().parseQualifiedTypeName(builder);
		}

		if(builder.getTokenType() != JSTokenTypes.LBRACE)
		{
			builder.error(JavaScriptBundle.message("javascript.parser.message.expected.name.or.lbrace"));
		}
		else
		{
			parseBlockOrFunctionBody(builder, BlockType.PACKAGE_OR_CLASS_BODY);
		}
		packageMarker.done(JSElementTypes.PACKAGE_STATEMENT);
	}
}
