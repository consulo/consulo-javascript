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

package com.intellij.lang.javascript;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.LanguageParserDefinitions;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilderFactory;
import com.intellij.lang.javascript.parsing.JSDocParsing;
import com.intellij.lexer.FlexAdapter;
import com.intellij.lexer.Lexer;
import com.intellij.lexer.MergingLexerAdapter;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.ILazyParseableElementType;
import com.intellij.psi.tree.TokenSet;
import consulo.annotations.RequiredReadAction;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.javascript.lang.parsing.JavaScriptParser;
import consulo.javascript.lang.parsing.JavaScriptParsingContext;
import consulo.javascript.lang.parsing.Parsing;
import consulo.lang.LanguageVersion;

/**
 * @author max, maxim.mossienko
 */
public interface JSTokenTypes
{
	IElementType IDENTIFIER = new JSElementType("IDENTIFIER");
	IElementType ANY_IDENTIFIER = new JSElementType("ANY_IDENTIFIER");
	IElementType WHITE_SPACE = TokenType.WHITE_SPACE;
	IElementType BAD_CHARACTER = TokenType.BAD_CHARACTER;

	IElementType END_OF_LINE_COMMENT = new JSElementType("END_OF_LINE_COMMENT");
	IElementType C_STYLE_COMMENT = new JSElementType("C_STYLE_COMMENT");
	IElementType XML_STYLE_COMMENT = new JSElementType("XML_STYLE_COMMENT");
	IElementType XML_STYLE_COMMENT_START = new JSElementType("XML_STYLE_COMMENT_START");

	IElementType DOC_COMMENT = new JSChameleonElementType("DOC_COMMENT")
	{
		@Override
		protected void doParse(JavaScriptParsingContext context, PsiBuilder builder)
		{
			JSDocParsing.parseJSDoc(builder);

		}

		@Override
		@Nullable
		protected Lexer createLexer()
		{
			return new MergingLexerAdapter(new FlexAdapter(new _JSDocLexer(false)), JSDocTokenTypes.TOKENS_TO_MERGE);
		}
	};

	// Keywords:
	IElementType BREAK_KEYWORD = new JSElementType("BREAK_KEYWORD");
	IElementType CASE_KEYWORD = new JSElementType("CASE_KEYWORD");
	IElementType CATCH_KEYWORD = new JSElementType("CATCH_KEYWORD");
	IElementType CONST_KEYWORD = new JSElementType("CONST_KEYWORD");
	IElementType CONTINUE_KEYWORD = new JSElementType("CONTINUE_KEYWORD");
	IElementType DELETE_KEYWORD = new JSElementType("DELETE_KEYWORD");
	IElementType DEFAULT_KEYWORD = new JSElementType("DEFAULT_KEYWORD");
	IElementType DO_KEYWORD = new JSElementType("DO_KEYWORD");
	IElementType ELSE_KEYWORD = new JSElementType("ELSE_KEYWORD");
	IElementType FINALLY_KEYWORD = new JSElementType("FINALLY_KEYWORD");
	IElementType FOR_KEYWORD = new JSElementType("FOR_KEYWORD");
	IElementType FUNCTION_KEYWORD = new JSElementType("FUNCTION_KEYWORD");
	IElementType IF_KEYWORD = new JSElementType("IF_KEYWORD");
	IElementType IN_KEYWORD = new JSElementType("IN_KEYWORD");
	IElementType INSTANCEOF_KEYWORD = new JSElementType("INSTANCEOF_KEYWORD");
	IElementType NEW_KEYWORD = new JSElementType("NEW_KEYWORD");
	IElementType RETURN_KEYWORD = new JSElementType("RETURN_KEYWORD");
	IElementType SWITCH_KEYWORD = new JSElementType("SWITCH_KEYWORD");
	IElementType THIS_KEYWORD = new JSElementType("THIS_KEYWORD");
	IElementType THROW_KEYWORD = new JSElementType("THROW_KEYWORD");
	IElementType TRY_KEYWORD = new JSElementType("TRY_KEYWORD");
	IElementType TYPEOF_KEYWORD = new JSElementType("TYPEOF_KEYWORD");
	IElementType VAR_KEYWORD = new JSElementType("VAR_KEYWORD");
	IElementType VOID_KEYWORD = new JSElementType("VOID_KEYWORD");
	IElementType WHILE_KEYWORD = new JSElementType("WHILE_KEYWORD");
	IElementType WITH_KEYWORD = new JSElementType("WITH_KEYWORD");

	IElementType PACKAGE_KEYWORD = new JSElementType("PACKAGE_KEYWORD"); // package
	IElementType IMPORT_KEYWORD = new JSElementType("IMPORT_KEYWORD"); // import
	IElementType CLASS_KEYWORD = new JSElementType("CLASS_KEYWORD"); // class
	IElementType INTERFACE_KEYWORD = new JSElementType("INTERFACE_KEYWORD"); // interface
	IElementType PUBLIC_KEYWORD = new JSElementType("PUBLIC_KEYWORD"); // public
	IElementType STATIC_KEYWORD = new JSElementType("STATIC_KEYWORD"); // static
	IElementType INTERNAL_KEYWORD = new JSElementType("INTERNAL_KEYWORD"); // internal
	IElementType FINAL_KEYWORD = new JSElementType("FINAL_KEYWORD"); // final
	IElementType DYNAMIC_KEYWORD = new JSElementType("DYNAMIC_KEYWORD"); // dynamic
	IElementType NATIVE_KEYWORD = new JSElementType("NATIVE_KEYWORD"); // native
	IElementType VIRTUAL_KEYWORD = new JSElementType("VIRTUAL_KEYWORD"); // virtual

	IElementType EXPORT_KEYWORD = new JSElementType("EXPORT_KEYWORD"); // export
	IElementType NAMESPACE_KEYWORD = new JSElementType("NAMESPACE_KEYWORD"); // namespace
	IElementType EXTENDS_KEYWORD = new JSElementType("EXTENDS_KEYWORD"); // extends
	IElementType IMPLEMENTS_KEYWORD = new JSElementType("IMPLEMENTS_KEYWORD"); // implements
	IElementType ENUM_KEYWORD = new JSElementType("ENUM_KEYWORD"); // enum
	IElementType USE_KEYWORD = new JSElementType("USE_KEYWORD"); // use

	IElementType PRIVATE_KEYWORD = new JSElementType("PRIVATE_KEYWORD"); // private
	IElementType PROTECTED_KEYWORD = new JSElementType("PROTECTED_KEYWORD"); // protected
	IElementType OVERRIDE_KEYWORD = new JSElementType("OVERRIDE_KEYWORD"); // override
	IElementType SUPER_KEYWORD = new JSElementType("SUPER_KEYWORD"); // super
	IElementType INCLUDE_KEYWORD = new JSElementType("INCLUDE_KEYWORD"); // include
	IElementType IS_KEYWORD = new JSElementType("IS_KEYWORD"); // is
	IElementType AS_KEYWORD = new JSElementType("AS_KEYWORD"); // as
	IElementType GET_KEYWORD = new JSElementType("GET_KEYWORD"); // GET
	IElementType SET_KEYWORD = new JSElementType("SET_KEYWORD"); // SET
	IElementType EACH_KEYWORD = new JSElementType("EACH_KEYWORD");
	IElementType INT_KEYWORD = new JSElementType("INT_KEYWORD");
	IElementType UINT_KEYWORD = new JSElementType("UINT_KEYWORD");
	IElementType AT = new JSElementType("AT");

	IElementType XML_START_TAG_START = new JSElementType("XML_TAG_START");
	IElementType XML_START_TAG_LIST = new JSElementType("XML_TAG__LIST_START");
	IElementType XML_END_TAG_LIST = new JSElementType("XML_TAG__LIST_END");
	IElementType XML_END_TAG_START = new JSElementType("XML_TAG_END_START");
	IElementType XML_EMPTY_TAG_END = new JSElementType("XML_EMPTY_END_START");
	IElementType XML_NAME = new JSElementType("XML_NAME");
	IElementType XML_TAG_NAME = new JSElementType("XML_TAG_NAME");
	IElementType XML_ATTR_EQUAL = new JSElementType("XML_ATTR_EQUAL");
	IElementType XML_TAG_END = new JSElementType("XML_TAG_END");
	IElementType XML_ATTR_VALUE = new JSElementType("XML_ATTR_VALUE");
	IElementType XML_ATTR_VALUE_START = new JSElementType("XML_ATTR_VALUE_START");
	IElementType XML_ATTR_VALUE_END = new JSElementType("XML_ATTR_VALUE_END");
	IElementType XML_JS_SCRIPT = new JSChameleonElementType("XML_JS_SCRIPT")
	{
		@Override
		protected void doParse(JavaScriptParsingContext context, PsiBuilder builder)
		{
			parseScriptExpression(context, builder);
		}

		public void parseScriptExpression(JavaScriptParsingContext context, final PsiBuilder builder)
		{
			PsiBuilder.Marker root = builder.mark();
			Parsing.checkMatches(builder, JSTokenTypes.LBRACE, JavaScriptBundle.message("javascript.parser.message.expected.lbrace"));
			context.getExpressionParsing().parseExpression(builder);
			Parsing.checkMatches(builder, JSTokenTypes.RBRACE, JavaScriptBundle.message("javascript.parser.message.expected.rbrace"));

			while(!builder.eof())
			{
				builder.advanceLexer();
			}

			root.done(JSElementTypes.EMBEDDED_EXPRESSION);
		}
	};

	IElementType XML_TAG_CONTENT = new JSElementType("XML_TAG_CONTENT");
	IElementType XML_ENTITY_REF = new JSElementType("XML_ENTITY_REF");
	IElementType XML_TAG_WHITE_SPACE = new JSElementType("XML_TAG_WHITESPACE");

	TokenSet XML_TOKENS = TokenSet.create(XML_START_TAG_START, XML_START_TAG_LIST, XML_END_TAG_LIST, XML_END_TAG_START, XML_EMPTY_TAG_END, XML_NAME, XML_TAG_NAME, XML_ATTR_EQUAL, XML_ATTR_VALUE,
			XML_ATTR_VALUE_START, XML_ATTR_VALUE_END, XML_TAG_END, XML_JS_SCRIPT, XML_TAG_CONTENT, XML_STYLE_COMMENT, XML_ENTITY_REF, XML_TAG_WHITE_SPACE);

	IElementType JSP_TEXT = new JSElementType("JSP_TEXT");
	IElementType YIELD_KEYWORD = new JSElementType("YIELD_KEYWORD");
	IElementType LET_KEYWORD = new JSElementType("LET_KEYWORD");

	// Hardcoded literals
	IElementType TRUE_KEYWORD = new JSElementType("TRUE_KEYWORD");
	IElementType FALSE_KEYWORD = new JSElementType("FALSE_KEYWORD");
	IElementType NULL_KEYWORD = new JSElementType("NULL_KEYWORD");
	IElementType UNDEFINED_KEYWORD = new JSElementType("UNDEFINED_KEYWORD");

	TokenSet JS_KEYWORDS = TokenSet.create(BREAK_KEYWORD, CASE_KEYWORD, CATCH_KEYWORD, CONST_KEYWORD, CONTINUE_KEYWORD, DELETE_KEYWORD, DEFAULT_KEYWORD, DO_KEYWORD, ELSE_KEYWORD, FINALLY_KEYWORD,
			FOR_KEYWORD, FUNCTION_KEYWORD, IF_KEYWORD, IN_KEYWORD, INSTANCEOF_KEYWORD, NEW_KEYWORD, RETURN_KEYWORD, SWITCH_KEYWORD, THIS_KEYWORD, THROW_KEYWORD, TRY_KEYWORD, TYPEOF_KEYWORD,
			VAR_KEYWORD, VOID_KEYWORD, WHILE_KEYWORD, WITH_KEYWORD, TRUE_KEYWORD, FALSE_KEYWORD, NULL_KEYWORD, UNDEFINED_KEYWORD, YIELD_KEYWORD, LET_KEYWORD);

	TokenSet JS2_KEYWORDS = TokenSet.create(INTERNAL_KEYWORD, DYNAMIC_KEYWORD, FINAL_KEYWORD, NATIVE_KEYWORD, VIRTUAL_KEYWORD,
			CLASS_KEYWORD, EXTENDS_KEYWORD, IMPORT_KEYWORD, USE_KEYWORD, NAMESPACE_KEYWORD, OVERRIDE_KEYWORD, INCLUDE_KEYWORD,
			SUPER_KEYWORD, IS_KEYWORD, AS_KEYWORD, GET_KEYWORD, SET_KEYWORD, EACH_KEYWORD, INT_KEYWORD, UINT_KEYWORD);

	TokenSet ECMASCRIPT6_KEYWORDS = TokenSet.create(EXPORT_KEYWORD, ENUM_KEYWORD);

	TokenSet ECMASCRIPT6_STRICT_KEYWORDS = TokenSet.create(PUBLIC_KEYWORD, PRIVATE_KEYWORD, PROTECTED_KEYWORD, PACKAGE_KEYWORD, IMPLEMENTS_KEYWORD, INTERFACE_KEYWORD, STATIC_KEYWORD);

	TokenSet KEYWORDS = TokenSet.orSet(JS_KEYWORDS, JS2_KEYWORDS, ECMASCRIPT6_KEYWORDS);

	TokenSet CONTEXT_KEYWORDS = TokenSet.orSet(ECMASCRIPT6_STRICT_KEYWORDS, TokenSet.create(GET_KEYWORD, SET_KEYWORD));

	// Literals
	IElementType NUMERIC_LITERAL = new JSElementType("NUMERIC_LITERAL");
	IElementType STRING_LITERAL = new JSElementType("STRING_LITERAL");
	IElementType SINGLE_QUOTE_STRING_LITERAL = new JSElementType("SINGLE_QUOTE_STRING_LITERAL");
	IElementType INTERPOLATION_STRING_LITERAL = new JSElementType("INTERPOLATION_STRING_LITERAL");
	IElementType REGEXP_LITERAL = new JSElementType("REGEXP_LITERAL");

	// Punctuators
	IElementType LBRACE = new JSElementType("LBRACE");// {
	IElementType RBRACE = new JSElementType("RBRACE");// }
	IElementType LPAR = new JSElementType("LPAR");// (
	IElementType RPAR = new JSElementType("RPAR");// )
	IElementType LBRACKET = new JSElementType("LBRACKET");// [
	IElementType RBRACKET = new JSElementType("RBRACKET");// ]
	IElementType DOT = new JSElementType("DOT");// .
	IElementType SEMICOLON = new JSElementType("SEMICOLON");// ;
	IElementType COMMA = new JSElementType("COMMA");// ,

	IElementType LT = new JSElementType("LT");// <
	IElementType GT = new JSElementType("GT");// >
	IElementType LE = new JSElementType("LE");// <=
	IElementType GE = new JSElementType("GE");// >=
	IElementType EQEQ = new JSElementType("EQEQ");// ==
	IElementType NE = new JSElementType("NE");// !=
	IElementType EQEQEQ = new JSElementType("EQEQEQ");// ===
	IElementType NEQEQ = new JSElementType("NEQEQ");// !==
	IElementType PLUS = new JSElementType("PLUS");// +
	IElementType MINUS = new JSElementType("MINUS");// -
	IElementType MULT = new JSElementType("MULT");// *
	IElementType PERC = new JSElementType("PERC");// %
	IElementType PLUSPLUS = new JSElementType("PLUSPLUS");// ++
	IElementType MINUSMINUS = new JSElementType("MINUSMINUS");// --
	IElementType LTLT = new JSElementType("LTLT");// <<
	IElementType GTGT = new JSElementType("GTGT");// >>
	IElementType GTGTGT = new JSElementType("GTGTGT");// >>>
	IElementType AND = new JSElementType("AND");// &
	IElementType OR = new JSElementType("OR");// |
	IElementType XOR = new JSElementType("XOR");// ^
	IElementType EXCL = new JSElementType("EXCL");// !
	IElementType TILDE = new JSElementType("TILDE");// ~
	IElementType ANDAND = new JSElementType("ANDAND");// &&
	IElementType OROR = new JSElementType("OROR");// ||
	IElementType QUEST = new JSElementType("QUEST");// ?
	IElementType COLON = new JSElementType("COLON");// :
	IElementType EQ = new JSElementType("EQ");// =
	IElementType PLUSEQ = new JSElementType("PLUSEQ");// +=
	IElementType MINUSEQ = new JSElementType("MINUSEQ");// -=
	IElementType MULTEQ = new JSElementType("MULTEQ");// *=
	IElementType PERCEQ = new JSElementType("PERCEQ");// %=
	IElementType LTLTEQ = new JSElementType("LTLTEQ");// <<=
	IElementType GTGTEQ = new JSElementType("GTGTEQ");// >>=
	IElementType GTGTGTEQ = new JSElementType("GTGTGTEQ");// >>>=
	IElementType ANDEQ = new JSElementType("ANDEQ");// &=
	IElementType OREQ = new JSElementType("OREQ");// |=
	IElementType XOREQ = new JSElementType("XOREQ");// ^=
	IElementType DIV = new JSElementType("DIV"); // /
	IElementType DIVEQ = new JSElementType("DIVEQ"); // /=
	IElementType DARROW = new JSElementType("DARROW"); // =>

	IElementType COLON_COLON = new JSElementType("COLON_COLON"); // ::
	IElementType GWT_FIELD_OR_METHOD = new JSElementType("GWT_FIELD_OR_METHOD"); // ::

	IElementType DOT_DOT = new JSElementType("DOT_DOT"); // ..
	IElementType DOT_DOT_DOT = new JSElementType("DOT_DOT_DOT"); // ...

	IElementType CDATA_START = new JSElementType("CDATA_START"); // <![CDATA[
	IElementType CDATA_END = new JSElementType("CDATA_END"); // ]]>
	IElementType JSDOC_TAG_DATA = new JSElementType("JSDOC_TAG_DATA");

	TokenSet OPERATIONS = TokenSet.create(LT, GT, LE, GE, EQEQ, NE, EQEQEQ, NEQEQ, PLUS, MINUS, MULT, PERC, PLUSPLUS, MINUSMINUS, LTLT, GTGT, GTGTGT, AND, OR, XOR, EXCL, TILDE, ANDAND, OROR, QUEST,
			COLON, EQ, PLUSEQ, MINUSEQ, MULTEQ, PERCEQ, LTLTEQ, GTGTEQ, GTGTGTEQ, ANDEQ, OREQ, XOREQ, DIV, DIVEQ, COMMA, IS_KEYWORD, AS_KEYWORD, DOT_DOT_DOT);

	TokenSet ASSOC_OPERATIONS = TokenSet.create(PLUS, MULT, AND, OR, XOR, OROR, ANDAND);

	TokenSet ASSIGNMENT_OPERATIONS = TokenSet.create(EQ, PLUSEQ, MINUSEQ, MULTEQ, PERCEQ, LTLTEQ, GTGTEQ, GTGTGTEQ, ANDEQ, OREQ, XOREQ, DIVEQ);


	TokenSet EQUALITY_OPERATIONS = TokenSet.create(EQEQ, NE, EQEQEQ, NEQEQ);

	TokenSet RELATIONAL_OPERATIONS = TokenSet.create(LT, GT, LE, GE, INSTANCEOF_KEYWORD, IN_KEYWORD);

	TokenSet ADDITIVE_OPERATIONS = TokenSet.create(PLUS, MINUS);

	TokenSet MULTIPLICATIVE_OPERATIONS = TokenSet.create(MULT, DIV, PERC);

	TokenSet SHIFT_OPERATIONS = TokenSet.create(LTLT, GTGT, GTGTGT);

	TokenSet UNARY_OPERATIONS = TokenSet.create(PLUS, MINUS, PLUSPLUS, MINUSMINUS, TILDE, EXCL, TYPEOF_KEYWORD, VOID_KEYWORD, DELETE_KEYWORD, DOT_DOT_DOT);

	TokenSet COMMENTS = TokenSet.create(END_OF_LINE_COMMENT, DOC_COMMENT, C_STYLE_COMMENT, XML_STYLE_COMMENT, CDATA_START, CDATA_END, XML_STYLE_COMMENT_START,
			JavaScriptHighlightingLexer.getTagContentTokenType(), XML_TAG_WHITE_SPACE, JSP_TEXT, JSDOC_TAG_DATA);

	TokenSet MODIFIERS = TokenSet.create(PUBLIC_KEYWORD, STATIC_KEYWORD, OVERRIDE_KEYWORD, PROTECTED_KEYWORD, PRIVATE_KEYWORD, INTERNAL_KEYWORD, DYNAMIC_KEYWORD, FINAL_KEYWORD, NATIVE_KEYWORD,
			VIRTUAL_KEYWORD);

	TokenSet ACCESS_MODIFIERS = TokenSet.create(PUBLIC_KEYWORD, PROTECTED_KEYWORD, PRIVATE_KEYWORD, INTERNAL_KEYWORD);

	TokenSet GET_SET_TOKEN_SET = TokenSet.create(GET_KEYWORD, SET_KEYWORD);

	@Deprecated
	TokenSet IDENTIFIER_TOKENS_SET = TokenSet.create(IDENTIFIER);

	abstract class JSChameleonElementType extends ILazyParseableElementType
	{
		public JSChameleonElementType(@NonNls String name)
		{
			super(name, JavaScriptLanguage.INSTANCE);
		}

		protected abstract void doParse(JavaScriptParsingContext context, PsiBuilder builder);

		@Override
		@RequiredReadAction
		protected ASTNode doParseContents(@NotNull ASTNode chameleon, @NotNull PsiElement psi)
		{
			final Project project = psi.getProject();
			final Language languageForParser = getLanguageForParser(psi);
			final LanguageVersion tempLanguageVersion = chameleon.getUserData(LanguageVersion.KEY);
			final LanguageVersion languageVersion = tempLanguageVersion == null ? psi.getLanguageVersion() : tempLanguageVersion;
			final PsiBuilder builder = PsiBuilderFactory.getInstance().createBuilder(project, chameleon, createLexer(), languageForParser, languageVersion, chameleon.getChars());
			final JavaScriptParser parser = (JavaScriptParser) LanguageParserDefinitions.INSTANCE.forLanguage(languageForParser).createParser(languageVersion);

			JavaScriptParsingContext parsingContext = parser.createParsingContext();
			doParse(parsingContext, builder);
			return builder.getTreeBuilt();
		}

		protected Lexer createLexer()
		{
			return null;
		}
	}
}
