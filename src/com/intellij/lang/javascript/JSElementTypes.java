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

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.*;
import com.intellij.lang.javascript.psi.stubs.*;
import com.intellij.lang.javascript.types.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.ElementTypeAsPsiFactory;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.ILazyParseableElementType;
import com.intellij.psi.tree.IStubFileElementType;
import com.intellij.psi.tree.TokenSet;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 28, 2005
 * Time: 12:27:21 AM
 * To change this template use File | Settings | File Templates.
 */
public interface JSElementTypes
{
	IStubFileElementType<JSFileStub> FILE = new JSFileElementType(JavascriptLanguage.INSTANCE);

	IElementType EMBEDDED_CONTENT = new ILazyParseableElementType("EMBEDDED_CONTENT", Language.findInstance(JavascriptLanguage.class))
	{
		@NotNull
		@Override
		public Language getLanguage()
		{
			return JavaScriptSupportLoader.JS_IN_HTML_DIALECT;
		}
	};
	IElementType EMBEDDED_EXPRESSION = new ElementTypeAsPsiFactory("EMBEDDED_EXPRESSION", JavascriptLanguage.INSTANCE, JSEmbeddedContentImpl.class);
	JSStubElementType<JSFunctionStub, JSFunction> FUNCTION_DECLARATION = new JSFunctionElementType("FUNCTION_DECLARATION")
	{
		@Override
		public JSFunction createPsi(@NotNull JSFunctionStub stub)
		{
			return new JSFunctionImpl(stub, this);
		}

		@NotNull
		@Override
		public PsiElement createElement(@NotNull ASTNode astNode)
		{
			return new JSFunctionImpl(astNode);
		}
	};
	JSStubElementType<JSParameterListStub, JSParameterList> PARAMETER_LIST = new JSParameterListElementType();

	JSStubElementType<JSParameterStub, JSParameter> FORMAL_PARAMETER = new JSParameterElementType();
	JSStubElementType<JSVariableStub, JSVariable> VARIABLE = new JSVariableElementType();
	IElementType ARGUMENT_LIST = new ElementTypeAsPsiFactory("ARGUMENT_LIST", JavascriptLanguage.INSTANCE, JSArgumentListImpl.class);

	JSStubElementType<JSAttributeStub, JSAttribute> ATTRIBUTE = new JSAttributeElementType();
	JSStubElementType<JSAttributeNameValuePairStub, JSAttributeNameValuePair> ATTRIBUTE_NAME_VALUE_PAIR = new JSAttributeNameValuePairType();
	JSStubElementType<JSAttributeListStub, JSAttributeList> ATTRIBUTE_LIST = new JSAttributeListElementType();
	JSStubElementType<JSPackageStatementStub, JSPackageStatement> PACKAGE_STATEMENT = new JSPackageStatementElementType();
	JSStubElementType<JSImportStatementStub, JSImportStatement> IMPORT_STATEMENT = new JSImportStatementElementType();
	JSStubElementType<JSClassStub, JSClass> CLASS = new JSClassElementType();
	JSStubElementType<JSReferenceListStub, JSReferenceList> EXTENDS_LIST = new JSReferenceListElementType("EXTENDS_LIST");
	JSStubElementType<JSReferenceListStub, JSReferenceList> IMPLEMENTS_LIST = new JSReferenceListElementType("IMPLEMENTS_LIST");
	JSStubElementType<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective> USE_NAMESPACE_DIRECTIVE = new JSUseNamespaceDirectiveType();
	JSStubElementType<JSIncludeDirectiveStub, JSIncludeDirective> INCLUDE_DIRECTIVE = new JSIncludeDirectiveElementType();
	JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration> NAMESPACE_DECLARATION = new JSNamespaceDeclarationElementType();
	IElementType SUPER_EXPRESSION = new ElementTypeAsPsiFactory("SUPER_EXPRESSION", JavascriptLanguage.INSTANCE, JSSuperExpressionImpl.class);
	IElementType GENERIC_SIGNATURE = new ElementTypeAsPsiFactory("GENERIC_SIGNATURE", JavascriptLanguage.INSTANCE, JSGenericSignatureImpl.class);

	// Statements
	IElementType BLOCK_STATEMENT = new ElementTypeAsPsiFactory("BLOCK_STATEMENT", JavascriptLanguage.INSTANCE, JSBlockStatementImpl.class);
	IElementType LABELED_STATEMENT = new ElementTypeAsPsiFactory("LABELED_STATEMENT", JavascriptLanguage.INSTANCE, JSLabeledStatementImpl.class);
	IElementType EXPRESSION_STATEMENT = new ElementTypeAsPsiFactory("EXPRESSION_STATEMENT", JavascriptLanguage.INSTANCE,
			JSExpressionStatementImpl.class);
	IElementType YIELD_STATEMENT = new ElementTypeAsPsiFactory("YIELD_STATEMENT", JavascriptLanguage.INSTANCE, JSYieldStatementImpl.class);
	IElementType LET_STATEMENT = new ElementTypeAsPsiFactory("LET_STATEMENT", JavascriptLanguage.INSTANCE, JSLetStatementImpl.class);
	JSStubElementType<JSVarStatementStub, JSVarStatement> VAR_STATEMENT = new JSVarStatementElementType();
	IElementType EMPTY_STATEMENT = new ElementTypeAsPsiFactory("EMPTY_STATEMENT", JavascriptLanguage.INSTANCE, JSEmptyStatementImpl.class);
	IElementType IF_STATEMENT = new ElementTypeAsPsiFactory("IF_STATEMENT", JavascriptLanguage.INSTANCE, JSIfStatementImpl.class);
	IElementType CONTINUE_STATEMENT = new ElementTypeAsPsiFactory("CONTINUE_STATEMENT", JavascriptLanguage.INSTANCE, JSContinueStatementImpl.class);
	IElementType BREAK_STATEMENT = new ElementTypeAsPsiFactory("BREAK_STATEMENT", JavascriptLanguage.INSTANCE, JSBreakStatementImpl.class);
	IElementType WITH_STATEMENT = new ElementTypeAsPsiFactory("WITH_STATEMENT", JavascriptLanguage.INSTANCE, JSWithStatementImpl.class);
	IElementType RETURN_STATEMENT = new ElementTypeAsPsiFactory("RETURN_STATEMENT", JavascriptLanguage.INSTANCE, JSReturnStatementImpl.class);
	IElementType THROW_STATEMENT = new ElementTypeAsPsiFactory("THROW_STATEMENT", JavascriptLanguage.INSTANCE, JSThrowStatementImpl.class);
	IElementType TRY_STATEMENT = new ElementTypeAsPsiFactory("TRY_STATEMENT", JavascriptLanguage.INSTANCE, JSCatchBlockImpl.class);
	IElementType CATCH_BLOCK = new ElementTypeAsPsiFactory("CATCH_BLOCK", JavascriptLanguage.INSTANCE, JSCatchBlockImpl.class);
	IElementType CASE_CLAUSE = new ElementTypeAsPsiFactory("CASE_CLAUSE", JavascriptLanguage.INSTANCE, JSCaseClauseImpl.class);
	IElementType SWITCH_STATEMENT = new ElementTypeAsPsiFactory("SWITCH_STATEMENT", JavascriptLanguage.INSTANCE, JSSwitchStatementImpl.class);
	IElementType FOR_STATEMENT = new ElementTypeAsPsiFactory("FOR_STATEMENT", JavascriptLanguage.INSTANCE, JSForStatementImpl.class);
	IElementType FOR_IN_STATEMENT = new ElementTypeAsPsiFactory("FOR_IN_STATEMENT", JavascriptLanguage.INSTANCE, JSForInStatementImpl.class);
	IElementType WHILE_STATEMENT = new ElementTypeAsPsiFactory("WHILE_STATEMENT", JavascriptLanguage.INSTANCE, JSWhileStatementImpl.class);
	IElementType DOWHILE_STATEMENT = new ElementTypeAsPsiFactory("DO_WHILE_STATEMENT", JavascriptLanguage.INSTANCE, JSDoWhileStatementImpl.class);

	TokenSet STATEMENTS = TokenSet.create(BLOCK_STATEMENT, LABELED_STATEMENT, VAR_STATEMENT, EMPTY_STATEMENT, IF_STATEMENT, CONTINUE_STATEMENT,
			BREAK_STATEMENT, WITH_STATEMENT, RETURN_STATEMENT, THROW_STATEMENT, TRY_STATEMENT, SWITCH_STATEMENT, FOR_IN_STATEMENT, FOR_STATEMENT,
			WHILE_STATEMENT, DOWHILE_STATEMENT, EXPRESSION_STATEMENT, YIELD_STATEMENT, LET_STATEMENT, IMPORT_STATEMENT, PACKAGE_STATEMENT,
			USE_NAMESPACE_DIRECTIVE);

	TokenSet SOURCE_ELEMENTS = TokenSet.orSet(STATEMENTS, TokenSet.create(FUNCTION_DECLARATION, CLASS, NAMESPACE_DECLARATION, INCLUDE_DIRECTIVE,
			USE_NAMESPACE_DIRECTIVE));

	// Expressions
	IElementType THIS_EXPRESSION = new ElementTypeAsPsiFactory("THIS_EXPRESSION", JavascriptLanguage.INSTANCE, JSThisExpressionImpl.class);
	IElementType REFERENCE_EXPRESSION = new ElementTypeAsPsiFactory("REFERENCE_EXPRESSION", JavascriptLanguage.INSTANCE,
			JSReferenceExpressionImpl.class);
	IElementType LITERAL_EXPRESSION = new ElementTypeAsPsiFactory("LITERAL_EXPRESSION", JavascriptLanguage.INSTANCE, JSLiteralExpressionImpl.class);
	IElementType XML_LITERAL_EXPRESSION = new ElementTypeAsPsiFactory("XML_LITERAL_EXPRESSION", JavascriptLanguage.INSTANCE,
			JSXmlLiteralExpressionImpl.class);
	IElementType PARENTHESIZED_EXPRESSION = new ElementTypeAsPsiFactory("PARENTHESIZED_EXPRESSION", JavascriptLanguage.INSTANCE,
			JSParenthesizedExpressionImpl.class);
	IElementType ARRAY_LITERAL_EXPRESSION = new ElementTypeAsPsiFactory("ARRAY_LITERAL_EXPRESSION", JavascriptLanguage.INSTANCE,
			JSArrayLiteralExpressionImpl.class);
	IElementType PROPERTY = new ElementTypeAsPsiFactory("PROPERTY", JavascriptLanguage.INSTANCE, JSPropertyImpl.class);
	IElementType OBJECT_LITERAL_EXPRESSION = new ElementTypeAsPsiFactory("OBJECT_LITERAL_EXPRESSION", JavascriptLanguage.INSTANCE,
			JSObjectLiteralExpressionImpl.class);
	IElementType ASSIGNMENT_EXPRESSION = new ElementTypeAsPsiFactory("ASSIGNMENT_EXPRESSION", JavascriptLanguage.INSTANCE,
			JSAssignmentExpressionImpl.class);
	IElementType CONDITIONAL_EXPRESSION = new ElementTypeAsPsiFactory("CONDITIONAL_EXPRESSION", JavascriptLanguage.INSTANCE,
			JSConditionalExpressionImpl.class);
	IElementType BINARY_EXPRESSION = new ElementTypeAsPsiFactory("BINARY_EXPRESSION", JavascriptLanguage.INSTANCE, JSBinaryExpressionImpl.class);
	IElementType PREFIX_EXPRESSION = new ElementTypeAsPsiFactory("PREFIX_EXPRESSION", JavascriptLanguage.INSTANCE, JSPrefixExpressionImpl.class);
	IElementType POSTFIX_EXPRESSION = new ElementTypeAsPsiFactory("POSTFIX_EXPRESSION", JavascriptLanguage.INSTANCE, JSPostfixExpressionImpl.class);
	IElementType COMMA_EXPRESSION = new ElementTypeAsPsiFactory("COMMA_EXPRESSION", JavascriptLanguage.INSTANCE, JSCommaExpressionImpl.class);
	JSStubElementType<JSFunctionStub, JSFunction> FUNCTION_EXPRESSION = new JSFunctionElementType("FUNCTION_EXPRESSION")
	{
		@Override
		public JSFunction createPsi(@NotNull JSFunctionStub stub)
		{
			return new JSFunctionExpressionImpl(stub, this);
		}

		@NotNull
		@Override
		public PsiElement createElement(@NotNull ASTNode astNode)
		{
			return new JSFunctionExpressionImpl(astNode);
		}
	};
	IElementType NEW_EXPRESSION = new ElementTypeAsPsiFactory("NEW_EXPRESSION", JavascriptLanguage.INSTANCE, JSNewExpressionImpl.class);
	IElementType INDEXED_PROPERTY_ACCESS_EXPRESSION = new ElementTypeAsPsiFactory("INDEXED_PROPERTY_ACCESS_EXPRESSION", JavascriptLanguage.INSTANCE,
			JSIndexedPropertyAccessExpressionImpl.class);
	IElementType CALL_EXPRESSION = new ElementTypeAsPsiFactory("CALL_EXPRESSION", JavascriptLanguage.INSTANCE, JSCallExpressionImpl.class);
	IElementType DEFINITION_EXPRESSION = new ElementTypeAsPsiFactory("DEFINITION_EXPRESSION", JavascriptLanguage.INSTANCE,
			JSDefinitionExpressionImpl.class);
	IElementType LET_EXPRESSION = new ElementTypeAsPsiFactory("LET_EXPRESSION", JavascriptLanguage.INSTANCE, JSLetExpressionImpl.class);

	TokenSet EXPRESSIONS = TokenSet.create(THIS_EXPRESSION, REFERENCE_EXPRESSION, LITERAL_EXPRESSION, PARENTHESIZED_EXPRESSION,
			ARRAY_LITERAL_EXPRESSION, OBJECT_LITERAL_EXPRESSION, ASSIGNMENT_EXPRESSION, CONDITIONAL_EXPRESSION, BINARY_EXPRESSION,
			PREFIX_EXPRESSION, POSTFIX_EXPRESSION, COMMA_EXPRESSION, FUNCTION_EXPRESSION, NEW_EXPRESSION, INDEXED_PROPERTY_ACCESS_EXPRESSION,
			CALL_EXPRESSION, DEFINITION_EXPRESSION, XML_LITERAL_EXPRESSION, SUPER_EXPRESSION, LET_EXPRESSION);

}
