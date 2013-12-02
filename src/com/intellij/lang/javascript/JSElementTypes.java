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
import com.intellij.lang.Language;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.stubs.*;
import com.intellij.lang.javascript.types.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.ILazyParseableElementType;
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
	IFileElementType FILE = new JSFileElementType(JavaScriptSupportLoader.JAVASCRIPT.getLanguage());
	IFileElementType JSON_FILE = new JSFileElementType(JavaScriptSupportLoader.JSON);
	IFileElementType ECMA4_FILE = new JSFileElementType(JavaScriptSupportLoader.ECMA_SCRIPT_L4);
	IFileElementType GWT_FILE = new JSFileElementType(JavaScriptSupportLoader.GWT_DIALECT);

	IElementType EMBEDDED_CONTENT = new ILazyParseableElementType("EMBEDDED_CONTENT", Language.findInstance(JavascriptLanguage.class))
	{
		@NotNull
		@Override
		public Language getLanguage()
		{
			return JavaScriptSupportLoader.JS_IN_HTML_DIALECT;
		}
	};
	IElementType EMBEDDED_EXPRESSION = new JSElementType("EMBEDDED_EXPRESSION");
	JSStubElementType<JSFunctionStub, JSFunction> FUNCTION_DECLARATION = new JSFunctionElementType();
	JSStubElementType<JSParameterListStub, JSParameterList> PARAMETER_LIST = new JSParameterListElementType();

	JSStubElementType<JSParameterStub, JSParameter> FORMAL_PARAMETER = new JSParameterElementType();
	JSStubElementType<JSVariableStub, JSVariable> VARIABLE = new JSVariableElementType();
	IElementType ARGUMENT_LIST = new JSElementType("ARGUMENT_LIST");

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
	IElementType SUPER_EXPRESSION = new JSElementType("SUPER_EXPRESSION");
	IElementType GENERIC_SIGNATURE = new JSElementType("GENERIC_SIGNATURE");

	// Statements
	IElementType BLOCK_STATEMENT = new JSElementType("BLOCK_STATEMENT");
	IElementType LABELED_STATEMENT = new JSElementType("LABELED_STATEMENT");
	IElementType EXPRESSION_STATEMENT = new JSElementType("EXPRESSION_STATEMENT");
	IElementType YIELD_STATEMENT = new JSElementType("YIELD_STATEMENT");
	IElementType LET_STATEMENT = new JSElementType("LET_STATEMENT");
	JSStubElementType<JSVarStatementStub, JSVarStatement> VAR_STATEMENT = new JSVarStatementElementType();
	IElementType EMPTY_STATEMENT = new JSElementType("EMPTY_STATEMENT");
	IElementType IF_STATEMENT = new JSElementType("IF_STATEMENT");
	IElementType CONTINUE_STATEMENT = new JSElementType("CONTINUE_STATEMENT");
	IElementType BREAK_STATEMENT = new JSElementType("BREAK_STATEMENT");
	IElementType WITH_STATEMENT = new JSElementType("WITH_STATEMENT");
	IElementType RETURN_STATEMENT = new JSElementType("RETURN_STATEMENT");
	IElementType THROW_STATEMENT = new JSElementType("THROW_STATEMENT");
	IElementType TRY_STATEMENT = new JSElementType("TRY_STATEMENT");
	IElementType CATCH_BLOCK = new JSElementType("CATCH_BLOCK");
	IElementType CASE_CLAUSE = new JSElementType("CASE_CLAUSE");
	IElementType SWITCH_STATEMENT = new JSElementType("SWITCH_STATEMENT");
	IElementType FOR_STATEMENT = new JSElementType("FOR_STATEMENT");
	IElementType FOR_IN_STATEMENT = new JSElementType("FOR_IN_STATEMENT");
	IElementType WHILE_STATEMENT = new JSElementType("WHILE_STATEMENT");
	IElementType DOWHILE_STATEMENT = new JSElementType("DO_WHILE_STATEMENT");

	TokenSet STATEMENTS = TokenSet.create(BLOCK_STATEMENT, LABELED_STATEMENT, VAR_STATEMENT, EMPTY_STATEMENT, IF_STATEMENT, CONTINUE_STATEMENT,
			BREAK_STATEMENT, WITH_STATEMENT, RETURN_STATEMENT, THROW_STATEMENT, TRY_STATEMENT, SWITCH_STATEMENT, FOR_IN_STATEMENT, FOR_STATEMENT,
			WHILE_STATEMENT, DOWHILE_STATEMENT, EXPRESSION_STATEMENT, YIELD_STATEMENT, LET_STATEMENT, IMPORT_STATEMENT, PACKAGE_STATEMENT,
			USE_NAMESPACE_DIRECTIVE);

	TokenSet SOURCE_ELEMENTS = TokenSet.orSet(STATEMENTS, TokenSet.create(FUNCTION_DECLARATION, CLASS, NAMESPACE_DECLARATION, INCLUDE_DIRECTIVE,
			USE_NAMESPACE_DIRECTIVE));

	// Expressions
	IElementType THIS_EXPRESSION = new JSElementType("THIS_EXPRESSION");
	IElementType REFERENCE_EXPRESSION = new JSElementType("REFERENCE_EXPRESSION");
	IElementType GWT_REFERENCE_EXPRESSION = new JSElementType("GWT_REFERENCE_EXPRESSION");
	IElementType LITERAL_EXPRESSION = new JSElementType("LITERAL_EXPRESSION");
	IElementType XML_LITERAL_EXPRESSION = new JSElementType("XML_LITERAL_EXPRESSION");
	IElementType PARENTHESIZED_EXPRESSION = new JSElementType("PARENTHESIZED_EXPRESSION");
	IElementType ARRAY_LITERAL_EXPRESSION = new JSElementType("ARRAY_LITERAL_EXPRESSION");
	IElementType PROPERTY = new JSElementType("PROPERTY");
	IElementType OBJECT_LITERAL_EXPRESSION = new JSElementType("OBJECT_LITERAL_EXPRESSION");
	IElementType ASSIGNMENT_EXPRESSION = new JSElementType("ASSIGNMENT_EXPRESSION");
	IElementType CONDITIONAL_EXPRESSION = new JSElementType("CONDITIONAL_EXPRESSION");
	IElementType BINARY_EXPRESSION = new JSElementType("BINARY_EXPRESSION");
	IElementType PREFIX_EXPRESSION = new JSElementType("PREFIX_EXPRESSION");
	IElementType POSTFIX_EXPRESSION = new JSElementType("POSTFIX_EXPRESSION");
	IElementType COMMA_EXPRESSION = new JSElementType("COMMA_EXPRESSION");
	JSStubElementType<JSFunctionExpressionStub, JSFunctionExpression> FUNCTION_EXPRESSION = new JSFunctionExpressionElementType();
	IElementType NEW_EXPRESSION = new JSElementType("NEW_EXPRESSION");
	IElementType INDEXED_PROPERTY_ACCESS_EXPRESSION = new JSElementType("INDEXED_PROPERTY_ACCESS_EXPRESSION");
	IElementType CALL_EXPRESSION = new JSElementType("CALL_EXPRESSION");
	IElementType DEFINITION_EXPRESSION = new JSElementType("DEFINITION_EXPRESSION");
	IElementType LET_EXPRESSION = new JSElementType("LET_EXPRESSION");

	TokenSet EXPRESSIONS = TokenSet.create(THIS_EXPRESSION, REFERENCE_EXPRESSION, LITERAL_EXPRESSION, PARENTHESIZED_EXPRESSION,
			ARRAY_LITERAL_EXPRESSION, OBJECT_LITERAL_EXPRESSION, ASSIGNMENT_EXPRESSION, CONDITIONAL_EXPRESSION, BINARY_EXPRESSION, PREFIX_EXPRESSION,
			POSTFIX_EXPRESSION, COMMA_EXPRESSION, FUNCTION_EXPRESSION, NEW_EXPRESSION, INDEXED_PROPERTY_ACCESS_EXPRESSION, CALL_EXPRESSION,
			DEFINITION_EXPRESSION, XML_LITERAL_EXPRESSION, SUPER_EXPRESSION, LET_EXPRESSION);

}
