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

package com.intellij.lang.javascript.psi;

import org.mustbe.consulo.javascript.psi.JavaScriptLambdaExpression;
import com.intellij.psi.PsiElementVisitor;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Feb 3, 2005
 * Time: 2:25:12 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSElementVisitor extends PsiElementVisitor
{
	public void visitJSCallExpression(final JSCallExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSIndexedPropertyAccessExpression(final JSIndexedPropertyAccessExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSNewExpression(final JSNewExpression node)
	{
		visitJSCallExpression(node);
	}

	public void visitJSFunctionExpression(final JSFunctionExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSPrefixExpression(final JSPrefixExpression expression)
	{
		visitJSExpression(expression);
	}

	public void visitJSPostfixExpression(final JSPostfixExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSConditionalExpression(final JSConditionalExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSCommaExpression(final JSCommaExpression node)
	{
		visitJSBinaryExpression(node);
	}

	public void visitJSAssignmentExpression(final JSAssignmentExpression node)
	{
		visitJSBinaryExpression(node);
	}

	public void visitJSBinaryExpression(final JSBinaryExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSProperty(final JSProperty node)
	{
		visitJSElement(node);
	}

	public void visitJSObjectLiteralExpression(final JSObjectLiteralExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSArrayLiteralExpression(final JSArrayLiteralExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSParenthesizedExpression(final JSParenthesizedExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSReferenceExpression(final JSReferenceExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSDefinitionExpression(final JSDefinitionExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSLiteralExpression(final JSSimpleLiteralExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSThisExpression(final JSThisExpression node)
	{
		visitJSExpression(node);
	}

	public void visitJSForInStatement(final JSForInStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSForStatement(final JSForStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSDoWhileStatement(final JSDoWhileStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSWhileStatement(final JSWhileStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSCaseClause(final JSCaseClause node)
	{
		visitJSElement(node);
	}

	public void visitJSSwitchStatement(final JSSwitchStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSCatchBlock(final JSCatchBlock node)
	{
		visitJSElement(node);
	}

	public void visitJSTryStatement(final JSTryStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSThrowStatement(final JSThrowStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSReturnStatement(final JSReturnStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSWithStatement(final JSWithStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSBreakStatement(final JSBreakStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSContinueStatement(final JSContinueStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSIfStatement(final JSIfStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSEmptyStatement(final JSEmptyStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSVarStatement(final JSVarStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSExpressionStatement(final JSExpressionStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSLabeledStatement(final JSLabeledStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSBlock(final JSBlockStatement node)
	{
		visitJSStatement(node);
	}

	public void visitJSArgumentList(final JSArgumentList node)
	{
		visitJSElement(node);
	}

	public void visitJSParameter(final JSParameter parameter)
	{
		visitJSVariable(parameter);
	}

	public void visitJSVariable(final JSVariable node)
	{
		visitJSElement(node);
	}

	public void visitJSParameterList(final JSParameterList node)
	{
		visitJSElement(node);
	}

	public void visitJSElement(final JSElement node)
	{
		visitElement(node);
	}

	public void visitJSSourceElement(final JSElement node)
	{
		visitJSElement(node);
	}

	public void visitJSFunctionDeclaration(final JSFunction node)
	{
		visitJSSourceElement(node);
	}

	public void visitJSStatement(final JSStatement node)
	{
		visitJSSourceElement(node);
	}

	public void visitJSExpression(final JSExpression node)
	{
		visitJSElement(node);
	}

	public void visitJSAttributeList(final JSAttributeList attributeList)
	{
		visitJSElement(attributeList);
	}

	public void visitJSPackageStatement(final JSPackageStatement packageStatement)
	{
		visitJSStatement(packageStatement);
	}

	public void visitJSImportStatement(final JSImportStatement importStatement)
	{
		visitJSStatement(importStatement);
	}

	public void visitJSUseNamespaceDirective(final JSUseNamespaceDirective useNamespaceDirective)
	{
		visitJSStatement(useNamespaceDirective);
	}

	public void visitJSNamespaceDeclaration(final JSNamespaceDeclaration namespaceDeclaration)
	{
		visitJSStatement(namespaceDeclaration);
	}

	public void visitJSClass(final JSClass aClass)
	{
		visitJSElement(aClass);
	}

	public void visitJSReferenceList(final JSReferenceList referenceList)
	{
		visitJSElement(referenceList);
	}

	public void visitJSSuperExpression(final JSSuperExpression superExpression)
	{
		visitJSExpression(superExpression);
	}

	public void visitJSIncludeDirective(final JSIncludeDirective includeDirective)
	{
		visitJSStatement(includeDirective);
	}

	public void visitJSAttribute(final JSAttribute jsAttribute)
	{
		visitJSElement(jsAttribute);
	}

	public void visitJSAttributeNameValuePair(final JSAttributeNameValuePair attributeNameValuePair)
	{
	}

	public void visitJSYieldStatement(final JSYieldStatement statement)
	{
		visitJSStatement(statement);
	}

	public void visitJSLetStatement(final JSLetStatement statement)
	{
		visitJSStatement(statement);
	}

	public void visitJSLetExpression(final JSLetExpression expression)
	{
		visitJSExpression(expression);
	}

	public void visitJSGenericSignature(final JSGenericSignature signature)
	{
		visitJSElement(signature);
	}

	public void visitJSDocTagValue(final JSDocTagValue tagValue)
	{
		visitJSElement(tagValue);
	}

	public void visitJSDocTag(final JSDocTag docTag)
	{
		visitJSElement(docTag);
	}

	public void visitJSDocComment(final JSDocComment docComment)
	{
		visitComment(docComment);
	}

	public void visitLambdaExpression(JavaScriptLambdaExpression expression)
	{
		visitJSExpression(expression);
	}
}
