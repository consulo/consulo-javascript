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

import consulo.javascript.psi.JSSimpleLiteralExpression;
import consulo.javascript.psi.JavaScriptLambdaExpression;
import consulo.language.psi.PsiElementVisitor;

/**
 * @author max
 * @since 2005-02-03
 */
public class JSElementVisitor extends PsiElementVisitor {
    public void visitJSCallExpression(JSCallExpression node) {
        visitJSExpression(node);
    }

    public void visitJSIndexedPropertyAccessExpression(JSIndexedPropertyAccessExpression node) {
        visitJSExpression(node);
    }

    public void visitJSNewExpression(JSNewExpression node) {
        visitJSCallExpression(node);
    }

    public void visitJSFunctionExpression(JSFunctionExpression node) {
        visitJSExpression(node);
    }

    public void visitJSPrefixExpression(JSPrefixExpression expression) {
        visitJSExpression(expression);
    }

    public void visitJSPostfixExpression(JSPostfixExpression node) {
        visitJSExpression(node);
    }

    public void visitJSConditionalExpression(JSConditionalExpression node) {
        visitJSExpression(node);
    }

    public void visitJSCommaExpression(JSCommaExpression node) {
        visitJSBinaryExpression(node);
    }

    public void visitJSAssignmentExpression(JSAssignmentExpression node) {
        visitJSBinaryExpression(node);
    }

    public void visitJSBinaryExpression(JSBinaryExpression node) {
        visitJSExpression(node);
    }

    public void visitJSProperty(JSProperty node) {
        visitJSElement(node);
    }

    public void visitJSObjectLiteralExpression(JSObjectLiteralExpression node) {
        visitJSExpression(node);
    }

    public void visitJSArrayLiteralExpression(JSArrayLiteralExpression node) {
        visitJSExpression(node);
    }

    public void visitJSParenthesizedExpression(JSParenthesizedExpression node) {
        visitJSExpression(node);
    }

    public void visitJSReferenceExpression(JSReferenceExpression node) {
        visitJSExpression(node);
    }

    public void visitJSDefinitionExpression(JSDefinitionExpression node) {
        visitJSExpression(node);
    }

    public void visitJSLiteralExpression(JSSimpleLiteralExpression node) {
        visitJSExpression(node);
    }

    public void visitJSThisExpression(JSThisExpression node) {
        visitJSExpression(node);
    }

    public void visitJSForInStatement(JSForInStatement node) {
        visitJSStatement(node);
    }

    public void visitJSForStatement(JSForStatement node) {
        visitJSStatement(node);
    }

    public void visitJSDoWhileStatement(JSDoWhileStatement node) {
        visitJSStatement(node);
    }

    public void visitJSWhileStatement(JSWhileStatement node) {
        visitJSStatement(node);
    }

    public void visitJSCaseClause(JSCaseClause node) {
        visitJSElement(node);
    }

    public void visitJSSwitchStatement(JSSwitchStatement node) {
        visitJSStatement(node);
    }

    public void visitJSCatchBlock(JSCatchBlock node) {
        visitJSElement(node);
    }

    public void visitJSTryStatement(JSTryStatement node) {
        visitJSStatement(node);
    }

    public void visitJSThrowStatement(JSThrowStatement node) {
        visitJSStatement(node);
    }

    public void visitJSReturnStatement(JSReturnStatement node) {
        visitJSStatement(node);
    }

    public void visitJSWithStatement(JSWithStatement node) {
        visitJSStatement(node);
    }

    public void visitJSBreakStatement(JSBreakStatement node) {
        visitJSStatement(node);
    }

    public void visitJSContinueStatement(JSContinueStatement node) {
        visitJSStatement(node);
    }

    public void visitJSIfStatement(JSIfStatement node) {
        visitJSStatement(node);
    }

    public void visitJSEmptyStatement(JSEmptyStatement node) {
        visitJSStatement(node);
    }

    public void visitJSVarStatement(JSVarStatement node) {
        visitJSStatement(node);
    }

    public void visitJSExpressionStatement(JSExpressionStatement node) {
        visitJSStatement(node);
    }

    public void visitJSLabeledStatement(JSLabeledStatement node) {
        visitJSStatement(node);
    }

    public void visitJSBlock(JSBlockStatement node) {
        visitJSStatement(node);
    }

    public void visitJSArgumentList(JSArgumentList node) {
        visitJSElement(node);
    }

    public void visitJSParameter(JSParameter parameter) {
        visitJSVariable(parameter);
    }

    public void visitJSVariable(JSVariable node) {
        visitJSElement(node);
    }

    public void visitJSParameterList(JSParameterList node) {
        visitJSElement(node);
    }

    public void visitJSElement(JSElement node) {
        visitElement(node);
    }

    public void visitJSSourceElement(JSElement node) {
        visitJSElement(node);
    }

    public void visitJSFunctionDeclaration(JSFunction node) {
        visitJSSourceElement(node);
    }

    public void visitJSStatement(JSStatement node) {
        visitJSSourceElement(node);
    }

    public void visitJSExpression(JSExpression node) {
        visitJSElement(node);
    }

    public void visitJSAttributeList(JSAttributeList attributeList) {
        visitJSElement(attributeList);
    }

    public void visitJSPackageStatement(JSPackageStatement packageStatement) {
        visitJSStatement(packageStatement);
    }

    public void visitJSImportStatement(JSImportStatement importStatement) {
        visitJSStatement(importStatement);
    }

    public void visitJSUseNamespaceDirective(JSUseNamespaceDirective useNamespaceDirective) {
        visitJSStatement(useNamespaceDirective);
    }

    public void visitJSNamespaceDeclaration(JSNamespaceDeclaration namespaceDeclaration) {
        visitJSStatement(namespaceDeclaration);
    }

    public void visitJSClass(JSClass aClass) {
        visitJSElement(aClass);
    }

    public void visitJSClassExpression(JSClassExpression expression) {
        visitJSExpression(expression);
    }

    public void visitJSReferenceList(JSReferenceList referenceList) {
        visitJSElement(referenceList);
    }

    public void visitJSSuperExpression(JSSuperExpression superExpression) {
        visitJSExpression(superExpression);
    }

    public void visitJSIncludeDirective(JSIncludeDirective includeDirective) {
        visitJSStatement(includeDirective);
    }

    public void visitJSAttribute(JSAttribute jsAttribute) {
        visitJSElement(jsAttribute);
    }

    public void visitJSAttributeNameValuePair(JSAttributeNameValuePair attributeNameValuePair) {
    }

    public void visitJSYieldStatement(JSYieldStatement statement) {
        visitJSStatement(statement);
    }

    public void visitJSLetStatement(JSLetStatement statement) {
        visitJSStatement(statement);
    }

    public void visitJSLetExpression(JSLetExpression expression) {
        visitJSExpression(expression);
    }

    public void visitJSGenericSignature(JSGenericSignature signature) {
        visitJSElement(signature);
    }

    public void visitJSDocTagValue(JSDocTagValue tagValue) {
        visitJSElement(tagValue);
    }

    public void visitJSDocTag(JSDocTag docTag) {
        visitJSElement(docTag);
    }

    public void visitJSDocComment(JSDocComment docComment) {
        visitComment(docComment);
    }

    public void visitLambdaExpression(JavaScriptLambdaExpression expression) {
        visitJSExpression(expression);
    }
}
