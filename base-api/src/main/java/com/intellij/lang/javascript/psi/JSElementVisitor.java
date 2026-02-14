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
import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-02-03
 */
public class JSElementVisitor extends PsiElementVisitor {
    public void visitJSCallExpression(@Nonnull JSCallExpression node) {
        visitJSExpression(node);
    }

    public void visitJSIndexedPropertyAccessExpression(@Nonnull JSIndexedPropertyAccessExpression node) {
        visitJSExpression(node);
    }

    public void visitJSNewExpression(@Nonnull JSNewExpression node) {
        visitJSCallExpression(node);
    }

    public void visitJSFunctionExpression(@Nonnull JSFunctionExpression node) {
        visitJSExpression(node);
    }

    public void visitJSPrefixExpression(@Nonnull JSPrefixExpression expression) {
        visitJSExpression(expression);
    }

    public void visitJSPostfixExpression(@Nonnull JSPostfixExpression node) {
        visitJSExpression(node);
    }

    public void visitJSConditionalExpression(@Nonnull JSConditionalExpression node) {
        visitJSExpression(node);
    }

    public void visitJSCommaExpression(@Nonnull JSCommaExpression node) {
        visitJSBinaryExpression(node);
    }

    public void visitJSAssignmentExpression(@Nonnull JSAssignmentExpression node) {
        visitJSBinaryExpression(node);
    }

    public void visitJSBinaryExpression(@Nonnull JSBinaryExpression node) {
        visitJSExpression(node);
    }

    public void visitJSProperty(@Nonnull JSProperty node) {
        visitJSElement(node);
    }

    public void visitJSObjectLiteralExpression(@Nonnull JSObjectLiteralExpression node) {
        visitJSExpression(node);
    }

    public void visitJSArrayLiteralExpression(@Nonnull JSArrayLiteralExpression node) {
        visitJSExpression(node);
    }

    public void visitJSParenthesizedExpression(@Nonnull JSParenthesizedExpression node) {
        visitJSExpression(node);
    }

    public void visitJSReferenceExpression(@Nonnull JSReferenceExpression node) {
        visitJSExpression(node);
    }

    public void visitJSDefinitionExpression(@Nonnull JSDefinitionExpression node) {
        visitJSExpression(node);
    }

    public void visitJSLiteralExpression(@Nonnull JSSimpleLiteralExpression node) {
        visitJSExpression(node);
    }

    public void visitJSThisExpression(@Nonnull JSThisExpression node) {
        visitJSExpression(node);
    }

    public void visitJSForInStatement(@Nonnull JSForInStatement node) {
        visitJSStatement(node);
    }

    public void visitJSForStatement(@Nonnull JSForStatement node) {
        visitJSStatement(node);
    }

    public void visitJSDoWhileStatement(@Nonnull JSDoWhileStatement node) {
        visitJSStatement(node);
    }

    public void visitJSWhileStatement(@Nonnull JSWhileStatement node) {
        visitJSStatement(node);
    }

    public void visitJSCaseClause(@Nonnull JSCaseClause node) {
        visitJSElement(node);
    }

    public void visitJSSwitchStatement(@Nonnull JSSwitchStatement node) {
        visitJSStatement(node);
    }

    public void visitJSCatchBlock(@Nonnull JSCatchBlock node) {
        visitJSElement(node);
    }

    public void visitJSTryStatement(@Nonnull JSTryStatement node) {
        visitJSStatement(node);
    }

    public void visitJSThrowStatement(@Nonnull JSThrowStatement node) {
        visitJSStatement(node);
    }

    public void visitJSReturnStatement(@Nonnull JSReturnStatement node) {
        visitJSStatement(node);
    }

    public void visitJSWithStatement(@Nonnull JSWithStatement node) {
        visitJSStatement(node);
    }

    public void visitJSBreakStatement(@Nonnull JSBreakStatement node) {
        visitJSStatement(node);
    }

    public void visitJSContinueStatement(@Nonnull JSContinueStatement node) {
        visitJSStatement(node);
    }

    public void visitJSIfStatement(@Nonnull JSIfStatement node) {
        visitJSStatement(node);
    }

    public void visitJSEmptyStatement(@Nonnull JSEmptyStatement node) {
        visitJSStatement(node);
    }

    public void visitJSVarStatement(@Nonnull JSVarStatement node) {
        visitJSStatement(node);
    }

    public void visitJSExpressionStatement(@Nonnull JSExpressionStatement node) {
        visitJSStatement(node);
    }

    public void visitJSLabeledStatement(@Nonnull JSLabeledStatement node) {
        visitJSStatement(node);
    }

    public void visitJSBlock(@Nonnull JSBlockStatement node) {
        visitJSStatement(node);
    }

    public void visitJSArgumentList(@Nonnull JSArgumentList node) {
        visitJSElement(node);
    }

    public void visitJSParameter(@Nonnull JSParameter parameter) {
        visitJSVariable(parameter);
    }

    public void visitJSVariable(@Nonnull JSVariable node) {
        visitJSElement(node);
    }

    public void visitJSParameterList(@Nonnull JSParameterList node) {
        visitJSElement(node);
    }

    public void visitJSElement(@Nonnull JSElement node) {
        visitElement(node);
    }

    public void visitJSSourceElement(@Nonnull JSElement node) {
        visitJSElement(node);
    }

    public void visitJSFunctionDeclaration(@Nonnull JSFunction node) {
        visitJSSourceElement(node);
    }

    public void visitJSStatement(@Nonnull JSStatement node) {
        visitJSSourceElement(node);
    }

    public void visitJSExpression(@Nonnull JSExpression node) {
        visitJSElement(node);
    }

    public void visitJSAttributeList(@Nonnull JSAttributeList attributeList) {
        visitJSElement(attributeList);
    }

    public void visitJSPackageStatement(@Nonnull JSPackageStatement packageStatement) {
        visitJSStatement(packageStatement);
    }

    public void visitJSImportStatement(@Nonnull JSImportStatement importStatement) {
        visitJSStatement(importStatement);
    }

    public void visitJSUseNamespaceDirective(@Nonnull JSUseNamespaceDirective useNamespaceDirective) {
        visitJSStatement(useNamespaceDirective);
    }

    public void visitJSNamespaceDeclaration(@Nonnull JSNamespaceDeclaration namespaceDeclaration) {
        visitJSStatement(namespaceDeclaration);
    }

    public void visitJSClass(@Nonnull JSClass aClass) {
        visitJSElement(aClass);
    }

    public void visitJSClassExpression(@Nonnull JSClassExpression expression) {
        visitJSExpression(expression);
    }

    public void visitJSReferenceList(@Nonnull JSReferenceList referenceList) {
        visitJSElement(referenceList);
    }

    public void visitJSSuperExpression(@Nonnull JSSuperExpression superExpression) {
        visitJSExpression(superExpression);
    }

    public void visitJSIncludeDirective(@Nonnull JSIncludeDirective includeDirective) {
        visitJSStatement(includeDirective);
    }

    public void visitJSAttribute(@Nonnull JSAttribute jsAttribute) {
        visitJSElement(jsAttribute);
    }

    public void visitJSAttributeNameValuePair(@Nonnull JSAttributeNameValuePair attributeNameValuePair) {
    }

    public void visitJSYieldStatement(@Nonnull JSYieldStatement statement) {
        visitJSStatement(statement);
    }

    public void visitJSLetStatement(@Nonnull JSLetStatement statement) {
        visitJSStatement(statement);
    }

    public void visitJSLetExpression(@Nonnull JSLetExpression expression) {
        visitJSExpression(expression);
    }

    public void visitJSGenericSignature(@Nonnull JSGenericSignature signature) {
        visitJSElement(signature);
    }

    public void visitJSDocTagValue(@Nonnull JSDocTagValue tagValue) {
        visitJSElement(tagValue);
    }

    public void visitJSDocTag(@Nonnull JSDocTag docTag) {
        visitJSElement(docTag);
    }

    public void visitJSDocComment(@Nonnull JSDocComment docComment) {
        visitComment(docComment);
    }

    public void visitLambdaExpression(@Nonnull JavaScriptLambdaExpression expression) {
        visitJSExpression(expression);
    }
}
