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
package com.intellij.lang.javascript.impl.validation;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.impl.JSPackageStatementImpl;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.util.JSUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.codeEditor.Editor;
import consulo.document.util.TextRange;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import consulo.javascript.psi.JavaScriptLambdaExpression;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.editor.FileModificationService;
import consulo.language.editor.annotation.*;
import consulo.language.editor.impl.intention.RenameFileFix;
import consulo.language.editor.inspection.LocalQuickFix;
import consulo.language.editor.inspection.LocalQuickFixProvider;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.editor.intention.IntentionAction;
import consulo.language.editor.intention.SyntheticIntentionAction;
import consulo.language.editor.template.Template;
import consulo.language.editor.template.TemplateManager;
import consulo.language.editor.template.macro.CompleteMacro;
import consulo.language.editor.template.macro.MacroCallNode;
import consulo.language.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.module.content.ProjectRootManager;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.util.lang.ref.SimpleReference;
import consulo.virtualFileSystem.VirtualFile;
import consulo.xml.psi.xml.XmlAttributeValue;
import consulo.xml.psi.xml.XmlTagChild;
import jakarta.annotation.Nonnull;

import java.util.*;

/**
 * @author max
 * @author maxim.mossienko
 */
public class JSAnnotatingVisitor extends JSElementVisitor implements Annotator {
    private AnnotationHolder myHolder;

    @Override
    public synchronized void annotate(PsiElement psiElement, @Nonnull AnnotationHolder holder) {
        myHolder = holder;
        psiElement.accept(this);
        myHolder = null;
    }

    @Override
    @RequiredReadAction
    public void visitJSAttributeNameValuePair(JSAttributeNameValuePair attributeNameValuePair) {
        checkReferences(attributeNameValuePair, HighlightSeverity.ERROR);
    }

    @Override
    @RequiredReadAction
    public void visitJSIncludeDirective(JSIncludeDirective includeDirective) {
        checkReferences(includeDirective, HighlightSeverity.ERROR);
    }

    @Override
    @RequiredReadAction
    public void visitJSLiteralExpression(JSSimpleLiteralExpression node) {
        checkReferences(node, HighlightSeverity.ERROR);
    }

    @RequiredReadAction
    private void checkReferences(PsiElement includeDirective, HighlightSeverity kind) {
        for (PsiReference ref : includeDirective.getReferences()) {
            if (!ref.isSoft() && hasBadResolve(ref)) {
                TextRange elementRange = ref.getElement().getTextRange();
                TextRange textRange = ref.getRangeInElement();

                TextRange range = new TextRange(
                    elementRange.getStartOffset() + textRange.getStartOffset(),
                    elementRange.getStartOffset() + textRange.getEndOffset()
                );
                LocalizeValue value = ((EmptyResolveMessageProvider)ref).buildUnresolvedMessage(ref.getCanonicalText());
                AnnotationBuilder builder = myHolder.newAnnotation(kind, value).range(range);

                if (ref instanceof LocalQuickFixProvider localQuickFixProvider) {
                    for (LocalQuickFix fix : localQuickFixProvider.getQuickFixes()) {
                        if (fix instanceof IntentionAction intentionAction) {
                            builder = builder.withFix(intentionAction);
                        }
                    }
                }
                builder.create();
            }
        }
    }

    @RequiredReadAction
    private boolean hasBadResolve(PsiReference ref) {
        return ref instanceof PsiPolyVariantReference psiPolyVariantReference
            ? psiPolyVariantReference.multiResolve(false).length == 0
            : ref.resolve() == null;
    }

    @Override
    @RequiredReadAction
    public void visitJSCallExpression(JSCallExpression node) {
        JSExpression methodExpression = node.getMethodExpression();

        if (methodExpression instanceof JSLiteralExpression literal) {
            myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptParserMessageExpectedFunctionName())
                .range(literal)
                .create();
        }
    }

    @Override
    @RequiredReadAction
    public void visitJSDocTagValue(@Nonnull JSDocTagValue tagValue) {
        checkReferences(tagValue, HighlightSeverity.WARNING);
    }

    @Override
    @RequiredReadAction
    public void visitJSDocTag(@Nonnull JSDocTag tagValue) {
        checkReferences(tagValue, HighlightSeverity.WARNING);
    }

    @Override
    @RequiredReadAction
    public void visitJSReferenceList(JSReferenceList referenceList) {
        JSClass jsClass = (JSClass)referenceList.getParent();
        if (JSResolveUtil.isArtificialClassUsedForReferenceList(jsClass)) {
            return; // implements="MyInterface" in mxml has artificial class created
        }

        boolean withinExtends = jsClass.getExtendsList() == referenceList;
        boolean withinImplements = jsClass.getImplementsList() == referenceList;

        if (withinImplements && jsClass.isInterface()) {
            myHolder.newAnnotation(
                    HighlightSeverity.ERROR,
                    JavaScriptLocalize.javascriptValidationMessageImplementsForInterfaceNotAllowed()
                )
                .range(referenceList)
                .create();
            return;
        }

        Map<String, JSReferenceExpression> nameToExprMap = new HashMap<>();

        JSReferenceExpression[] referenceExpressions = referenceList.getExpressions();
        if (referenceExpressions != null) {
            for (JSReferenceExpression expr : referenceExpressions) {
                String s = expr.getReferencedName();
                if (s != null) {
                    nameToExprMap.put(s, expr);
                }
            }
        }

        for (JSClass clazz : referenceList.getReferencedClasses()) {
            boolean isInterface = clazz.isInterface();
            JSReferenceExpression expr = nameToExprMap.get(clazz.getName());

            if (!isInterface && withinImplements) {
                myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptValidationMessageInterfaceNameExpectedHere())
                    .range(expr)
                    .create();
            }
            else if (withinExtends && isInterface != jsClass.isInterface()) {
                myHolder.newAnnotation(
                        HighlightSeverity.ERROR,
                        isInterface
                            ? JavaScriptLocalize.javascriptValidationMessageClassNameExpectedHere()
                            : JavaScriptLocalize.javascriptValidationMessageInterfaceNameExpectedHere()
                    )
                    .range(expr)
                    .create();
            }
            if (clazz == jsClass) {
                myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptValidationMessageCircularDependency())
                    .range(expr)
                    .withFix(new RemoveASTNodeFix(referenceList.getNode(), JavaScriptLocalize.javascriptFixRemoveCircularDependency()))
                    .create();
            }
        }

        if (withinImplements) {
            checkImplementedMethods(jsClass, myHolder);
        }
    }

    public static void checkImplementedMethods(final JSClass jsClass, final AnnotationHolder holder) {
        JSResolveUtil.CollectMethodsToImplementProcessor implementedMethodProcessor = new ImplementedMethodProcessor(jsClass) {
            ImplementMethodsFix implementMethodsFix = null;

            @Override
            @RequiredReadAction
            protected void addNonimplementedFunction(JSFunction function) {
                PsiElement node = myJsClass.getNameIdentifier();
                if (node == null) {
                    return;
                }
                if (implementMethodsFix == null) {
                    implementMethodsFix = new ImplementMethodsFix(myJsClass);
                }
                implementMethodsFix.addElementToProcess(function);
                holder.newAnnotation(
                        HighlightSeverity.ERROR,
                        JavaScriptLocalize.javascriptValidationMessageInterfaceMethodNotImplemented(
                            function.getName(),
                            ((JSClass)JSResolveUtil.findParent(function)).getQualifiedName()
                        )
                    )
                    .range(node)
                    .withFix(implementMethodsFix)
                    .create();
            }

            @Override
            @RequiredReadAction
            protected void addImplementedFunction(JSFunction interfaceFunction, JSFunction implementationFunction) {
                final JSAttributeList attributeList = implementationFunction.getAttributeList();
                if (attributeList == null || attributeList.getAccessType() != JSAttributeList.AccessType.PUBLIC) {
                    ASTNode node = findElementForAccessModifierError(implementationFunction, attributeList);
                    holder.newAnnotation(
                            HighlightSeverity.ERROR,
                            JavaScriptLocalize.javascriptValidationMessageInterfaceMethodInvalidAccessModifier()
                        )
                        .range(node)
                        .create(); // TODO: quickfix
                }

                SignatureMatchResult incompatibleSignature = checkCompatibleSignature(implementationFunction, interfaceFunction);

                if (incompatibleSignature != SignatureMatchResult.COMPATIBLE_SIGNATURE) {
                    PsiElement parent = JSResolveUtil.findParent(implementationFunction);
                    if (parent instanceof JSFile jsFile) {
                        parent = JSResolveUtil.getClassReferenceForXmlFromContext(jsFile);
                    }

                    if (parent != myJsClass) {
                        // some parent incorrectly implements method from our interface
                        addNonimplementedFunction(interfaceFunction);
                        return;
                    }

                    if (incompatibleSignature == SignatureMatchResult.PARAMETERS_DIFFERS) {
                        JSParameterList parameterList = implementationFunction.getParameterList();
                        JSParameterList expectedParameterList = interfaceFunction.getParameterList();
                        holder.newAnnotation(
                                HighlightSeverity.ERROR,
                                JavaScriptLocalize.javascriptValidationMessageInterfaceMethodInvalidSignature(
                                    expectedParameterList != null ? expectedParameterList.getText() : "()"
                                )
                            )
                            .range(parameterList.getNode())
                            .create(); // TODO: quickfix
                    }
                    else if (incompatibleSignature == SignatureMatchResult.RETURN_TYPE_DIFFERS) {
                        PsiElement implementationReturnTypeExpr = implementationFunction.getReturnTypeElement();
                        PsiElement interfaceReturnTypeExpr = interfaceFunction.getReturnTypeElement();
                        holder.newAnnotation(
                                HighlightSeverity.ERROR,
                                JavaScriptLocalize.javascriptValidationMessageInterfaceMethodInvalidSignature2(
                                    interfaceReturnTypeExpr != null ? interfaceReturnTypeExpr.getText() : "*"
                                )
                            )
                            .range(
                                implementationReturnTypeExpr != null
                                    ? implementationReturnTypeExpr
                                    : implementationFunction.getNameIdentifier()
                            )
                            .create(); // TODO: quickfix
                    }
                }
            }
        };
        JSResolveUtil.processInterfaceMethods(jsClass, implementedMethodProcessor);
    }

    @RequiredReadAction
    private static ASTNode findElementForAccessModifierError(@Nonnull JSFunction o, JSAttributeList attributeList) {
        if (attributeList != null) {
            PsiElement accessTypeElement = attributeList.findAccessTypeElement();
            if (accessTypeElement != null) {
                return accessTypeElement.getNode();
            }
        }
        PsiElement nameIdentifier = o.getNameIdentifier();
        return nameIdentifier == null ? null : nameIdentifier.getNode();
    }

    @Override
    @RequiredReadAction
    public void visitJSAttributeList(JSAttributeList attributeList) {
        PsiElement parentForCheckingNsOrAccessModifier = null;

        PsiElement namespaceElement = attributeList.getNamespaceElement();
        PsiElement accessTypeElement = attributeList.findAccessTypeElement();
        PsiElement namespaceOrAccessModifierElement = namespaceElement;

        if (namespaceOrAccessModifierElement == null) {
            namespaceOrAccessModifierElement = accessTypeElement;
        }
        else if (accessTypeElement != null) {
            myHolder.newAnnotation(
                    HighlightSeverity.ERROR,
                    JavaScriptLocalize.javascriptValidationMessageUseNamespaceReferenceOrAccessModifier()
                )
                .range(namespaceOrAccessModifierElement)
                .withFix(new RemoveASTNodeFix(
                    namespaceOrAccessModifierElement.getNode(),
                    JavaScriptLocalize.javascriptFixRemoveNamespaceReference()
                ))
                .create();

            myHolder.newAnnotation(
                    HighlightSeverity.ERROR,
                    JavaScriptLocalize.javascriptValidationMessageUseNamespaceReferenceOrAccessModifier()
                )
                .range(accessTypeElement)
                .withFix(new RemoveASTNodeFix(accessTypeElement.getNode(), JavaScriptLocalize.javascriptFixRemoveNamespaceReference()))
                .create();
        }

        if (namespaceOrAccessModifierElement != null) {
            parentForCheckingNsOrAccessModifier = JSResolveUtil.findParent(attributeList.getParent());
            if (!(parentForCheckingNsOrAccessModifier instanceof JSClass)) {
                String typeElementText;
                boolean nodeUnderPackage;

                if (((!(nodeUnderPackage = (parentForCheckingNsOrAccessModifier instanceof JSPackageStatement))
                    && (!(parentForCheckingNsOrAccessModifier instanceof JSFile)
                    || attributeList.getAccessType() != JSAttributeList.AccessType.PACKAGE_LOCAL))
                    || (!"public".equals(typeElementText = namespaceOrAccessModifierElement.getText()))
                    && !"internal".equals(typeElementText))) {
                    boolean nsRef = namespaceOrAccessModifierElement instanceof JSReferenceExpression;
                    myHolder.newAnnotation(
                            HighlightSeverity.ERROR,
                            nodeUnderPackage ?
                                JavaScriptLocalize.javascriptValidationMessageAccessModifierAllowedOnlyForPackageMembers()
                                : nsRef
                                ? JavaScriptLocalize.javascriptValidationMessageNamespaceAllowedOnlyForClassMembers()
                                : JavaScriptLocalize.javascriptValidationMessageAccessModifierAllowedOnlyForClassMembers()
                        )
                        .range(namespaceOrAccessModifierElement)
                        .withFix(new RemoveASTNodeFix(
                            namespaceOrAccessModifierElement.getNode(),
                            nsRef
                                ? JavaScriptLocalize.javascriptFixRemoveNamespaceReference()
                                : JavaScriptLocalize.javascriptFixRemoveAccessModifier()
                        ))
                        .create();
                }
            }
            else if (((JSClass)parentForCheckingNsOrAccessModifier).isInterface()) {
                if (attributeList.getAccessType() != JSAttributeList.AccessType.PACKAGE_LOCAL
                    || attributeList.getNode().findChildByType(JSTokenTypes.INTERNAL_KEYWORD) != null
                ) {
                    ASTNode astNode = attributeList.getNode().findChildByType(JSTokenTypes.ACCESS_MODIFIERS);
                    myHolder.newAnnotation(
                            HighlightSeverity.ERROR,
                            JavaScriptLocalize.javascriptValidationMessageInterfaceMembersCannotHaveAccessModifiers()
                        )
                        .range(astNode)
                        .withFix(new RemoveASTNodeFix(astNode, JavaScriptLocalize.javascriptFixRemoveAccessModifier()))
                        .create();
                }
            }
        }
    }

    @Override
    @RequiredReadAction
    public void visitJSReferenceExpression(JSReferenceExpression node) {
        PsiElement parent = node.getParent();

        if (parent instanceof JSNamedElement namedElement) {
            PsiElement nameIdentifier = namedElement.getNameIdentifier();

            if (nameIdentifier != null && nameIdentifier == node) {
                if (parent instanceof JSPackageStatement packageStatement) {
                    checkPackageStatement(packageStatement);
                }
                else if (!(parent instanceof JSImportStatement) && parent.getParent() instanceof JSPackageStatement) {
                    checkNamedObjectIsInCorrespondingFile(namedElement);
                }
                else if (parent instanceof JSFunction function) {
                    if (function.isConstructor()) {
                        JSClass clazz;
                        if (parent.getParent() instanceof JSClass jsClass) {
                            clazz = jsClass;
                        }
                        else {
                            assert parent.getParent() instanceof JSFile;
                            clazz = JSResolveUtil.getXmlBackedClass((JSFile)parent.getParent());
                            assert clazz != null;
                        }

                        checkMissedSuperCall(node, function, clazz);
                    }
                    else if (function.isSetProperty()) {
                        String typeString = function.getReturnTypeString();

                        if (typeString != null && !"void".equals(typeString)) {
                            // TODO: fix!
                            myHolder.newAnnotation(
                                    HighlightSeverity.ERROR,
                                    JavaScriptLocalize.javascriptValidationMessageSetMethodShouldBeVoidOrWithoutType()
                                )
                                .range(function.getReturnTypeElement())
                                .create();
                        }

                        JSParameterList parameterList = function.getParameterList();
                        if (parameterList != null && parameterList.getParameters().length != 1) {
                            // TODO: fix!
                            myHolder.newAnnotation(
                                    HighlightSeverity.ERROR,
                                    JavaScriptLocalize.javascriptValidationMessageSetMethodShouldHaveOneParameter()
                                )
                                .range(parameterList)
                                .create();
                        }
                    }
                    else if (function.isGetProperty()) {
                        String typeString = function.getReturnTypeString();

                        if (typeString == null || "void".equals(typeString)) {
                            // TODO: fix!
                            myHolder.newAnnotation(
                                    HighlightSeverity.ERROR,
                                    JavaScriptLocalize.javascriptValidationMessageGetMethodShouldBeValidType(
                                        typeString != null ? typeString : "empty"
                                    )
                                )
                                .range(typeString != null ? function.getReturnTypeElement() : nameIdentifier)
                                .create();
                        }

                        JSParameterList parameterList = function.getParameterList();
                        if (parameterList != null && parameterList.getParameters().length != 0) {
                            // TODO: fix!
                            myHolder.newAnnotation(
                                    HighlightSeverity.ERROR,
                                    JavaScriptLocalize.javascriptValidationMessageGetMethodShouldHaveNoParameter()
                                )
                                .range(parameterList)
                                .create();
                        }
                    }
                }

                if (parent instanceof JSClass jsClass) {
                    JSFunction constructor = jsClass.findFunctionByName(jsClass.getName());
                    if (constructor == null) {
                        checkMissedSuperCall(node, constructor, jsClass);
                    }

                    PsiElement clazzParent = jsClass.getParent();
                    if (!(clazzParent instanceof JSPackageStatement || clazzParent instanceof JSFile)) {
                        myHolder.newAnnotation(
                                HighlightSeverity.ERROR,
                                JavaScriptLocalize.javascriptValidationMessageNestedClassesAreNotAllowed()
                            )
                            .range(node)
                            .create();
                    }
                }
            }
        }

        if (node.getQualifier() == null && !(parent instanceof JSCallExpression) && "arguments".equals(node.getText())) {
            JSFunction fun = PsiTreeUtil.getParentOfType(node, JSFunction.class);
            if (fun == null) {
                myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptValidationMessageArgumentsOutOfFunction())
                    .range(node)
                    .create();
            }
            else {
                JSParameterList parameterList = fun.getParameterList();
                if (parameterList != null) {
                    for (JSParameter p : parameterList.getParameters()) {
                        if (p.isRest()) {
                            myHolder.newAnnotation(
                                    HighlightSeverity.ERROR,
                                    JavaScriptLocalize.javascriptValidationMessageArgumentsWithRestParameter()
                                )
                                .range(node);
                        }
                    }
                }
            }
        }
    }

    @RequiredReadAction
    private void checkMissedSuperCall(JSReferenceExpression node, JSFunction constructor, JSClass jsClass) {
        if (jsClass.isInterface()) {
            return;
        }
        JSFunction nontrivialSuperClassConstructor = getNontrivialSuperClassConstructor(jsClass);

        if (nontrivialSuperClassConstructor != null && !hasSuperConstructorCall(constructor)) {
            myHolder.newAnnotation(
                    HighlightSeverity.ERROR,
                    JavaScriptLocalize.javascriptValidationMessageMissedSuperConstructorCall()
                )
                .range(node)
                .withFix(
                    constructor == null
                        ? new AddConstructorAndSuperInvokationFix(node, nontrivialSuperClassConstructor)
                        : new AddSuperInvokationFix(node, nontrivialSuperClassConstructor)
                )
                .create();
        }
    }

    @RequiredReadAction
    private boolean hasSuperConstructorCall(JSFunction jsFunction) {
        if (jsFunction == null) {
            return false;
        }
        JSSourceElement[] body = jsFunction.getBody();
        JSStatement[] statements = body.length > 0 ? ((JSBlockStatement)body[0]).getStatements() : JSStatement.EMPTY;

        for (JSStatement st : statements) {
            if (st instanceof JSExpressionStatement expression
                && expression.getExpression() instanceof JSCallExpression call
                && call.getMethodExpression() instanceof JSSuperExpression) {
                return true;
            }
        }

        return false;
    }

    @RequiredReadAction
    public static JSFunction getNontrivialSuperClassConstructor(JSClass clazz) {
        JSClass[] classes = clazz.getSuperClasses();

        if (classes.length > 0) {
            JSFunction constructor = classes[0].findFunctionByName(classes[0].getName());

            if (constructor != null) {
                JSParameter[] jsParameters = constructor.getParameterList().getParameters();
                boolean hasRequiredParameters = false;
                for (JSParameter p : jsParameters) {
                    if (!p.isRest() && !p.hasInitializer()) {
                        hasRequiredParameters = true;
                        break;
                    }
                }
                return hasRequiredParameters ? constructor : null;
            }
        }

        return null;
    }

    @Override
    @RequiredReadAction
    public void visitJSParameterList(JSParameterList node) {
        boolean foundRest = false;
        boolean initializerPresent = false;

        for (JSParameter parameter : node.getParameters()) {
            JSExpression initializer = parameter.getInitializer();
            boolean hasInitializer = initializer != null;

            if (hasInitializer && !initializerPresent) {
                initializerPresent = true;
            }
            else if (!hasInitializer && initializerPresent && !parameter.isRest()) {
                myHolder.newAnnotation(
                        HighlightSeverity.ERROR,
                        JavaScriptLocalize.javascriptValidationMessageParameterShouldBeInitialized()
                    )
                    .range(parameter)
                    .withFix(new RemoveASTNodeFix(parameter.getNode(), JavaScriptLocalize.javascriptFixRemoveParameter()))
                    .create();
            }
            else if (hasInitializer && parameter.isRest()) {
                myHolder.newAnnotation(
                        HighlightSeverity.ERROR,
                        JavaScriptLocalize.javascriptValidationMessageRestParameterShouldNotBeInitialized()
                    )
                    .range(parameter)
                    .withFix(new RemoveASTNodeFix(
                        JavaScriptLocalize.javascriptFixRemoveInitializer(),
                        getNodesBefore(initializer, JSTokenTypes.EQ)
                    ))
                    .create();
            }

            if (parameter.isRest() && !foundRest) {
                foundRest = true;
                PsiElement typeElement = parameter.getTypeElement();
                if (typeElement != null && !"Array".equals(typeElement.getText())) {
                    myHolder.newAnnotation(
                            HighlightSeverity.ERROR,
                            JavaScriptLocalize.javascriptValidationMessageUnexpectedTypeForRestParameter()
                        )
                        .range(typeElement)
                        .withFix(new RemoveASTNodeFix(
                            JavaScriptLocalize.javascriptFixRemoveTypeReference(),
                            getNodesBefore(typeElement, JSTokenTypes.COLON)
                        ))
                        .create();
                }
            }
            else if (foundRest) {
                myHolder.newAnnotation(
                        HighlightSeverity.ERROR,
                        JavaScriptLocalize.javascriptValidationMessageParameterIsNotAllowedAfterRestParameter()
                    )
                    .range(parameter)
                    .withFix(new RemoveASTNodeFix(parameter.getNode(), JavaScriptLocalize.javascriptFixRemoveParameter()))
                    .create();
            }
        }
    }

    @RequiredReadAction
    private static ASTNode[] getNodesBefore(PsiElement initializer, IElementType eq) {
        List<ASTNode> nodes = new ArrayList<>();
        PsiElement element = initializer.getPrevSibling();
        PsiElement lastElement = element;

        if (element instanceof PsiWhiteSpace whiteSpace) {
            nodes.add(whiteSpace.getNode());
            lastElement = whiteSpace.getPrevSibling();
        }

        if (lastElement != null && lastElement.getNode().getElementType() == eq) {
            nodes.add(lastElement.getNode());
        }

        nodes.add(initializer.getNode());
        return nodes.toArray(new ASTNode[nodes.size()]);
    }

    @Override
    @RequiredReadAction
    public void visitJSPackageStatement(JSPackageStatement packageStatement) {
        for (PsiElement el = packageStatement.getPrevSibling(); el != null; el = el.getPrevSibling()) {
            if (!(el instanceof PsiWhiteSpace || el instanceof PsiComment)) {
                myHolder.newAnnotation(
                        HighlightSeverity.ERROR,
                        JavaScriptLocalize.javascriptValidationMessagePackageShouldbeFirstStatement()
                    )
                    .range(packageStatement.getFirstChild().getNode())
                    .create();
                break;
            }
        }
        PsiElement node = packageStatement.getNameIdentifier();
        if (node == null) {
            checkPackageStatement(packageStatement);
        }
    }

    @Override
    @RequiredReadAction
    public void visitJSAssignmentExpression(JSAssignmentExpression expression) {
        JSExpression lExpr = expression.getLOperand();
        if (lExpr == null) {
            return;
        }
        if (lExpr instanceof JSDefinitionExpression definitionExpression) {
            lExpr = definitionExpression.getExpression();
        }

        if (lExpr instanceof JSReferenceExpression lRefExpr) {
            PsiElement resolved = lRefExpr.resolve();
            if (resolved instanceof JSVariable variable && variable.isConst()) {
                myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptValidationMessageAssignmentToConst())
                    .range(lExpr)
                    .create();
            }
        }

        if (!JSUtils.isLHSExpression(lExpr)) {
            myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptValidationMessageMustBeLvalue())
                .range(lExpr)
                .create();
        }
    }

    @Override
    @RequiredReadAction
    public void visitJSArrayLiteralExpression(JSArrayLiteralExpression node) {
        PsiElement lastChild = node.getLastChild();
        PsiElement child = lastChild != null ? lastChild.getPrevSibling() : null;
        if (child instanceof PsiWhiteSpace whiteSpace) {
            child = whiteSpace.getPrevSibling();
        }
        ASTNode childNode;

        if (child != null && (childNode = child.getNode()) != null && childNode.getElementType() == JSTokenTypes.COMMA) {
            myHolder.newAnnotation(HighlightSeverity.WARNING, JavaScriptLocalize.javascriptValidationMessageUnneededComma())
                .range(child)
                .withFix(new RemoveASTNodeFix(childNode, JavaScriptLocalize.javascriptValidationMessageRemoveUnneededCommaFix()))
                .create();
        }
    }

    @Override
    @RequiredReadAction
    public void visitJSTryStatement(JSTryStatement node) {
        JSCatchBlock[] blocks = node.getAllCatchBlocks();

        if (blocks.length > 1) {
            Set<String> typeToCatch = new HashSet<>();

            for (JSCatchBlock block : blocks) {
                JSParameter p = block.getParameter();
                if (p == null) {
                    continue;
                }

                String s = p.getTypeString();
                if (s == null) {
                    s = "";
                }

                if (typeToCatch.contains(s)) {
                    myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptValidationMessageDuplicatedCatchBlock())
                        .range(block)
                        .withFix(new RemoveASTNodeFix(
                            block.getNode(),
                            JavaScriptLocalize.javascriptValidationMessageDuplicatedCatchBlockFix()
                        ))
                        .create();
                }
                else {
                    typeToCatch.add(s);
                }
            }
        }
    }

    @Override
    @RequiredReadAction
    public void visitJSVariable(JSVariable var) {
        if (var.isConst() && var.getInitializer() == null) {
            if (var.getParent() instanceof JSVarStatement varStatement && varStatement.getParent() instanceof JSForInStatement) {
                return;
            }

            JSAttributeList attributeList = var.getAttributeList();
            if (attributeList == null || attributeList.getAttributesByName("Embed").length == 0) {
                myHolder.newAnnotation(
                        HighlightSeverity.WARNING,
                        JavaScriptLocalize.javascriptValidationMessageConstVariableWithoutInitializer()
                    )
                    .range(var)
                    .create();
            }
        }

        if (var.getParent().getParent() instanceof JSPackageStatement) {
            checkNamedObjectIsInCorrespondingFile(var);
        }
    }

    @Override
    @RequiredReadAction
    public void visitJSContinueStatement(JSContinueStatement node) {
        if (node.getStatementToContinue() == null) {
            myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptValidationMessageContinueWithoutTarget())
                .range(node)
                .create();
        }
    }

    @Override
    @RequiredReadAction
    public void visitJSBreakStatement(JSBreakStatement node) {
        if (node.getStatementToBreak() == null) {
            myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptValidationMessageBreakWithoutTarget())
                .range(node)
                .create();
        }
    }

    @Override
    @RequiredReadAction
    public void visitJSThisExpression(@Nonnull JSThisExpression node) {
        checkClassReferenceInStaticContext(node, JavaScriptLocalize.javascriptValidationMessageThisReferencedFromStaticContext());
    }

    @RequiredReadAction
    @SuppressWarnings("unchecked")
    private void checkClassReferenceInStaticContext(JSExpression node, LocalizeValue message) {
        PsiElement element = PsiTreeUtil.getParentOfType(
            node,
            JSFunction.class,
            JSFile.class,
            JSClass.class,
            JSObjectLiteralExpression.class,
            XmlTagChild.class
        );

        if (element instanceof JSFunction function) {
            JSAttributeList attributeList = function.getAttributeList();
            if (attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)) {
                myHolder.newAnnotation(HighlightSeverity.ERROR, message)
                    .range(node)
                    .create();
                return;
            }
        }

        if (!(node instanceof JSSuperExpression)) {
            return;
        }

        PsiElement elementParent = element != null ? element.getParent() : null;
        if (element == null
            || !(elementParent instanceof JSClass || elementParent instanceof JSFile jsFile && jsFile.getContext() != null)) {
            myHolder.newAnnotation(
                    HighlightSeverity.ERROR,
                    JavaScriptLocalize.javascriptValidationMessageSuperReferencedWithoutClassInstanceContext()
                )
                .range(node)
                .create();
        }
    }

    @Override
    @RequiredReadAction
    public void visitJSSuperExpression(@Nonnull JSSuperExpression node) {
        checkClassReferenceInStaticContext(node, JavaScriptLocalize.javascriptValidationMessageSuperReferencedFromStaticContext());
    }

    @Override
    @RequiredReadAction
    public void visitJSFunctionDeclaration(final JSFunction node) {
        final PsiElement nameIdentifier = node.getNameIdentifier();
        if (nameIdentifier == null) {
            return;
        }
        PsiElement parent = node.getParent();

        if (parent instanceof JSFile file) {
            parent = JSResolveUtil.getClassReferenceForXmlFromContext(file);

            if (parent instanceof JSClass jsClass && node.getName().equals(jsClass.getName())
                && JavaScriptSupportLoader.isFlexMxmFile(parent.getContainingFile())) {
                myHolder.newAnnotation(
                        HighlightSeverity.ERROR,
                        JavaScriptLocalize.javascriptValidationMessageConstructorInMxmlIsNotAllowed()
                    )
                    .range(nameIdentifier)
                    .withFix(new RemoveASTNodeFix(node.getNode(), JavaScriptLocalize.javascriptFixRemoveConstructor()))
                    .create();
            }
        }

        if (parent instanceof JSPackageStatement) {
            checkNamedObjectIsInCorrespondingFile(node);
        }

        if (parent instanceof JSClass clazz && !node.isConstructor()) {
            final JSAttributeList attributeList = node.getAttributeList();

            if (attributeList == null || (!attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)
                && (attributeList.getAccessType() != JSAttributeList.AccessType.PRIVATE
                || attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE)))) {
                final String qName = clazz.getQualifiedName();
                final boolean hasOverride = attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE);

                final SimpleReference<JSFunction> set = new SimpleReference<>();
                boolean b = JSResolveUtil.iterateType(
                    node,
                    parent,
                    qName,
                    (processor, scope, className) -> {
                        if (qName == className || (qName != null && qName.equals(className))) {
                            return true;
                        }
                        set.set((JSFunction)processor.getResult());
                        if ("Object".equals(className)) {
                            /*native modifier is written always*/
                            if (hasOverride && !attributeList.hasModifier(JSAttributeList.ModifierType.NATIVE)) {
                                ASTNode astNode = attributeList.getNode().findChildByType(JSTokenTypes.OVERRIDE_KEYWORD);
                                //noinspection RequiredXAction
                                myHolder.newAnnotation(
                                        HighlightSeverity.ERROR,
                                        JavaScriptLocalize.javascriptValidationMessageFunctionOverrideForObjectMethod()
                                    )
                                    .range(astNode)
                                    .withFix(new RemoveASTNodeFix(astNode, JavaScriptLocalize.javascriptFixRemoveOverrideModifier()))
                                    .create();
                            }
                            return false;
                        }
                        else if (!hasOverride) {
                            //noinspection RequiredXAction
                            myHolder.newAnnotation(
                                    HighlightSeverity.ERROR,
                                    JavaScriptLocalize.javascriptValidationMessageFunctionOverrideWithoutOverrideModifier(className)
                                )
                                .range(nameIdentifier)
                                .withFix(new AddOverrideIntentionAction(node))
                                .create();
                        }
                        return false;
                    }
                );

                if (b && hasOverride) {
                    ASTNode astNode = attributeList.getNode().findChildByType(JSTokenTypes.OVERRIDE_KEYWORD);
                    myHolder.newAnnotation(
                            HighlightSeverity.ERROR,
                            JavaScriptLocalize.javascriptValidationMessageFunctionOverrideWithoutParentMethod()
                        )
                        .range(astNode)
                        .withFix(new RemoveASTNodeFix(astNode, JavaScriptLocalize.javascriptFixRemoveOverrideModifier()))
                        .create();
                }

                if (!b && hasOverride) {
                    JSFunction override = set.get();
                    JSAttributeList overrideAttrList = override.getAttributeList();
                    String overrideNs = null;

                    if ((overrideAttrList == null && (attributeList.getAccessType() != JSAttributeList.AccessType.PACKAGE_LOCAL))
                        || (overrideAttrList != null && attributeList.getAccessType() != overrideAttrList.getAccessType())
                        || overrideAttrList != null
                            && (overrideNs = overrideAttrList.getNamespace()) != null && !overrideNs.equals(attributeList.getNamespace())) {
                        String accessType = overrideNs != null ? overrideNs : (
                            overrideAttrList != null
                                ? overrideAttrList.getAccessType()
                                : JSAttributeList.AccessType.PACKAGE_LOCAL
                        ).toString().toLowerCase();
                        myHolder.newAnnotation(
                                HighlightSeverity.ERROR,
                                JavaScriptLocalize.javascriptValidationMessageFunctionOverrideIncompatibleAccessModifier(accessType)
                            )
                            .range(findElementForAccessModifierError(node, attributeList))
                            .create();

                        // TODO: quickfix
                        //annotation.registerFix(
                        //    new RemoveASTNodeFix(astNode,"javascript.fix.remove.override.modifier")
                        //);
                    }

                    SignatureMatchResult incompatibleSignature = checkCompatibleSignature(node, override);

                    if (incompatibleSignature == SignatureMatchResult.PARAMETERS_DIFFERS) {
                        JSParameterList nodeParameterList = node.getParameterList();
                        JSParameterList overrideParameterList = override.getParameterList();

                        String params = overrideParameterList != null ? overrideParameterList.getText() : "()";
                        myHolder.newAnnotation(
                                HighlightSeverity.ERROR,
                                JavaScriptLocalize.javascriptValidationMessageFunctionOverrideIncompatibleSignature(params)
                            )
                            .range(nodeParameterList != null ? nodeParameterList : node.getNameIdentifier())
                            .create();

                        // TODO: quickfix
                        //annotation.registerFix(
                        //    new RemoveASTNodeFix(astNode,"javascript.fix.remove.override.modifier")
                        //);
                    }
                    else if (incompatibleSignature == SignatureMatchResult.RETURN_TYPE_DIFFERS) {
                        PsiElement returnTypeExpr = node.getReturnTypeElement();
                        PsiElement overrideReturnTypeExpr = override.getReturnTypeElement();
                        String retType = overrideReturnTypeExpr != null ? overrideReturnTypeExpr.getText() : "*";
                        myHolder.newAnnotation(
                                HighlightSeverity.ERROR,
                                JavaScriptLocalize.javascriptValidationMessageFunctionOverrideIncompatibleSignature2(retType)
                            )
                            .range(returnTypeExpr != null ? returnTypeExpr : node.getNameIdentifier())
                            .create();

                        // TODO: quickfix
                        //annotation.registerFix(
                        //    new RemoveASTNodeFix(astNode,"javascript.fix.remove.override.modifier")
                        //);
                    }
                }
            }
        }
    }

    enum SignatureMatchResult {
        PARAMETERS_DIFFERS,
        RETURN_TYPE_DIFFERS,
        COMPATIBLE_SIGNATURE
    }

    @RequiredReadAction
    private static SignatureMatchResult checkCompatibleSignature(JSFunction fun, JSFunction override) {
        JSParameterList nodeParameterList = fun.getParameterList();
        JSParameterList overrideParameterList = override.getParameterList();
        JSParameter[] parameters = nodeParameterList != null ? nodeParameterList.getParameters() : JSParameter.EMPTY_ARRAY;
        JSParameter[] overrideParameters = overrideParameterList != null
            ? overrideParameterList.getParameters()
            : JSParameter.EMPTY_ARRAY;

        SignatureMatchResult result = parameters.length != overrideParameters.length
            ? SignatureMatchResult.PARAMETERS_DIFFERS
            : SignatureMatchResult.COMPATIBLE_SIGNATURE;

        if (result == SignatureMatchResult.COMPATIBLE_SIGNATURE) {
            for (int i = 0; i < parameters.length; ++i) {
                if (!compatibleType(
                    overrideParameters[i].getTypeString(),
                    parameters[i].getTypeString(),
                    overrideParameterList,
                    nodeParameterList
                ) || overrideParameters[i].hasInitializer() != parameters[i].hasInitializer()) {
                    result = SignatureMatchResult.PARAMETERS_DIFFERS;
                    break;
                }
            }
        }

        if (result == SignatureMatchResult.COMPATIBLE_SIGNATURE
            && !compatibleType(override.getReturnTypeString(), fun.getReturnTypeString(), override, fun)) {
            result = SignatureMatchResult.RETURN_TYPE_DIFFERS;
        }
        return result;
    }

    private static boolean compatibleType(
        String overrideParameterType,
        String parameterType,
        PsiElement overrideContext,
        PsiElement funContext
    ) {
        // TODO: This should be more accurate
        if (overrideParameterType != null && !overrideParameterType.equals(parameterType)) {
            parameterType = JSImportHandlingUtil.resolveTypeName(parameterType, funContext);
            overrideParameterType = JSImportHandlingUtil.resolveTypeName(overrideParameterType, overrideContext);

            return overrideParameterType.equals(parameterType);
        }
        else if (overrideParameterType == null && parameterType != null && !"*".equals(parameterType)) {
            return false;
        }

        return true;
    }

    @Override
    @RequiredReadAction
    @SuppressWarnings("unchecked")
    public void visitJSReturnStatement(@Nonnull JSReturnStatement node) {
        PsiElement element = PsiTreeUtil.getParentOfType(
            node,
            JSFunction.class,
            XmlTagChild.class,
            XmlAttributeValue.class,
            JSFile.class,
            JavaScriptLambdaExpression.class
        );
        if ((element instanceof JSFile jsFile && !(jsFile.getContext() instanceof PsiLanguageInjectionHost))
            || (element instanceof XmlTagChild xmlTagChild && !(xmlTagChild.getParent() instanceof XmlAttributeValue))) {
            myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptValidationMessageReturnOutsideFunctionDefinition())
                .range(node)
                .create();
        }

        if (element instanceof JSFunction function) {
            String typeString = function.getReturnTypeString();
            if (typeString != null && !"void".equals(typeString) && node.getExpression() == null) {
                myHolder.newAnnotation(
                        HighlightSeverity.ERROR,
                        JavaScriptLocalize.javascriptValidationMessageReturnValueOfTypeIsRequired(typeString)
                    )
                    .range(node)
                    .create();
            }
        }
    }

    @Override
    @RequiredReadAction
    public void visitJSLabeledStatement(JSLabeledStatement node) {
        String label = node.getLabel();
        if (label != null) {
            PsiElement run = node.getParent();
            while (run != null) {
                if (run instanceof JSLabeledStatement labeledStatement && label.equals(labeledStatement.getLabel())) {
                    myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptValidationMessageDuplicateLabel())
                        .range(node.getNameIdentifier())
                        .create();
                    break;
                }

                if (run instanceof JSFunction) {
                    break;
                }
                run = run.getParent();
            }
        }
    }

    @RequiredReadAction
    private void checkNamedObjectIsInCorrespondingFile(JSNamedElement aClass) {
        PsiFile containingFile = aClass.getContainingFile();

        if (containingFile.getContext() != null) {
            return;
        }
        VirtualFile file = containingFile.getVirtualFile();

        if (file != null && !file.getNameWithoutExtension().equals(aClass.getName())) {
            PsiElement node = aClass.getNameIdentifier();

            if (node != null) {
                String name = aClass.getName();
                String nameWithExtension = name + "." + file.getExtension();
                LocalizeValue message = aClass instanceof JSClass
                    ? JavaScriptLocalize.javascriptValidationMessageClassShouldBeInFile(name, nameWithExtension)
                    : aClass instanceof JSNamespaceDeclaration
                    ? JavaScriptLocalize.javascriptValidationMessageNamespaceShouldBeInFile(name, nameWithExtension)
                    : aClass instanceof JSVariable
                    ? JavaScriptLocalize.javascriptValidationMessageVariableShouldBeInFile(name, nameWithExtension)
                    : JavaScriptLocalize.javascriptValidationMessageFunctionShouldBeInFile(name, nameWithExtension);
                myHolder.newAnnotation(HighlightSeverity.ERROR, message)
                    .range(node)
                    .withFix(new RenameFileFix(nameWithExtension))
                    .create();
                /*annotation.registerFix(new RenamePublicClassFix(aClass) {
                      String text;
                      String familyName;

                      {
                          String term = getTerm(message);
                          text = super.getText().replace("class", StringUtil.decapitalize(term));
                          familyName = super.getFamilyName().replace("Class", term);
                      }

                      @NotNull
                      @Override
                      public String getText() {
                          return text;
                      }

                      @NotNull
                      @Override
                      public String getFamilyName() {
                          return familyName;
                      }
                    });*/
            }
        }

        checkFileUnderSourceRoot(aClass, myHolder);
    }

    private String getTerm(String message) {
        String term = message.substring(0, message.indexOf(' '));
        return term;
    }

    @RequiredReadAction
    public static void checkFileUnderSourceRoot(JSNamedElement aClass, AnnotationHolder holder) {
        PsiElement nameIdentifier = aClass.getNameIdentifier();
        if (nameIdentifier == null) {
            nameIdentifier = aClass.getFirstChild();
        }

        PsiFile containingFile = aClass.getContainingFile();
        VirtualFile file = containingFile.getVirtualFile();
        if (file == null) {
            return;
        }
        VirtualFile rootForFile = ProjectRootManager.getInstance(containingFile.getProject()).getFileIndex().getSourceRootForFile(file);

        if (rootForFile == null) {
            holder.newAnnotation(HighlightSeverity.WARNING, JavaScriptLocalize.javascriptValidationMessageFileShouldBeUnderSourceRoot())
                .range(nameIdentifier.getNode())
                .create();
        }
    }

    @RequiredReadAction
    private void checkPackageStatement(JSPackageStatement packageStatement) {
        String s = packageStatement.getQualifiedName();

        PsiFile containingFile = packageStatement.getContainingFile();
        String expected = JSResolveUtil.getExpectedPackageNameFromFile(
            containingFile.getVirtualFile(),
            containingFile.getProject(),
            true
        );

        if (expected != null && ((s == null && expected.length() != 0) || (s != null && !expected.equals(s)))) {
            PsiElement nameIdentifier = packageStatement.getNameIdentifier();
            myHolder.newAnnotation(HighlightSeverity.ERROR, JavaScriptLocalize.javascriptValidationMessageIncorrectPackageName(s, expected))
                .range(nameIdentifier != null ? nameIdentifier : packageStatement.getFirstChild())
                .withFix(new SyntheticIntentionAction() {
                    @Nonnull
                    @Override
                    public LocalizeValue getText() {
                        return JavaScriptLocalize.javascriptFixPackageName(expected);
                    }

                    @Override
                    public boolean isAvailable(@Nonnull Project project, Editor editor, PsiFile file) {
                        return packageStatement.isValid();
                    }

                    @Override
                    @RequiredWriteAction
                    public void invoke(@Nonnull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
                        JSPackageStatementImpl.doChangeName(project, packageStatement, expected);
                    }

                    @Override
                    public boolean startInWriteAction() {
                        return true;
                    }
                })
                .create();
        }

        Set<JSNamedElement> elements = new HashSet<>();

        for (JSSourceElement statement : packageStatement.getStatements()) {
            if (statement instanceof JSNamedElement namedElement && !(statement instanceof JSImportStatement)) {
                elements.add(namedElement);
            }
        }

        if (elements.size() > 1) {
            for (JSNamedElement el : elements) {
                PsiElement nameIdentifier = el.getNameIdentifier();
                myHolder.newAnnotation(
                        HighlightSeverity.ERROR,
                        JavaScriptLocalize.javascriptValidationMessageMoreThanOneExternallyVisibleSymbol()
                    )
                    .range(nameIdentifier != null ? nameIdentifier : el.getFirstChild())
                    .withFix(new RemoveASTNodeFix(el.getNode(), JavaScriptLocalize.javascriptFixRemoveExternallyVisibleSymbol()))
                    .create();
            }
        }

        checkFileUnderSourceRoot(packageStatement, myHolder);
    }

    public static class RemoveASTNodeFix implements SyntheticIntentionAction, LocalQuickFix {
        private ASTNode[] myAstNodes;
        private LocalizeValue myProp;

        public RemoveASTNodeFix(ASTNode astNode, LocalizeValue prop) {
            this(prop, astNode);
        }

        public RemoveASTNodeFix(LocalizeValue prop, ASTNode... astNodes) {
            myProp = prop;
            myAstNodes = astNodes;
        }

        @Override
        @Nonnull
        public LocalizeValue getText() {
            return myProp;
        }

        @Override
        @Nonnull
        public LocalizeValue getName() {
            return getText();
        }

        @Override
        @RequiredUIAccess
        public void applyFix(@Nonnull Project project, @Nonnull ProblemDescriptor descriptor) {
            invoke(project, null, descriptor.getPsiElement().getContainingFile());
        }

        @Override
        public boolean isAvailable(@Nonnull Project project, Editor editor, PsiFile file) {
            for (ASTNode astNode : myAstNodes) {
                if (!astNode.getPsi().isValid()) {
                    return false;
                }
            }

            return true;
        }

        @Override
        @RequiredUIAccess
        public void invoke(@Nonnull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
            if (!FileModificationService.getInstance().prepareFileForWrite(file)) {
                return;
            }
            for (ASTNode astNode : myAstNodes) {
                if (astNode.getPsi().isValid()) {
                    astNode.getPsi().delete();
                }
            }
        }

        @Override
        public boolean startInWriteAction() {
            return true;
        }
    }

    private static class AddOverrideIntentionAction implements SyntheticIntentionAction {
        private final JSFunction myNode;

        public AddOverrideIntentionAction(JSFunction node) {
            myNode = node;
        }

        @Override
        @Nonnull
        public LocalizeValue getText() {
            return JavaScriptLocalize.javascriptFixAddOverrideModifier();
        }

        @Override
        public boolean isAvailable(@Nonnull Project project, Editor editor, PsiFile file) {
            return myNode.isValid();
        }

        @Override
        @RequiredUIAccess
        public void invoke(@Nonnull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
            if (!FileModificationService.getInstance().prepareFileForWrite(file)) {
                return;
            }
            ASTNode fromText = JSChangeUtil.createJSTreeFromText(project, "override class A {}");
            JSAttributeList jsAttributeList = myNode.getAttributeList();
            JSAttributeList createdAttrList = ((JSClass)fromText.getPsi()).getAttributeList();

            if (jsAttributeList != null) {
                jsAttributeList.add(createdAttrList.getFirstChild());
            }
            else {
                myNode.addBefore(createdAttrList, myNode.getFirstChild());
            }
        }

        @Override
        public boolean startInWriteAction() {
            return true;
        }
    }

    private static class AddSuperInvokationFix implements SyntheticIntentionAction {
        private final JSReferenceExpression node;
        private final JSFunction superConstructor;

        public AddSuperInvokationFix(JSReferenceExpression node, JSFunction superConstructor) {
            this.node = node;
            this.superConstructor = superConstructor;
        }

        @Override
        @Nonnull
        public LocalizeValue getText() {
            return JavaScriptLocalize.javascriptFixCreateInvokeSuper();
        }

        @Override
        public boolean isAvailable(@Nonnull Project project, Editor editor, PsiFile file) {
            return superConstructor.isValid() && node.isValid();
        }

        @Override
        @RequiredUIAccess
        public void invoke(@Nonnull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
            if (!FileModificationService.getInstance().prepareFileForWrite(file)) {
                return;
            }
            Template t = TemplateManager.getInstance(project).createTemplate("", "");
            t.setToReformat(true);

            t.addTextSegment("super(");
            boolean first = true;
            for (JSParameter p : superConstructor.getParameterList().getParameters()) {
                if (p.isRest()) {
                    break;
                }
                if (!first) {
                    t.addTextSegment(", ");
                }
                first = false;
                MacroCallNode node = new MacroCallNode(new CompleteMacro());
                t.addVariable(p.getName(), node, node, true);
            }
            t.addTextSegment(")");
            String s = JSChangeUtil.getSemicolon(project);
            if (s.length() > 0) {
                t.addTextSegment(s);
            }

            JSFunction fun = PsiTreeUtil.getParentOfType(node, JSFunction.class);
            JSSourceElement[] body = fun != null ? fun.getBody() : JSSourceElement.EMPTY_ARRAY;

            if (body.length > 0 && body[0] instanceof JSBlockStatement blockStatement) {
                PsiElement firstChild = blockStatement.getFirstChild();
                editor.getCaretModel().moveToOffset(firstChild.getTextRange().getEndOffset());
                TemplateManager.getInstance(project).startTemplate(editor, t);
            }
        }

        @Override
        public boolean startInWriteAction() {
            return false;
        }
    }

    private static class AddConstructorAndSuperInvokationFix implements SyntheticIntentionAction {
        private final JSReferenceExpression node;
        private final JSFunction superConstructor;

        AddConstructorAndSuperInvokationFix(JSReferenceExpression _node, JSFunction _superCall) {
            node = _node;
            superConstructor = _superCall;
        }

        @Override
        @Nonnull
        public LocalizeValue getText() {
            return JavaScriptLocalize.javascriptFixCreateConstructorInvokeSuper();
        }

        @Override
        public boolean isAvailable(@Nonnull Project project, Editor editor, PsiFile file) {
            return node.isValid() && superConstructor.isValid();
        }

        @Override
        @RequiredUIAccess
        public void invoke(@Nonnull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
            if (!FileModificationService.getInstance().prepareFileForWrite(file)) {
                return;
            }
            JSClass jsClass = PsiTreeUtil.getParentOfType(node, JSClass.class);
            if (jsClass == null) {
                return;
            }
            JSAttributeList attributeList = jsClass.getAttributeList();
            StringBuilder fun = new StringBuilder();

            if (attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PUBLIC) {
                fun.append("public ");
            }

            fun.append("function ");

            JSParameterList parameterList = superConstructor.getParameterList();
            fun.append(jsClass.getName()).append(parameterList.getText()).append("{\n");
            fun.append("super(");
            int i = 0;

            for (JSParameter p : parameterList.getParameters()) {
                if (i != 0) {
                    fun.append(",");
                }
                ++i;
                fun.append(p.getName());
            }
            fun.append(")").append(JSChangeUtil.getSemicolon(project));
            fun.append("\n}");

            jsClass.add(JSChangeUtil.createJSTreeFromText(project, fun.toString()).getPsi());
        }

        @Override
        public boolean startInWriteAction() {
            return true;
        }
    }
}
