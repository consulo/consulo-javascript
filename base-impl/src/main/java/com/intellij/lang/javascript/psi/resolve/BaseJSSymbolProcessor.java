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

package com.intellij.lang.javascript.psi.resolve;

import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.index.JSSymbolUtil;
import com.intellij.lang.javascript.index.JSTypeEvaluateManager;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.document.util.TextRange;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.language.JavaScriptVersionUtil;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.inject.InjectedLanguageManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiLanguageInjectionHost;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.util.collection.ArrayUtil;
import consulo.util.dataholder.Key;
import consulo.util.lang.Pair;
import consulo.util.lang.StringUtil;
import consulo.xml.psi.xml.XmlFile;
import consulo.xml.psi.xml.XmlTag;
import consulo.xml.psi.xml.XmlToken;
import jakarta.annotation.Nonnull;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author Maxim.Mossienko
 */
abstract public class BaseJSSymbolProcessor implements PsiScopeProcessor {
    protected final PsiFile myTargetFile;
    protected PsiFile myCurrentFile;
    protected final boolean mySkipDclsInTargetFile;
    protected final PsiElement myContext;
    protected final String myWindowIndex;
    protected final String myFunctionIndex;

    protected String[] myContextNameIds;
    @Deprecated
    protected boolean ecmal4;
    protected Set<JavaScriptFeature> myFeatures;
    protected boolean myDefinitelyGlobalReference;
    protected boolean myDefinitelyNonglobalReference;
    protected boolean myAddOnlyCompleteMatches;
    protected boolean myAllowPartialResults = true;

    private static final String ANY_TYPE = "*";
    public static final String ARRAY_TYPE_NAME = "Array";
    public static final String STRING_TYPE_NAME = "String";
    public static final String REG_EXP_TYPE_NAME = "RegExp";
    public static final String XML_TYPE_NAME = "XML";
    public static final String XML_LIST_TYPE_NAME = "XMLList";
    public static final String NUMBER_TYPE_NAME = "Number";
    public static final String FUNCTION_TYPE_NAME = "Function";
    public static final String HTML_ELEMENT_TYPE_NAME = "HTMLElement";
    private static final String INT_TYPE = "int";
    private static final String BOOLEAN_TYPE_NAME = "Boolean";
    protected String myIteratedTypeName;

    protected BaseJSSymbolProcessor(
        PsiFile targetFile,
        boolean skipDclsInTargetFile,
        PsiElement context,
        String[] contextIds
    ) {
        myTargetFile = targetFile;
        mySkipDclsInTargetFile = skipDclsInTargetFile;
        myContext = context;

        myWindowIndex = "window";
        myFunctionIndex = "Function";

        myFeatures = JavaScriptVersionUtil.getFeatures(targetFile);
        ecmal4 = myFeatures.contains(JavaScriptFeature.CLASS);

        if (contextIds == null && context instanceof JSReferenceExpression) {
            contextIds = calculateContextIds((JSReferenceExpression)context);
        }

        if (contextIds == null) { // TODO: workaround for NPE
            contextIds = new String[0];
        }

        myContextNameIds = contextIds;

        boolean notWithinWithStatement = PsiTreeUtil.getParentOfType(myContext, JSWithStatement.class) == null;

        if (((contextIds == null || contextIds.length == 0)
            && myContext instanceof JSReferenceExpression referenceExpression
            && referenceExpression.getQualifier() == null && notWithinWithStatement
            && JSResolveUtil.getRealRefExprQualifier(referenceExpression) == null)
            || (contextIds != null && contextIds.length == 1 && contextIds[0].equals(myWindowIndex) && notWithinWithStatement)) {
            myDefinitelyGlobalReference = true;
        }

        if (contextIds != null
            && (contextIds.length > 1 || (contextIds.length == 1 && !contextIds[0].isEmpty() && !contextIds[0].equals(myWindowIndex)))) {
            myDefinitelyNonglobalReference = true;
        }
    }

    protected abstract String[] calculateContextIds(JSReferenceExpression jsReferenceExpression);

    public void setAddOnlyCompleteMatches(boolean addOnlyCompleteMatches) {
        myAddOnlyCompleteMatches = addOnlyCompleteMatches;
        myAllowPartialResults = false;
    }


    protected boolean isFromRelevantFileOrDirectory() {
        PsiFile psiFile = myCurrentFile;

        return myTargetFile == psiFile || false;
        //(myTargetFile != null &&
        // psiFile != null &&
        // myTargetFile.getContainingDirectory() == psiFile.getContainingDirectory()
        //);
    }

    private static ThreadLocal<EvaluateContext> contextHolder = new ThreadLocal<>();

    @RequiredReadAction
    public static void doEvalForExpr(JSExpression rawqualifier, PsiFile myTargetFile, TypeProcessor typeProcessor) {
        EvaluateContext context = contextHolder.get();
        boolean contextHolderInitialized = false;

        if (context == null) {
            context = new EvaluateContext(myTargetFile);
            contextHolder.set(context);
            contextHolderInitialized = true;
        }
        else if (context.isAlreadyProcessingItem(rawqualifier)) {
            return;
        }
        else {
            context.addProcessingItem(rawqualifier);
        }

        try {
            doEvalForExpr(rawqualifier, typeProcessor, contextHolderInitialized ? context : new EvaluateContext(myTargetFile));
        }
        finally {
            if (!contextHolderInitialized) {
                context.removeProcessingItem(rawqualifier);
            }
            if (contextHolderInitialized) {
                contextHolder.set(null);
            }
        }
    }

    @RequiredReadAction
    public static JSExpression getOriginalQualifier(JSExpression rawQualifier) {
        if (rawQualifier instanceof JSReferenceExpression qualifier) {
            TextRange textRange = qualifier.getTextRange();
            PsiFile targetFile = rawQualifier.getContainingFile();
            PsiElement context = targetFile.getContext();
            if (context != null) {
                targetFile = context.getContainingFile();
            }

            PsiFile originalFile = targetFile.getOriginalFile();

            if (!targetFile.isPhysical() && originalFile != null) {
                PsiElement startElement;
                if (context != null) {
                    PsiElement at = PsiTreeUtil.getNonStrictParentOfType(
                        originalFile.findElementAt(context.getTextOffset()),
                        PsiLanguageInjectionHost.class
                    );
                    if (at instanceof PsiLanguageInjectionHost) {
                        List<Pair<PsiElement, TextRange>> injectedPsiFiles =
                            InjectedLanguageManager.getInstance(at.getProject()).getInjectedPsiFiles(at);
                        if (injectedPsiFiles != null) {
                            for (Pair<PsiElement, TextRange> pair : injectedPsiFiles) {
                                if (pair.getFirst() instanceof JSFile) {
                                    originalFile = (PsiFile)pair.getFirst();
                                    break;
                                }
                            }
                        }
                    }
                }
                startElement = originalFile.findElementAt(qualifier.getTextOffset());

                do {
                    qualifier = PsiTreeUtil.getParentOfType(startElement, JSReferenceExpression.class);
                    if (qualifier == null) {
                        break;
                    }
                    startElement = qualifier;
                }
                while (!textRange.equals(qualifier.getTextRange()));
            }

            rawQualifier = qualifier;
        }
        else if (rawQualifier instanceof JSIndexedPropertyAccessExpression indexedPropertyAccessExpression) {
            JSExpression qualifier = indexedPropertyAccessExpression.getQualifier();
            if (qualifier != null) {
                JSExpression jsExpression = getOriginalQualifier(qualifier);
                if (jsExpression != null && jsExpression.getParent() instanceof JSIndexedPropertyAccessExpression) {
                    return (JSExpression)jsExpression.getParent();
                }
            }
        }
        else if (rawQualifier instanceof JSCallExpression call) {
            JSExpression qualifier = call.getMethodExpression();
            if (qualifier != null) {
                JSExpression jsExpression = getOriginalQualifier(qualifier);
                if (jsExpression != null && jsExpression.getParent() instanceof JSCallExpression parentCall) {
                    return parentCall;
                }
            }
        }
        return rawQualifier;
    }

    @RequiredReadAction
    private static void doEvalForExpr(JSExpression rawQualifier, TypeProcessor typeProcessor, EvaluateContext context) {
        if (rawQualifier instanceof JSDefinitionExpression definition) {
            rawQualifier = definition.getExpression();
        }

        if (rawQualifier instanceof JSNewExpression newExpression) {
            JSExpression methodExpr = newExpression.getMethodExpression();
            if (methodExpr != null) {
                String text = methodExpr.getText();
                if (methodExpr instanceof JSReferenceExpression && typeProcessor.ecma()) {
                    SimpleTypeProcessor processor = new SimpleTypeProcessor(typeProcessor.getFeatures());
                    doEvalForExpr(methodExpr, context.targetFile, processor);

                    if (processor.type != null && !"*".equals(processor.type)) {
                        text = "Class".equals(processor.type) ? "*" : processor.type;
                    }
                }
                addType(text, typeProcessor, context, methodExpr);
            }
            return;
        }

        if (rawQualifier instanceof JSCallExpression call) {
            rawQualifier = call.getMethodExpression();
        }

        if (rawQualifier instanceof JSReferenceExpression qualifier) {
            boolean wasPrototype = false;

            if (qualifier != null && "prototype".equals(qualifier.getReferencedName())) {
                JSExpression expression = qualifier.getQualifier();
                if (expression instanceof JSReferenceExpression refExpr) {
                    qualifier = refExpr;
                    wasPrototype = true;
                }
            }

            boolean topLevelQualifier = qualifier.getQualifier() == null;

            if (topLevelQualifier) {
                JSReferenceExpression expression =
                    typeProcessor.ecma() ? qualifier : JSSymbolUtil.findReferenceExpressionUsedForClassExtending(qualifier);
                if (!typeProcessor.ecma() && expression != qualifier && expression.getQualifier() == null) {
                    addType(expression.getText(), typeProcessor, context, qualifier);
                    return;
                }
            }

            ResolveResult[] resolveResults = qualifier != null ? qualifier.multiResolve(false) : ResolveResult.EMPTY_ARRAY;

            if (resolveResults.length > 0) {
                for (ResolveResult r : resolveResults) {
                    PsiElement psiElement = r.getElement();
                    context.setSource(psiElement);
                    if (psiElement == qualifier.getParent()) {
                        continue;
                    }

                    //if (psiElement == qualifier && ((JSReferenceExpressionImpl)qualifier).isAttributeReference()) {
                    //   addType(STRING_TYPE_NAME, typeProcessor, context, null); continue;
                    //}
                    String type = psiElement instanceof JSNamedElement ? null : null;

                    if (type == null) {
                        if (psiElement instanceof JSVariable jsVariable) {
                            String parameterType = jsVariable.getTypeString();

                            if (isSomeFunctionCall(rawQualifier, parameterType)) {
                                parameterType = ANY_TYPE;

                                if (jsVariable.hasInitializer()
                                    && jsVariable.getInitializer() instanceof JSFunctionExpression functionExpr) {
                                    String typeString = functionExpr.getFunction().getReturnTypeString();
                                    if (isValidType(typeString)) {
                                        parameterType = typeString;
                                    }
                                }
                            }

                            if (isValidType(parameterType)) {
                                addType(JSImportHandlingUtil.resolveTypeName(parameterType, jsVariable), typeProcessor, context, null);
                            }

                            if (!typeProcessor.ecma() || parameterType == null) {
                                JSExpression expression = jsVariable.getInitializer();
                                if (expression != null) {
                                    doEvalForExpr(expression, context.targetFile, typeProcessor);
                                }
                                else {
                                    boolean hasSomeInfoAboutVar = false;

                                    if (topLevelQualifier) {
                                        ResolveProcessor processor = new ResolveProcessor(qualifier.getText(), true);
                                        hasSomeInfoAboutVar = findDef(typeProcessor, context, qualifier, processor);
                                    }

                                    if (!hasSomeInfoAboutVar && parameterType == null
                                        && jsVariable.getParent().getParent() instanceof JSForInStatement forInStatement
                                        && jsVariable.getParent() == forInStatement.getDeclarationStatement()) {
                                        parameterType = evalComponentTypeFromArrayExpression(
                                            rawQualifier,
                                            typeProcessor,
                                            context,
                                            forInStatement.getCollectionExpression()
                                        );
                                    }
                                    if (!hasSomeInfoAboutVar && parameterType == null) {
                                        typeProcessor.setUnknownElement(jsVariable);
                                    }
                                }
                            }
                        }
                        else {
                            boolean hasSomeType = false;

                            if (psiElement instanceof JSProperty property) {
                                psiElement = property.getValue();
                            }

                            if (psiElement instanceof JSClass jsClass) {
                                addType(jsClass.getQualifiedName(), typeProcessor, context, psiElement);
                                hasSomeType = true;
                            }
                            else if (psiElement instanceof JSObjectLiteralExpression objectLiteralExpression
                                && typeProcessor instanceof ResolveProcessor) {
                                addTypeFromObjectLiteralExpression(objectLiteralExpression, typeProcessor);
                            }
                            else if (psiElement instanceof JSDefinitionExpression) {
                                String parameterType = null;

                                if (psiElement.getParent() instanceof JSForInStatement forInStatement
                                    && psiElement == forInStatement.getVariableExpression()) {
                                    parameterType = evalComponentTypeFromArrayExpression(
                                        rawQualifier,
                                        typeProcessor,
                                        context,
                                        forInStatement.getCollectionExpression()
                                    );
                                }
                                if (parameterType == null) {
                                    addTypeFromDefExpr(typeProcessor, context, psiElement);
                                }
                            }
                            else if (psiElement instanceof XmlToken xmlToken) {
                                hasSomeType = true;

                                TagContextBuilder builder = new TagContextBuilder(xmlToken, HTML_ELEMENT_TYPE_NAME);
                                PsiElement element = builder.element;
                                String typeName = builder.typeName;

                                if (HTML_ELEMENT_TYPE_NAME.equals(typeName)) {
                                    hasSomeType = false;
                                }

                                addType(typeName, typeProcessor, context, element);
                            }
                            else if (psiElement instanceof JSNamedElement namedElement) {
                                if (namedElement instanceof JSFunction function) {
                                    boolean inCall = rawQualifier.getParent() instanceof JSCallExpression;

                                    if (!inCall && (!function.isGetProperty() && !function.isSetProperty())) {
                                        addType(FUNCTION_TYPE_NAME, typeProcessor, context, function);
                                        hasSomeType = true;
                                        if (wasPrototype) {
                                            String name = function.getName();
                                            if (name != null) {
                                                addType(name, typeProcessor, context, null);
                                            }
                                        }
                                    }
                                    else if (function.isConstructor()) {
                                        PsiElement parentClass = JSResolveUtil.findParent(function);
                                        if (parentClass instanceof JSClass clazz) {
                                            addType(clazz.getQualifiedName(), typeProcessor, context, clazz);
                                            hasSomeType = true;
                                        }
                                    }
                                    else {
                                        String s = null;
                                        if (function.isSetProperty()) {
                                            JSParameterList parameterList = function.getParameterList();
                                            JSParameter[] parameters =
                                                parameterList != null ? parameterList.getParameters() : JSParameter.EMPTY_ARRAY;
                                            s = parameters.length == 1 ? parameters[0].getTypeString() : null;
                                        }

                                        if (s == null) {
                                            s = function.getReturnTypeString();
                                            if (isSomeFunctionCall(rawQualifier, s)) {
                                                s = ANY_TYPE;
                                            }
                                        }
                                        if (isValidType(s)) {
                                            addType(JSImportHandlingUtil.resolveTypeName(s, function), typeProcessor, context, null);
                                            hasSomeType = true;
                                        }
                                        else {
                                            addQNameFromElementAsType(typeProcessor, context, function, false);
                                        }
                                    }
                                }
                                else {
                                    boolean hasTypeKnowledge = false;
                                    boolean passSource = false;

                                    if (!hasTypeKnowledge || wasPrototype) {
                                        addQNameFromElementAsType(typeProcessor, context, namedElement, passSource);
                                    }
                                }
                            }
                            else if (psiElement instanceof JSLiteralExpression literal) {
                                getTypeFromConstant(literal, typeProcessor, context);
                                hasSomeType = true;
                            }
                            else if (psiElement instanceof JSExpressionStatement expression) {
                                String exprType = JSDocumentationUtils.findType(expression);
                                if (isValidType(exprType)) {
                                    addType(JSImportHandlingUtil.resolveTypeName(exprType, expression), typeProcessor, context, null);
                                    hasSomeType = true;
                                }
                            }
                            if (!hasSomeType && psiElement != null) {
                                typeProcessor.setUnknownElement(psiElement);
                            }
                        }
                        //myTargetFiles.add(psiElement.getContainingFile());
                    }
                    else if (!context.visitedTypes.contains(type)) {
                        //if (context.typeEvaluateManager.isArrayType(type))) type = ARRAY_TYPE_NAME;
                        addType(type, typeProcessor, context, null);
                    }
                }
            }
            else {
                typeProcessor.setUnknownElement(qualifier);
            }

            JSReferenceExpression localQualifier = qualifier;

            while (true) {
                if (localQualifier.getQualifier() instanceof JSCallExpression call) {
                    JSExpression methodExpression = call.getMethodExpression();

                    if (methodExpression instanceof JSReferenceExpression referenceExpression) {
                        localQualifier = referenceExpression;
                        continue;
                    }
                }

                break;
            }

            if ("$".equals(localQualifier.getText())) {
                addType("jQuery", typeProcessor, context, localQualifier);
            }
            if ("$".equals(qualifier.getText())) {
                addType(HTML_ELEMENT_TYPE_NAME, typeProcessor, context, null);
            }
            else if ("getComponentById".equals(qualifier.getReferencedName())) {
                tryAddBindowsType(qualifier, typeProcessor, context);
            }
        }
        else if (rawQualifier instanceof JSBinaryExpression binaryExpr) {
            IElementType sign = binaryExpr.getOperationSign();
            JSExpression rOperand = binaryExpr.getROperand();
            JSExpression lOperand = binaryExpr.getLOperand();

            if (rOperand != null) {
                if (sign == JSTokenTypes.AS_KEYWORD) {
                    addType(JSImportHandlingUtil.resolveTypeName(rOperand.getText(), rOperand), typeProcessor, context, null);
                }
                else if (JSTokenTypes.RELATIONAL_OPERATIONS.contains(sign) || JSTokenTypes.EQUALITY_OPERATIONS.contains(sign) ||
                    sign == JSTokenTypes.IS_KEYWORD) {
                    addType(BOOLEAN_TYPE_NAME, typeProcessor, context, null);
                }
                else if (JSTokenTypes.ADDITIVE_OPERATIONS.contains(sign)
                    || JSTokenTypes.MULTIPLICATIVE_OPERATIONS.contains(sign)
                    || sign == JSTokenTypes.ANDAND || sign == JSTokenTypes.OROR) {

                    SimpleTypeProcessor lprocessor = new SimpleTypeProcessor(typeProcessor.getFeatures());
                    SimpleTypeProcessor rprocessor = new SimpleTypeProcessor(typeProcessor.getFeatures());
                    doEvalForExpr(lOperand, lprocessor, context);
                    doEvalForExpr(rOperand, rprocessor, context);

                    String evaluatedType = lprocessor.type != null && lprocessor.type.equals(rprocessor.type) ? lprocessor.type : null;
                    if (evaluatedType != null) {
                        addType(evaluatedType, typeProcessor, context, rawQualifier);
                    }
                    else {
                        typeProcessor.setUnknownElement(rawQualifier);
                    }
                }
                else if (sign == JSTokenTypes.EQ) {
                    SimpleTypeProcessor rprocessor = new SimpleTypeProcessor(typeProcessor.getFeatures());
                    doEvalForExpr(rOperand, rprocessor, context);

                    String evaluatedType = rprocessor.type;
                    if (evaluatedType != null) {
                        addType(evaluatedType, typeProcessor, context, rawQualifier);
                    }
                    else {
                        typeProcessor.setUnknownElement(rawQualifier);
                    }
                }
            }
        }
        else if (rawQualifier instanceof JSLiteralExpression literal) {
            getTypeFromConstant(literal, typeProcessor, context);
        }
        else if (rawQualifier instanceof JSArrayLiteralExpression arrayLiteral) {
            addType(ARRAY_TYPE_NAME, typeProcessor, context, arrayLiteral);
            if (typeProcessor instanceof ResolveProcessor resolveProcessor) {
                resolveProcessor.execute(arrayLiteral, ResolveState.initial());
            }
        }
        else if (rawQualifier instanceof JSIndexedPropertyAccessExpression propertyAccess) {
            SimpleTypeProcessor lprocessor = new SimpleTypeProcessor(typeProcessor.getFeatures());

            doEvalForExpr(propertyAccess.getQualifier(), lprocessor, context);

            if (lprocessor.result instanceof JSArrayLiteralExpression arrayLiteral && typeProcessor instanceof ResolveProcessor) {
                for (JSExpression expr : arrayLiteral.getExpressions()) {
                    if (expr instanceof JSObjectLiteralExpression) {
                        addTypeFromObjectLiteralExpression(expr, typeProcessor);
                    }
                }
            }

            addComponentTypeFromProcessor(propertyAccess, typeProcessor, context, lprocessor);
        }
        else if (rawQualifier instanceof JSObjectLiteralExpression objectLiteral && typeProcessor instanceof ResolveProcessor) {
            addTypeFromObjectLiteralExpression(objectLiteral, typeProcessor);
        }
        else if (rawQualifier instanceof JSParenthesizedExpression parenthesized) {
            doEvalForExpr(parenthesized.getInnerExpression(), typeProcessor, context);
        }
        else if (rawQualifier instanceof JSThisExpression thisExpr) {
            JSClass jsClass = JSResolveUtil.getClassOfContext(thisExpr);
            if (jsClass != null) {
                addType(jsClass.getQualifiedName(), typeProcessor, context, jsClass);
            }
            else {
                typeProcessor.setUnknownElement(thisExpr);
            }
        }
        else if (rawQualifier instanceof JSSuperExpression superExpr) {
            JSClass jsClass = JSResolveUtil.getClassOfContext(superExpr);
            if (jsClass != null) {
                JSClass[] classes = jsClass.getSuperClasses();
                if (classes.length > 0) {
                    addType(classes[0].getQualifiedName(), typeProcessor, context, classes[0]);
                }
            }
        }
        else if (rawQualifier != null) {
            typeProcessor.setUnknownElement(rawQualifier);
        }

        JavaScriptType type = rawQualifier == null ? null : rawQualifier.getType();
        PsiElement targetElement = type == null ? null : type.getTargetElement();
        if (targetElement instanceof JSExpression jsTarget) {
            doEvalForExpr(jsTarget, typeProcessor, context);
        }
    }

    private static boolean isSomeFunctionCall(JSExpression rawqualifier, String parameterType) {
        return FUNCTION_TYPE_NAME.equals(parameterType) && rawqualifier.getParent() instanceof JSCallExpression;
    }

    @RequiredReadAction
    private static void addQNameFromElementAsType(
        TypeProcessor typeProcessor,
        EvaluateContext context,
        PsiElement psiElement,
        boolean passSource
    ) {
        String name = ((JSNamedElement)psiElement).getName();
        if (name == null) {
            return;
        }
        if (psiElement instanceof JSQualifiedNamedElement qualifiedNamedElement) {
            String qName = qualifiedNamedElement.getQualifiedName();
            if (qName != null) {
                name = qName;
            }
        }
        addType(name, typeProcessor, context, passSource ? psiElement : null);
    }

    @RequiredReadAction
    private static String evalComponentTypeFromArrayExpression(
        JSExpression rawqualifier,
        TypeProcessor typeProcessor,
        EvaluateContext context,
        JSExpression collectionExpression
    ) {
        if (collectionExpression != null) {
            if (context.isAlreadyProcessingItem(rawqualifier)) {
                return null;
            }
            context.addProcessingItem(rawqualifier);
            SimpleTypeProcessor lprocessor = new SimpleTypeProcessor(typeProcessor.getFeatures());
            doEvalForExpr(collectionExpression, lprocessor, context);
            return addComponentTypeFromProcessor(rawqualifier, typeProcessor, context, lprocessor);
        }

        return null;
    }

    private static String addComponentTypeFromProcessor(
        JSExpression rawqualifier,
        TypeProcessor typeProcessor,
        EvaluateContext context,
        SimpleTypeProcessor lprocessor
    ) {
        if (lprocessor.type != null) {
            String type = lprocessor.type;
            type = JSTypeEvaluateManager.isArrayType(type) ? JSTypeEvaluateManager.getComponentType(type) : "*";
            addType(JSImportHandlingUtil.resolveTypeName(type, rawqualifier), typeProcessor, context, rawqualifier);
            return type;
        }
        return null;
    }

    private static boolean isValidType(String parameterType) {
        return parameterType != null && parameterType.length() > 0;
    }

    private static boolean findDef(
        TypeProcessor typeProcessor,
        EvaluateContext context,
        JSReferenceExpression qualifier,
        ResolveProcessor processor
    ) {
        PsiElement parent = qualifier.getParent();
        JSResolveUtil.treeWalkUp(processor, qualifier, parent, qualifier);

        PsiElement jsElement = processor.getResult();
        if (jsElement instanceof JSDefinitionExpression) {
            addTypeFromDefExpr(typeProcessor, context, jsElement);
            return true;
        }
        return false;
    }

    @RequiredReadAction
    private static void tryAddBindowsType(JSReferenceExpression qualifier, TypeProcessor typeProcessor, EvaluateContext context) {
        PsiElement element = qualifier.getParent();
        if (!(element instanceof JSCallExpression call)) {
            return;
        }
        JSArgumentList argumentList = call.getArgumentList();
        if (argumentList == null) {
            return;
        }
        JSExpression[] expressions = argumentList.getArguments();
        if (expressions.length == 0 || !(expressions[0] instanceof JSLiteralExpression)) {
            return;
        }

        String val = StringUtil.stripQuotesAroundValue(expressions[0].getText());
        PsiElement contextElement = qualifier.getContainingFile().getContext();

        if (contextElement != null) {
            PsiFile containingFile = contextElement.getContainingFile();

            if (isBindowsXml(containingFile)) {
            }
        }
    }

    @RequiredReadAction
    private static boolean isBindowsXml(PsiFile containingFile) {
        return containingFile.getName().endsWith(".xml") && containingFile instanceof XmlFile;
    }

    private static void addTypeFromObjectLiteralExpression(JSExpression rawQualifier, TypeProcessor typeProcessor) {
        JSProperty[] properties = ((JSObjectLiteralExpression)rawQualifier).getProperties();

        boolean b = properties.length <= 0 || rawQualifier.processDeclarations(
            (ResolveProcessor)typeProcessor,
            ResolveState.initial(),
            properties[0],
            properties[0]
        );
        if (b) {
            typeProcessor.setUnknownElement(rawQualifier);
        }
    }

    @RequiredReadAction
    private static void addTypeFromDefExpr(TypeProcessor typeProcessor, EvaluateContext context, PsiElement psiElement) {
        String type = psiElement.getText();

        if (!context.visitedTypes.contains(type)) {
            context.visitedTypes.add(type);
            PsiElement parentElement = psiElement.getParent();
            if (parentElement instanceof JSAssignmentExpression assignment) {
                JSExpression expr = assignment.getROperand();
                while (expr instanceof JSAssignmentExpression rOperandAssignment) {
                    expr = rOperandAssignment.getROperand();
                }
                if (expr != null) {
                    doEvalForExpr(expr, typeProcessor, context);
                }
            }
            else {
                typeProcessor.setUnknownElement(parentElement);
            }
        }
    }

    private static void getTypeFromConstant(JSExpression rawqualifier, TypeProcessor typeProcessor, EvaluateContext context) {
        ASTNode childNode = rawqualifier.getNode().getFirstChildNode();
        IElementType constantType = childNode.getElementType();

        String type = JavaScriptTokenSets.STRING_LITERALS.contains(constantType)
            ? STRING_TYPE_NAME
            : constantType == JSTokenTypes.NUMERIC_LITERAL
            ? NUMBER_TYPE_NAME
            : constantType == JSTokenTypes.REGEXP_LITERAL
            ? REG_EXP_TYPE_NAME
            : constantType == JSTokenTypes.XML_START_TAG_START
            ? XML_TYPE_NAME
            : constantType == JSTokenTypes.XML_START_TAG_LIST
            ? XML_LIST_TYPE_NAME
            : constantType == JSTokenTypes.TRUE_KEYWORD || constantType == JSTokenTypes.FALSE_KEYWORD
            ? BOOLEAN_TYPE_NAME
            : null;

        if (type == NUMBER_TYPE_NAME && typeProcessor.ecma()) {
            String text = childNode.getText();
            if (text.indexOf('.') == -1) {
                type = INT_TYPE;
            }
        }

        if (type != null) {
            addType(type, typeProcessor, context, rawqualifier);
        }
        else {
            typeProcessor.setUnknownElement(rawqualifier);
        }
    }

    protected void addPackageScope(List<String[]> possibleNameIds, JSClass jsClass, PsiElement expression) {
        String packageQualifier = JSResolveUtil.findPackageStatementQualifier(expression);
        String qName;

        if (packageQualifier != null) {
            buildIndexListFromQNameAndCorrectQName(packageQualifier, jsClass, possibleNameIds);
        }
        else if (jsClass != null && (qName = jsClass.getQualifiedName()) != null && !qName.equals(jsClass.getName())) {
            int index = qName.lastIndexOf('.');
            if (index > 0) {
                buildIndexListFromQNameAndCorrectQName(qName.substring(0, index), jsClass, possibleNameIds);
            }
        }
        else if (jsClass == null) {
            String s = JSResolveUtil.findPackageForMxml(expression);
            if (isValidType(s)) {
                buildIndexListFromQNameAndCorrectQName(s, expression, possibleNameIds);
            }
        }
    }

    protected String buildIndexListFromQNameAndCorrectQName(String type, PsiElement source, List<String[]> possibleNameIds) {
        List<String> list = new ArrayList<>();
        type = addIndexListFromQName(type, source, list);

        possibleNameIds.add(ArrayUtil.toStringArray(list));
        return type;
    }

    public static String addIndexListFromQName(String type, PsiElement source, List<String> list) {
        int i = type.indexOf('.');
        int lastI = 0;

        while (i != -1) {
            list.add(type.substring(lastI, i));
            lastI = i + 1;
            i = type.indexOf('.', lastI);
        }

        if (i != type.length()) {
            String s = type.substring(lastI, type.length());
            if (source == null) {
                s = StringUtil.capitalize(s); // when type name goes from comments
            }
            if (lastI == 0 && !s.equals(type)) {
                type = s;
            }
            list.add(s);
        }
        return type;
    }

    private static void addType(String type, TypeProcessor typeProcessor, EvaluateContext context, PsiElement source) {
        if (type == null) {
            return;
        }
        int spacePos = type.indexOf(' ');
        if (spacePos != -1) {
            type = type.substring(0, spacePos);
        }

        if (!(typeProcessor instanceof GenericTypeParametersClient)) {
            int gtPos = type.indexOf('<');
            if (gtPos > 0 && type.charAt(gtPos - 1) == '.') {
                type = type.substring(0, gtPos - 1);
            }
        }

        if (!typeProcessor.ecma()) {
            type = JSTypeEvaluateManager.getInstanceNameByType(type);
        }
        typeProcessor.process(type, context, source);
    }

    @Override
    public <T> T getHint(Key<T> hintClass) {
        return null;
    }

    @Override
    public void handleEvent(Event event, Object associated) {
    }

    enum MatchType {
        COMPLETE_TYPE,
        COMPLETE,
        COMPLETE_NS,
        PARTIAL,
        NOMATCH
    }

    interface HierarchyProcessor {
        boolean processClass(JSClass clazz);
    }

    protected void doIterateTypeHierarchy(String[] contextIds, HierarchyProcessor processor) {
        StringBuilder builder = new StringBuilder();
        for (String cnameId : contextIds) {
            if (builder.length() > 0) {
                builder.append('.');
            }
            builder.append(cnameId);
        }

        String typeName = builder.toString();
        if (typeName.length() == 0) {
            return;
        }

        myIteratedTypeName = typeName;
        doIterateHierarchy(typeName, processor);
        myIteratedTypeName = null;
    }

    protected void doIterateHierarchy(String typeName, final HierarchyProcessor processor) {
        PsiElement clazz = JSResolveUtil.findClassByQName(typeName, myContext);
        if (clazz instanceof JSClass jsClass) {
            for (JSClass superClazz : jsClass.getSuperClasses()) {
                superClazz.processDeclarations(
                    new ResolveProcessor(null) {
                        {
                            setToProcessMembers(false);
                            setToProcessHierarchy(true);
                            setTypeContext(true);
                        }

                        @Override
                        @RequiredReadAction
                        public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                            return !(element instanceof JSClass jsClass && !processor.processClass(jsClass));
                        }
                    },
                    ResolveState.initial(),
                    null,
                    clazz
                );
            }
        }
    }

    public static class EvaluateContext {
        public final PsiFile targetFile;
        public final Set<String> visitedTypes = new HashSet<>();
        private Set<JSExpression> processingItems;
        private PsiElement source;

        public EvaluateContext(PsiFile targetFile) {
            this.targetFile = targetFile;
        }

        public boolean isAlreadyProcessingItem(JSExpression rawqualifier) {
            return processingItems != null && processingItems.contains(rawqualifier);
        }

        public void addProcessingItem(JSExpression rawqualifier) {
            if (processingItems == null) {
                processingItems = new HashSet<>();
            }
            processingItems.add(rawqualifier);
        }

        public void removeProcessingItem(JSExpression rawqualifier) {
            processingItems.remove(rawqualifier);
        }

        public PsiElement getSource() {
            return source;
        }

        public void setSource(PsiElement source) {
            this.source = source;
        }
    }

    public interface TypeProcessor {
        void process(@Nonnull String type, @Nonnull EvaluateContext evaluateContext, PsiElement source);

        @Deprecated
        boolean ecma();

        Set<JavaScriptFeature> getFeatures();

        void setUnknownElement(@Nonnull PsiElement element);
    }

    public interface GenericTypeParametersClient {
    }

    public static class SimpleTypeProcessor extends ResolveProcessor implements TypeProcessor, GenericTypeParametersClient {
        private String type;
        private PsiElement result;
        private PsiElement source;
        private final Set<JavaScriptFeature> myFeatures;

        public SimpleTypeProcessor(Set<JavaScriptFeature> features) {
            super(null);
            myFeatures = features;
        }

        @Override
        public void process(@Nonnull String _type, @Nonnull EvaluateContext evaluateContext, PsiElement _source) {
            setType(type != null && !type.equals(_type) ? ANY_TYPE : _type, _source);
        }

        private void setType(String s, PsiElement _source) {
            type = s;
            source = _source;
        }

        @Override
        public Set<JavaScriptFeature> getFeatures() {
            return myFeatures;
        }

        @Override
        public boolean ecma() {
            return myFeatures.contains(JavaScriptFeature.CLASS);
        }

        @Override
        @RequiredReadAction
        public boolean execute(@Nonnull PsiElement element, ResolveState state) {
            result = element;
            return true;
        }

        @Override
        public void setUnknownElement(@Nonnull PsiElement _element) {
            setType(ANY_TYPE, _element);
        }

        public String getType() {
            return type;
        }

        public PsiElement getSource() {
            return source;
        }
    }

    public static class TagContextBuilder {
        public final PsiElement element;
        public final String typeName;

        public TagContextBuilder(PsiElement psiElement, String defaultName) {
            String typeName = null;
            XmlTag tag = PsiTreeUtil.getParentOfType(psiElement, XmlTag.class);
            JSClass element = null;

            if (tag != null) {
                PsiFile containingFile = tag.getContainingFile();

                if (isBindowsXml(containingFile)) {
                    typeName = "Bi" + tag.getLocalName();
                }
                else {
                    element = JSResolveUtil.getClassFromTagNameInMxml(tag);
                    typeName = element != null ? element.getQualifiedName() : null;
                }
            }

            if (typeName == null) {
                typeName = defaultName;
            }

            this.typeName = typeName;
            this.element = element;
        }
    }
}
