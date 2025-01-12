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

package com.intellij.lang.javascript.impl.inspections;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.impl.flex.AddImportECMAScriptClassOrFunctionAction;
import com.intellij.lang.javascript.impl.validation.JSAnnotatingVisitor;
import com.intellij.lang.javascript.inspections.qucikFixes.BaseCreateFix;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.util.JSUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.codeEditor.Editor;
import consulo.javascript.ide.codeInsight.JavaScriptQuickFixFactory;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.language.JavaScriptVersionUtil;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.LocalQuickFix;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.editor.inspection.ProblemsHolder;
import consulo.language.editor.template.Template;
import consulo.language.editor.template.TemplateManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import consulo.xml.ide.highlighter.XmlFileType;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;

/**
 * @author Maxim.Mossienko
 */
@ExtensionImpl
public class JSUnresolvedFunctionInspection extends JSInspection {
    private static final String SHORT_NAME = "JSUnresolvedFunction";

    @Nonnull
    @Override
    public String getGroupDisplayName() {
        return "General";
    }

    @Nonnull
    @Override
    public String getDisplayName() {
        return JavaScriptLocalize.jsUnresolvedFunctionInspectionName().get();
    }

    @Nonnull
    @Override
    public String getShortName() {
        return SHORT_NAME;
    }

    @Override
    protected JSElementVisitor createVisitor(final ProblemsHolder holder) {
        return new JSElementVisitor() {
            @Override
            @RequiredReadAction
            public void visitJSCallExpression(@Nonnull JSCallExpression node) {
                if (node.getMethodExpression() instanceof JSReferenceExpression methodRefExpr) {
                    ResolveResult[] resolveResults = methodRefExpr.multiResolve(false);

                    boolean noCompleteResolve = true;

                    boolean inNewExpression = node instanceof JSNewExpression;
                    for (ResolveResult r : resolveResults) {
                        if (r.isValidResult()) {
                            noCompleteResolve = false;

                            PsiElement element = r.getElement();

                            if (element instanceof JSVariable variable) {
                                String typeText = variable.getTypeString();

                                if (typeText != null && !"*".equals(typeText) && !allowMemberReference(inNewExpression, typeText)) {
                                    holder.newProblem(JavaScriptLocalize.javascriptTermDoesNotEvaluateToFunction())
                                        .range(methodRefExpr.getReferenceNameElement())
                                        .highlightType(getHighlightTypeForTypeOrSignatureProblem(node))
                                        .create();
                                }
                            }
                            else if (element instanceof JSFunction function && function.isGetProperty()) {
                                String typeText = function.getReturnTypeString();

                                if (!allowMemberReference(inNewExpression, typeText)) {
                                    JSArgumentList argumentList = node.getArgumentList();
                                    LocalQuickFix fixes[] = LocalQuickFix.EMPTY_ARRAY;

                                    if (argumentList != null) {
                                        fixes = new LocalQuickFix[]{
                                            new JSAnnotatingVisitor.RemoveASTNodeFix(
                                                argumentList.getNode(),
                                                JavaScriptLocalize.javascriptTermDoesNotEvaluateToFunction2Fix()
                                            )
                                        };
                                    }

                                    holder.newProblem(JavaScriptLocalize.javascriptTermDoesNotEvaluateToFunction2())
                                        .range(methodRefExpr.getReferenceNameElement())
                                        .highlightType(getHighlightTypeForTypeOrSignatureProblem(node))
                                        .withFixes(fixes)
                                        .create();
                                }
                            }
                            break;
                        }
                    }
                    JavaScriptQuickFixFactory javaScriptQuickFixFactory = JavaScriptQuickFixFactory.byElement(methodRefExpr);

                    if (resolveResults.length == 0 || noCompleteResolve) {
                        JSExpression qualifier = methodRefExpr.getQualifier();
                        List<LocalQuickFix> quickFixes = new ArrayList<>();
                        String refName = methodRefExpr.getReferencedName();

                        if (myOnTheFly && ((qualifier == null || qualifier instanceof JSThisExpression)
                            || JSUtils.isLHSExpression(qualifier))) {
                            if (node.getMethodExpression().getParent() instanceof JSCallExpression) {
                                boolean simpleJs = !JavaScriptVersionUtil.containsFeature(methodRefExpr, JavaScriptFeature.CLASS);

                                if (!inNewExpression || simpleJs) {
                                    quickFixes.add(javaScriptQuickFixFactory.createFunctionOrMethodFix(refName, true));
                                }

                                if (qualifier == null) {
                                    if (simpleJs) {
                                        quickFixes.add(javaScriptQuickFixFactory.createFunctionOrMethodFix(refName, false));
                                    }
                                    else {
                                        quickFixes.add(new AddImportECMAScriptClassOrFunctionAction(null, methodRefExpr));
                                        if (inNewExpression) {
                                            quickFixes.add(new CreateClassOrInterfaceAction(methodRefExpr, false));
                                        }
                                    }
                                }
                            }
                        }

                        PsiElement referenceNameElement = methodRefExpr.getReferenceNameElement();

                        if (referenceNameElement != null) {
                            holder.newProblem(
                                    inNewExpression
                                        ? JavaScriptLocalize.javascriptUnresolvedTypeNameMessage(refName)
                                        : JavaScriptLocalize.javascriptUnresolvedFunctionNameMessage(refName)
                                )
                                .range(referenceNameElement)
                                .highlightType(getUnresolveReferenceHighlightType(qualifier, node))
                                .withFixes(quickFixes.size() > 0 ? quickFixes.toArray(new LocalQuickFix[quickFixes.size()]) : null)
                                .create();
                        }
                    }
                    else {
                        PsiElement element = resolveResults[0].getElement();

                        if (inNewExpression && element instanceof JSClass jsClass && jsClass.isInterface()) {
                            holder.newProblem(JavaScriptLocalize.javascriptInterfaceCanNotBeInstantiatedMessage())
                                .range(methodRefExpr.getReferenceNameElement())
                                .highlightType(getUnresolveReferenceHighlightType(methodRefExpr.getQualifier(), node))
                                .create();
                        }
                        else {
                            checkFunction(node, element, holder);
                        }
                    }
                }
                else if (node.getMethodExpression() instanceof JSSuperExpression superExpression) {
                    PsiElement element = superExpression.getReference().resolve();

                    if (element != null) {
                        checkFunction(node, element, holder);
                    }
                }
                else if (node.getMethodExpression() instanceof JSNewExpression newExpression) {
                    if (newExpression.getMethodExpression() instanceof JSReferenceExpression methodRefExpr) {
                        ResolveResult[] results = methodRefExpr.multiResolve(false);
                        PsiElement element = results.length > 0 ? results[0].getElement() : null;
                        if (element instanceof JSFunction function && function.isConstructor()
                            || element instanceof JSClass) {

                            holder.newProblem(JavaScriptLocalize.javascriptTermDoesNotEvaluateToFunction())
                                .range(node.getMethodExpression())
                                .highlightType(getUnresolveReferenceHighlightType(null, node))
                                .create();
                        }
                    }
                }

                super.visitJSCallExpression(node);
            }

            @Override
            @RequiredReadAction
            public void visitJSAssignmentExpression(@Nonnull JSAssignmentExpression node) {
                JSExpression lOperand = node.getLOperand();
                if (lOperand == null) {
                    return;
                }

                JSExpression rOperand = node.getROperand();
                if (rOperand == null) {
                    return;
                }

                PsiFile containingFile = node.getContainingFile();
                if (containingFile.getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
                    return;
                }
                String expressionType = null;

                if (lOperand instanceof JSDefinitionExpression definition
                    && definition.getExpression() instanceof JSReferenceExpression refExpr
                    && JSResolveUtil.unwrapProxy(refExpr.resolve()) instanceof JSNamedElement namedElement) {

                    expressionType = JSResolveUtil.getTypeFromSetAccessor(namedElement);
                    if (expressionType != null) {
                        expressionType = JSImportHandlingUtil.resolveTypeName(expressionType, namedElement);
                    }
                }

                if (expressionType == null) {
                    expressionType = JSResolveUtil.getQualifiedExpressionType(lOperand, containingFile);
                }
                checkExpressionIsAssignableToType(
                    rOperand,
                    expressionType,
                    holder,
                    containingFile,
                    JavaScriptLocalize::javascriptAssignedExpressionTypeMismatch
                );
            }

            @Override
            @RequiredReadAction
            public void visitJSReturnStatement(@Nonnull JSReturnStatement node) {
                JSExpression expression = node.getExpression();
                if (expression == null) {
                    return;
                }

                JSFunction fun = PsiTreeUtil.getParentOfType(node, JSFunction.class);
                if (fun == null) {
                    return; // TODO: complain about it
                }
                String typeString = fun.getReturnTypeString();
                if (typeString == null) {
                    return;
                }
                PsiFile containingFile = fun.getContainingFile();
                if (containingFile.getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
                    return;
                }

                checkExpressionIsAssignableToType(
                    expression,
                    JSImportHandlingUtil.resolveTypeName(typeString, fun),
                    holder,
                    containingFile,
                    JavaScriptLocalize::javascriptReturnedExpressionTypeMismatch
                );
            }

            @Override
            @RequiredReadAction
            public void visitJSVariable(@Nonnull JSVariable node) {
                JSExpression initializer = node.getInitializer();
                if (initializer == null) {
                    return;
                }
                checkExpressionIsAssignableToVariable(
                    node,
                    initializer,
                    holder,
                    node.getContainingFile(),
                    JavaScriptLocalize::javascriptInitializerTypeMismatch
                );
            }

            @Override
            @RequiredReadAction
            public void visitJSBinaryExpression(@Nonnull JSBinaryExpression node) {
                IElementType sign = node.getOperationSign();

                if (sign == JSTokenTypes.AS_KEYWORD || sign == JSTokenTypes.IS_KEYWORD) {
                    if (node.getROperand() instanceof JSReferenceExpression refExpr) {
                        ResolveResult[] results = refExpr.multiResolve(false);

                        if (results.length > 0 && results[0].getElement() instanceof JSVariable) {
                            checkTypeIs(refExpr, refExpr, holder, "Class");
                        }
                    }
                }
            }

            @Override
            @RequiredReadAction
            public void visitJSForInStatement(@Nonnull JSForInStatement node) {
                if (!node.isForEach()) {
                    JSVarStatement statement = node.getDeclarationStatement();

                    if (statement != null) {
                        String expressionType =
                            JSResolveUtil.getQualifiedExpressionType(node.getCollectionExpression(), node.getContainingFile());
                        boolean isDictionary = "flash.utils.Dictionary".equals(expressionType);

                        for (JSVariable var : statement.getVariables()) {
                            PsiElement typeElement = var.getTypeElement();
                            String typeElementText;

                            if (typeElement != null
                                && "Array".equals(expressionType)
                                && ("Object".equals(typeElementText = typeElement.getText()) || "*".equals(typeElementText))) {
                                holder.newProblem(JavaScriptLocalize.javascriptIncorrectArrayTypeInForin())
                                    .range(typeElement)
                                    .highlightType(ProblemHighlightType.GENERIC_ERROR_OR_WARNING)
                                    .create();
                                continue;
                            }

                            if (isDictionary && typeElement != null && "Object".equals(typeElement.getText())) {
                                continue;
                            }
                            checkTypeIs(typeElement, typeElement, holder, "XMLList".equals(expressionType) ? "XML" : "String");
                        }
                    }
                }
            }
        };
    }

    private static boolean allowMemberReference(boolean inNewExpression, String typeText) {
        return ("Class".equals(typeText) && inNewExpression) || "Function".equals(typeText);
    }

    @RequiredReadAction
    private static void checkTypeIs(PsiElement type, PsiElement node, ProblemsHolder holder, String typeName) {
        if (type instanceof JSReferenceExpression refExpr) {
            String expressionType = JSResolveUtil.getQualifiedExpressionType(refExpr, refExpr.getContainingFile());
            if (!typeName.equals(expressionType)) {
                holder.newProblem(JavaScriptLocalize.javascriptIncorrectVariableTypeMismatch(typeName, expressionType))
                    .range(node)
                    .highlightType(getHighlightTypeForTypeOrSignatureProblem(node))
                    .create();
            }
        }
        else if (type != null) {
            holder.newProblem(JavaScriptLocalize.javascriptIncorrectVariableTypeMismatch(typeName, type.getText()))
                .range(node)
                .highlightType(getHighlightTypeForTypeOrSignatureProblem(node))
                .create();
        }
    }

    @RequiredReadAction
    private static void checkTypeIs(JSExpression rOperand, PsiElement node, ProblemsHolder holder, String typeName) {
        String expressionType = JSResolveUtil.getQualifiedExpressionType(rOperand, rOperand.getContainingFile());
        if (!typeName.equals(expressionType)) {
            holder.registerProblem(
                node,
                JavaScriptLocalize.javascriptBinaryOperandTypeMismatch(typeName, expressionType).get(),
                getHighlightTypeForTypeOrSignatureProblem(node)
            );
        }
    }

    @RequiredReadAction
    static ProblemHighlightType getUnresolveReferenceHighlightType(@Nullable JSExpression qualifier, @Nonnull JSExpression node) {
        JSClass jsClass;

        PsiFile containingFile = node.getContainingFile();
        if (qualifier != null) {
            jsClass = JSResolveUtil.findClassOfQualifier(qualifier, containingFile);

            if (jsClass == null && qualifier instanceof JSReferenceExpression referenceExpression) {
                ResolveResult[] results = referenceExpression.multiResolve(false);

                if (results.length != 0) {
                    PsiElement resultElement = results[0].getElement();
                    String type = null;

                    if (resultElement instanceof JSVariable variable) {
                        type = variable.getTypeString();
                    }
                    else if (resultElement instanceof JSFunction function) {
                        type = function.getReturnTypeString();
                    }

                    if ("*".equals(type)) {
                        return ProblemHighlightType.LIKE_UNKNOWN_SYMBOL;
                    }
                    jsClass = JSResolveUtil.getClassOfContext(resultElement);
                }
            }
        }
        else {
            jsClass = JSResolveUtil.getClassOfContext(node);
        }

        boolean ecmaL4File = containingFile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;

        if (jsClass != null && ecmaL4File
            && (!(jsClass instanceof XmlBackedJSClassImpl) || jsClass.getContainingFile().getFileType() == XmlFileType.INSTANCE)) {
            JSAttributeList attributeList = jsClass.getAttributeList();
            if (attributeList == null || !attributeList.hasModifier(JSAttributeList.ModifierType.DYNAMIC)) {
                return ProblemHighlightType.ERROR;
            }
        }

        if (ecmaL4File && jsClass == null && node.getParent() instanceof JSReferenceList) {
            return ProblemHighlightType.ERROR;
        }
        return ProblemHighlightType.LIKE_UNKNOWN_SYMBOL;
    }

    @RequiredReadAction
    private static void checkFunction(JSCallExpression node, PsiElement element, ProblemsHolder holder) {
        if (element instanceof JSFunction function) {
            if (!function.isGetProperty() || !"Function".equals(function.getReturnTypeString())) {
                JSParameterList parameterList = function.getParameterList();

                if (parameterList != null) {
                    checkCallParameters(node, parameterList.getParameters(), function.isReferencesArguments(), holder);
                }
            }
        }
        else if (element instanceof JSClass) {
            if (node instanceof JSNewExpression || node.getMethodExpression() instanceof JSSuperExpression) {
                checkCallParameters(node, JSParameter.EMPTY_ARRAY, false, holder);
            }
            else {
                JSArgumentList argumentList = node.getArgumentList();
                if (argumentList == null || argumentList.getArguments().length != 1) {
                    holder.newProblem(JavaScriptLocalize.javascriptInvalidNumberOfParameters("one"))
                        .range(argumentList != null ? argumentList : node)
                        .highlightType(getHighlightTypeForTypeOrSignatureProblem(node))
                        .create();
                }
            }
        }
    }

    @RequiredReadAction
    private static void checkCallParameters(
        JSCallExpression node,
        JSParameter[] parameters,
        boolean functionReferencesArguments,
        ProblemsHolder holder
    ) {
        JSArgumentList argumentList = node.getArgumentList();
        JSExpression[] expressions = argumentList != null ? argumentList.getArguments() : JSExpression.EMPTY_ARRAY;

        boolean lastIsRest = false;
        int minParameterLength = 0;
        int maxParameterLength = parameters.length;

        for (int i = 0; i < parameters.length; ++i) {
            JSParameter parameter = parameters[i];
            if (parameter.isOptional()) {
                break;
            }

            if (i == parameters.length - 1 && parameter.isRest()) {
                lastIsRest = true;
                maxParameterLength = Integer.MAX_VALUE;
                break;
            }
            minParameterLength++;
        }

        if (!lastIsRest && parameters.length > 0 && parameters[parameters.length - 1].isRest()) {
            lastIsRest = true;
            maxParameterLength = Integer.MAX_VALUE;
        }

        if ((expressions.length < minParameterLength || expressions.length > maxParameterLength) && !functionReferencesArguments) {
            String s = (lastIsRest ? minParameterLength + " or more " : String.valueOf(minParameterLength) +
                (minParameterLength != maxParameterLength ? ".." + maxParameterLength : ""));
            holder.registerProblem(
                argumentList != null ? argumentList : node,
                JavaScriptLocalize.javascriptInvalidNumberOfParameters(s).get(),
                getHighlightTypeForTypeOrSignatureProblem(node)
            );
        }
        else {
            int i = 0;
            PsiFile containingFile = node.getContainingFile();

            for (JSParameter p : parameters) {
                if (i == expressions.length) {
                    break;
                }
                if (p.isRest()) {
                    break;
                }
                checkExpressionIsAssignableToVariable(
                    p,
                    expressions[i],
                    holder,
                    containingFile,
                    JavaScriptLocalize::javascriptArgumentTypeMismatch
                );
                ++i;
            }
        }
    }

    @RequiredReadAction
    private static void checkExpressionIsAssignableToVariable(
        JSVariable p,
        JSExpression expr,
        ProblemsHolder holder,
        PsiFile containingFile,
        BiFunction<Object, Object, LocalizeValue> messageGenerator
    ) {
        String parameterTypeResolved = JSImportHandlingUtil.resolveTypeName(p.getTypeString(), p);
        checkExpressionIsAssignableToType(
            expr,
            parameterTypeResolved,
            holder,
            containingFile,
            messageGenerator
        );
    }

    @RequiredReadAction
    private static void checkExpressionIsAssignableToType(
        JSExpression expr,
        String type,
        ProblemsHolder holder,
        PsiFile containingFile,
        BiFunction<Object, Object, LocalizeValue> messageGenerator
    ) {
        if ("*".equals(type) || type == null) {
            return; // optimization
        }
        String expressionType = JSResolveUtil.getQualifiedExpressionType(expr, containingFile);

        if (!JSResolveUtil.isAssignableType(type, expressionType, containingFile)) {
            holder.newProblem(messageGenerator.apply(type, expressionType))
                .range(expr)
                .highlightType(getHighlightTypeForTypeOrSignatureProblem(expr))
                .withFix(new JSInsertCastFix(type))
                .create();
        }
    }

    @RequiredReadAction
    private static ProblemHighlightType getHighlightTypeForTypeOrSignatureProblem(@Nonnull PsiElement node) {
        if (node.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
            return ProblemHighlightType.GENERIC_ERROR;
        }
        return ProblemHighlightType.GENERIC_ERROR_OR_WARNING;
    }

    private static class JSInsertCastFix implements LocalQuickFix {
        private final String type;

        public JSInsertCastFix(String type) {
            this.type = type;
        }

        @Override
        @Nonnull
        public String getName() {
            return JavaScriptLocalize.javascriptInsertCastFix().get();
        }

        @Override
        @Nonnull
        public String getFamilyName() {
            return getName();
        }

        @Override
        @RequiredReadAction
        public void applyFix(@Nonnull Project project, @Nonnull ProblemDescriptor descriptor) {
            PsiElement element = descriptor.getPsiElement();
            Editor editor = BaseCreateFix.getEditor(project, element.getContainingFile());
            if (editor == null) {
                return;
            }

            String shortenedType = JSResolveUtil.getShortenedType(this.type, element);
            TemplateManager templateManager = TemplateManager.getInstance(project);
            Template template = templateManager.createTemplate("", "", shortenedType + "($SELECTION$)");
            template.setToReformat(true);

            int offset = element.getTextOffset();
            editor.getSelectionModel().setSelection(offset, offset + element.getTextLength());
            editor.getCaretModel().moveToOffset(offset);
            templateManager.startTemplate(editor, element.getText(), template);
        }
    }
}
