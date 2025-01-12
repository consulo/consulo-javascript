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
import jakarta.annotation.Nullable;
import org.jetbrains.annotations.NonNls;

import jakarta.annotation.Nonnull;

import java.util.LinkedList;
import java.util.List;
import java.util.function.BiFunction;

/**
 * @author Maxim.Mossienko
 */
@ExtensionImpl
public class JSUnresolvedFunctionInspection extends JSInspection {
    @NonNls
    private static final String SHORT_NAME = "JSUnresolvedFunction";

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return "General";
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return JavaScriptLocalize.jsUnresolvedFunctionInspectionName().get();
    }

    @Override
    @Nonnull
    @NonNls
    public String getShortName() {
        return SHORT_NAME;
    }

    @Override
    protected JSElementVisitor createVisitor(final ProblemsHolder holder) {
        return new JSElementVisitor() {
            @Override
            public void visitJSCallExpression(final JSCallExpression node) {
                final JSExpression methodExpression = node.getMethodExpression();

                if (methodExpression instanceof JSReferenceExpression referenceExpression) {
                    final ResolveResult[] resolveResults = referenceExpression.multiResolve(false);

                    boolean noCompleteResolve = true;

                    boolean inNewExpression = node instanceof JSNewExpression;
                    for (ResolveResult r : resolveResults) {
                        if (r.isValidResult()) {
                            noCompleteResolve = false;

                            PsiElement element = r.getElement();

                            if (element instanceof JSVariable variable) {
                                String typeText = variable.getTypeString();

                                if (typeText != null && !"*".equals(typeText)) {
                                    if (!allowMemberReference(inNewExpression, typeText)) {
                                        holder.registerProblem(
                                            referenceExpression.getReferenceNameElement(),
                                            JavaScriptLocalize.javascriptTermDoesNotEvaluateToFunction().get(),
                                            getHighlightTypeForTypeOrSignatureProblem(node)
                                        );
                                    }
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

                                    holder.registerProblem(
                                        ((JSReferenceExpression)methodExpression).getReferenceNameElement(),
                                        JavaScriptLocalize.javascriptTermDoesNotEvaluateToFunction2().get(),
                                        getHighlightTypeForTypeOrSignatureProblem(node),
                                        fixes
                                    );
                                }
                            }
                            break;
                        }
                    }
                    JavaScriptQuickFixFactory javaScriptQuickFixFactory = JavaScriptQuickFixFactory.byElement(referenceExpression);

                    if (resolveResults.length == 0 || noCompleteResolve) {
                        final JSExpression qualifier = referenceExpression.getQualifier();
                        final List<LocalQuickFix> quickFixes = new LinkedList<LocalQuickFix>();
                        final String refName = referenceExpression.getReferencedName();

                        if (myOnTheFly && ((qualifier == null || qualifier instanceof JSThisExpression) || JSUtils.isLHSExpression(qualifier))) {
                            if (methodExpression.getParent() instanceof JSCallExpression) {
                                boolean simpleJs = !JavaScriptVersionUtil.containsFeature(referenceExpression, JavaScriptFeature.CLASS);

                                if (!inNewExpression || simpleJs) {
                                    quickFixes.add(javaScriptQuickFixFactory.createFunctionOrMethodFix(refName, true));
                                }

                                if (qualifier == null) {
                                    if (simpleJs) {
                                        quickFixes.add(javaScriptQuickFixFactory.createFunctionOrMethodFix(refName, false));
                                    }
                                    else {
                                        quickFixes.add(new AddImportECMAScriptClassOrFunctionAction(null, referenceExpression));
                                        if (inNewExpression) {
                                            quickFixes.add(new CreateClassOrInterfaceAction(referenceExpression, false));
                                        }
                                    }
                                }
                            }
                        }

                        final PsiElement referenceNameElement = referenceExpression.getReferenceNameElement();

                        if (referenceNameElement != null) {
                            holder.registerProblem(
                                referenceNameElement,
                                inNewExpression
                                    ? JavaScriptLocalize.javascriptUnresolvedTypeNameMessage(refName).get()
                                    : JavaScriptLocalize.javascriptUnresolvedFunctionNameMessage(refName).get(),
                                getUnresolveReferenceHighlightType(qualifier, node),
                                quickFixes.size() > 0 ? quickFixes.toArray(new LocalQuickFix[quickFixes.size()]) : null
                            );
                        }
                    }
                    else {
                        PsiElement element = resolveResults[0].getElement();

                        if (inNewExpression && element instanceof JSClass jsClass && jsClass.isInterface()) {
                            final PsiElement referenceNameElement = referenceExpression.getReferenceNameElement();

                            holder.registerProblem(
                                referenceNameElement,
                                JavaScriptLocalize.javascriptInterfaceCanNotBeInstantiatedMessage().get(),
                                getUnresolveReferenceHighlightType(referenceExpression.getQualifier(), node)
                            );
                        }
                        else {
                            checkFunction(node, element, holder);
                        }
                    }
                }
                else if (methodExpression instanceof JSSuperExpression) {
                    final PsiElement element = (methodExpression.getReference()).resolve();

                    if (element != null) {
                        checkFunction(node, element, holder);
                    }
                }
                else if (methodExpression instanceof JSNewExpression newExpression) {
                    JSExpression methodExpr = newExpression.getMethodExpression();

                    if (methodExpr instanceof JSReferenceExpression referenceExpression) {
                        ResolveResult[] results = referenceExpression.multiResolve(false);
                        PsiElement elt;

                        if (results.length > 0 && ((elt =
                            results[0].getElement()) instanceof JSFunction && ((JSFunction)elt).isConstructor() || elt instanceof
                            JSClass)) {
                            holder.registerProblem(
                                methodExpression,
                                JavaScriptLocalize.javascriptTermDoesNotEvaluateToFunction().get(),
                                getUnresolveReferenceHighlightType(null, node)
                            );
                        }
                    }
                }

                super.visitJSCallExpression(node);
            }

            @Override
            public void visitJSAssignmentExpression(final JSAssignmentExpression node) {
                final JSExpression lOperand = node.getLOperand();
                if (lOperand == null) {
                    return;
                }
                final JSExpression rOperand = node.getROperand();
                if (rOperand == null) {
                    return;
                }

                final PsiFile containingFile = node.getContainingFile();
                if (containingFile.getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
                    return;
                }
                String expressionType = null;

                if (lOperand instanceof JSDefinitionExpression) {
                    JSExpression expression = ((JSDefinitionExpression)lOperand).getExpression();
                    if (expression instanceof JSReferenceExpression) {
                        PsiElement resolve = JSResolveUtil.unwrapProxy(((JSReferenceExpression)expression).resolve());

                        if (resolve instanceof JSNamedElement) {
                            expressionType = JSResolveUtil.getTypeFromSetAccessor((JSNamedElement)resolve);
                            if (expressionType != null) {
                                expressionType = JSImportHandlingUtil.resolveTypeName(expressionType, resolve);
                            }
                        }
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
            public void visitJSReturnStatement(final JSReturnStatement node) {
                final JSExpression expression = node.getExpression();
                if (expression == null) {
                    return;
                }

                final JSFunction fun = PsiTreeUtil.getParentOfType(node, JSFunction.class);
                if (fun == null) {
                    return; // TODO: complain about it
                }
                final String typeString = fun.getReturnTypeString();
                if (typeString == null) {
                    return;
                }
                final PsiFile containingFile = fun.getContainingFile();
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
            public void visitJSVariable(final JSVariable node) {
                final JSExpression initializer = node.getInitializer();
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
            public void visitJSBinaryExpression(JSBinaryExpression node) {
                IElementType sign = node.getOperationSign();

                if (sign == JSTokenTypes.AS_KEYWORD || sign == JSTokenTypes.IS_KEYWORD) {
                    JSExpression rOperand = node.getROperand();

                    if (rOperand instanceof JSReferenceExpression) {
                        ResolveResult[] results = ((JSReferenceExpression)rOperand).multiResolve(false);

                        if (results.length > 0 && results[0].getElement() instanceof JSVariable) {
                            checkTypeIs(rOperand, rOperand, holder, "Class");
                        }
                    }
                }
            }

            @Override
            public void visitJSForInStatement(JSForInStatement node) {
                if (!node.isForEach()) {
                    JSVarStatement statement = node.getDeclarationStatement();

                    if (statement != null) {
                        String expressionType =
                            JSResolveUtil.getQualifiedExpressionType(node.getCollectionExpression(), node.getContainingFile());
                        boolean isDictionary = "flash.utils.Dictionary".equals(expressionType);

                        for (JSVariable var : statement.getVariables()) {
                            PsiElement typeElement = var.getTypeElement();
                            String typeElementText;

                            if (typeElement != null &&
                                "Array".equals(expressionType) &&
                                ("Object".equals(typeElementText = typeElement.getText()) || "*".equals(typeElementText))) {
                                holder.registerProblem(
                                    typeElement,
                                    JavaScriptLocalize.javascriptIncorrectArrayTypeInForin().get(),
                                    ProblemHighlightType.GENERIC_ERROR_OR_WARNING
                                );
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

    private static void checkTypeIs(PsiElement type, PsiElement node, ProblemsHolder holder, String typeName) {
        if (type instanceof JSReferenceExpression referenceExpression) {
            String expressionType = JSResolveUtil.getQualifiedExpressionType(referenceExpression, referenceExpression.getContainingFile());
            if (!typeName.equals(expressionType)) {
                holder.registerProblem(
                    node,
                    JavaScriptLocalize.javascriptIncorrectVariableTypeMismatch(typeName, expressionType).get(),
                    getHighlightTypeForTypeOrSignatureProblem(node)
                );
            }
        }
        else if (type != null) {
            holder.registerProblem(
                node,
                JavaScriptLocalize.javascriptIncorrectVariableTypeMismatch(typeName, type.getText()).get(),
                getHighlightTypeForTypeOrSignatureProblem(node)
            );
        }
    }

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

    static ProblemHighlightType getUnresolveReferenceHighlightType(final @Nullable JSExpression qualifier, @Nonnull JSExpression node) {
        JSClass jsClass;

        final PsiFile containingFile = node.getContainingFile();
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

        final boolean ecmaL4File = containingFile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;

        if (jsClass != null && ecmaL4File &&
            (!(jsClass instanceof XmlBackedJSClassImpl) || jsClass.getContainingFile().getFileType() == XmlFileType.INSTANCE)) {
            final JSAttributeList attributeList = jsClass.getAttributeList();
            if (attributeList == null || !attributeList.hasModifier(JSAttributeList.ModifierType.DYNAMIC)) {
                return ProblemHighlightType.ERROR;
            }
        }

        if (ecmaL4File && jsClass == null && node.getParent() instanceof JSReferenceList) {
            return ProblemHighlightType.ERROR;
        }
        return ProblemHighlightType.LIKE_UNKNOWN_SYMBOL;
    }

    private static void checkFunction(final JSCallExpression node, final PsiElement element, final ProblemsHolder holder) {
        if (element instanceof JSFunction function) {
            if (!function.isGetProperty() || !"Function".equals(function.getReturnTypeString())) {
                final JSParameterList parameterList = function.getParameterList();

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
                    holder.registerProblem(
                        argumentList != null ? argumentList : node,
                        JavaScriptLocalize.javascriptInvalidNumberOfParameters("one").get(),
                        getHighlightTypeForTypeOrSignatureProblem(node)
                    );
                }
            }
        }
    }

    private static void checkCallParameters(
        final JSCallExpression node, final JSParameter[] parameters, boolean functionReferencesArguments,
        final ProblemsHolder holder
    ) {
        final JSArgumentList argumentList = node.getArgumentList();
        final JSExpression[] expressions = argumentList != null ? argumentList.getArguments() : JSExpression.EMPTY_ARRAY;

        boolean lastIsRest = false;
        int minParameterLength = 0;
        int maxParameterLength = parameters.length;

        for (int i = 0; i < parameters.length; ++i) {
            final JSParameter parameter = parameters[i];
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
            final String s = (lastIsRest ? minParameterLength + " or more " : String.valueOf(minParameterLength) +
                (minParameterLength != maxParameterLength ? ".." + maxParameterLength : ""));
            holder.registerProblem(
                argumentList != null ? argumentList : node,
                JavaScriptLocalize.javascriptInvalidNumberOfParameters(s).get(),
                getHighlightTypeForTypeOrSignatureProblem(node)
            );
        }
        else {
            int i = 0;
            final PsiFile containingFile = node.getContainingFile();

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

    private static void checkExpressionIsAssignableToVariable(
        final JSVariable p,
        JSExpression expr,
        final ProblemsHolder holder,
        final PsiFile containingFile,
        final BiFunction<Object, Object, LocalizeValue> messageGenerator
    ) {
        final String parameterTypeResolved = JSImportHandlingUtil.resolveTypeName(p.getTypeString(), p);
        checkExpressionIsAssignableToType(
            expr,
            parameterTypeResolved,
            holder,
            containingFile,
            messageGenerator
        );
    }

    private static void checkExpressionIsAssignableToType(
        final JSExpression expr,
        final String type,
        final ProblemsHolder holder,
        final PsiFile containingFile,
        final BiFunction<Object, Object, LocalizeValue> messageGenerator
    ) {
        if ("*".equals(type) || type == null) {
            return; // optimization
        }
        final String expressionType = JSResolveUtil.getQualifiedExpressionType(expr, containingFile);

        if (!JSResolveUtil.isAssignableType(type, expressionType, containingFile)) {
            holder.registerProblem(
                expr,
                messageGenerator.apply(type, expressionType).get(),
                getHighlightTypeForTypeOrSignatureProblem(expr),
                new JSInsertCastFix(type)
            );
        }
    }

    private static ProblemHighlightType getHighlightTypeForTypeOrSignatureProblem(@Nonnull PsiElement node) {
        if (node.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
            return ProblemHighlightType.GENERIC_ERROR;
        }
        return ProblemHighlightType.GENERIC_ERROR_OR_WARNING;
    }

    private static class JSInsertCastFix implements LocalQuickFix {
        private final String type;

        public JSInsertCastFix(final String type) {
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
        public void applyFix(@Nonnull final Project project, @Nonnull final ProblemDescriptor descriptor) {
            final PsiElement element = descriptor.getPsiElement();
            final Editor editor = BaseCreateFix.getEditor(project, element.getContainingFile());
            if (editor == null) {
                return;
            }

            final String shortenedType = JSResolveUtil.getShortenedType(this.type, element);
            final TemplateManager templateManager = TemplateManager.getInstance(project);
            Template template = templateManager.createTemplate("", "", shortenedType + "($SELECTION$)");
            template.setToReformat(true);

            final int offset = element.getTextOffset();
            editor.getSelectionModel().setSelection(offset, offset + element.getTextLength());
            editor.getCaretModel().moveToOffset(offset);
            templateManager.startTemplate(editor, element.getText(), template);
        }
    }
}
