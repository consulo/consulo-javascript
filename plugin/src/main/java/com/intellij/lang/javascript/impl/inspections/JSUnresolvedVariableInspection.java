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

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.formatter.JSCodeStyleSettings;
import com.intellij.lang.javascript.impl.flex.AddImportECMAScriptClassOrFunctionAction;
import com.intellij.lang.javascript.inspections.qucikFixes.BaseCreateFix;
import com.intellij.lang.javascript.inspections.qucikFixes.CreateJSFunctionOrMethodFix;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.util.JSUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.inspections.qucikFixes.CreateJSFunctionFixBase;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.codeStyle.CodeStyleSettingsManager;
import consulo.language.editor.inspection.LocalQuickFix;
import consulo.language.editor.inspection.ProblemsHolder;
import consulo.language.editor.template.Template;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.util.lang.StringUtil;
import jakarta.annotation.Nonnull;

import java.util.LinkedList;
import java.util.List;
import java.util.Set;

/**
 * @author Maxim.Mossienko
 */
@ExtensionImpl
public class JSUnresolvedVariableInspection extends JSInspection {
    private static final String SHORT_NAME = "JSUnresolvedVariable";

    @Nonnull
    @Override
    public String getGroupDisplayName() {
        return "General";
    }

    @Nonnull
    @Override
    public String getDisplayName() {
        return JavaScriptLocalize.jsUnresolvedVariableInspectionName().get();
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
            public void visitJSReferenceExpression(@Nonnull JSReferenceExpression node) {
                PsiElement parentElement = node.getParent();

                if (node.shouldCheckReferences() && !(parentElement instanceof JSCallExpression)) {
                    ResolveResult[] resolveResults = node.multiResolve(false);
                    boolean emptyResolve = resolveResults.length == 0;
                    boolean noCompleteResolve = true;

                    for (ResolveResult r : resolveResults) {
                        if (r.isValidResult()) {
                            noCompleteResolve = false;
                            break;
                        }
                    }

                    if (emptyResolve || noCompleteResolve) {
                        PsiElement nameIdentifier = node.getReferenceNameElement();

                        if (nameIdentifier != null) {
                            List<LocalQuickFix> fixes = new LinkedList<>();
                            JSExpression qualifier = node.getQualifier();

                            if (myOnTheFly) {
                                PsiFile containingFile = node.getContainingFile();
                                boolean ecma = containingFile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;

                                if ((qualifier == null || JSUtils.isLHSExpression(qualifier) || qualifier instanceof JSThisExpression)
                                    && (!(parentElement instanceof JSDefinitionExpression) || ecma)) {
                                    String referencedName = node.getReferencedName();
                                    boolean isField = qualifier != null;
                                    JSClass contextClass = null;

                                    if (!isField && ecma) {
                                        contextClass = JSResolveUtil.getClassOfContext(node);
                                        if (contextClass != null) {
                                            isField = true;
                                        }
                                    }

                                    if (!JSResolveUtil.isExprInTypeContext(node)) {
                                        if (node.getParent() instanceof JSArgumentList) {
                                            fixes.add(new CreateJSFunctionOrMethodFix(
                                                referencedName,
                                                !(qualifier == null || (qualifier instanceof JSThisExpression && ecma))
                                            ) {
                                                @Override
                                                @RequiredReadAction
                                                protected void addParameters(
                                                    Template template,
                                                    JSReferenceExpression referenceExpression,
                                                    PsiFile file,
                                                    Set<JavaScriptFeature> features
                                                ) {
                                                    JSCallExpression call = (JSCallExpression)referenceExpression.getParent().getParent();
                                                    JSExpression method = call.getMethodExpression();
                                                    if (method instanceof JSReferenceExpression methodRefExpr
                                                        && "bindSetter".equals(methodRefExpr.getReferencedName())) {
                                                        MyExpression expression = new MyExpression("value");
                                                        template.addVariable("value", expression, expression, false);
                                                        if (ecma) {
                                                            template.addTextSegment(":int");
                                                        }
                                                    }
                                                }

                                                @Override
                                                @RequiredReadAction
                                                protected void addReturnType(
                                                    Template template,
                                                    JSReferenceExpression referenceExpression,
                                                    PsiFile file
                                                ) {
                                                    template.addTextSegment("void");
                                                }
                                            });
                                        }

                                        boolean suggestCreateVar = true;

                                        JSClass targetClass = contextClass;

                                        if (qualifier instanceof JSReferenceExpression qualifierRefExpr) {
                                            JSClass clazz = JSResolveUtil.findClassOfQualifier(qualifierRefExpr, containingFile);
                                            if (clazz != null) {
                                                targetClass = clazz;
                                            }
                                        }

                                        if (targetClass != null) {
                                            suggestCreateVar = !targetClass.isInterface();
                                        }

                                        if (suggestCreateVar) {
                                            fixes.add(new CreateJSVariableIntentionAction(referencedName, isField, false));

                                            if (ecma) {
                                                fixes.add(new CreateJSVariableIntentionAction(referencedName, isField, true));
                                            }
                                        }

                                        if (ecma) {
                                            boolean getter = !(node.getParent() instanceof JSDefinitionExpression);
                                            String invokedName = nameIdentifier.getText();
                                            fixes.add(new CreateJSPropertyAccessorIntentionAction(invokedName, getter));
                                            JSCallExpression expression = PsiTreeUtil.getParentOfType(node, JSCallExpression.class);

                                            if (expression != null) {
                                                JSExpression methodExpression = expression.getMethodExpression();

                                                if (methodExpression instanceof JSReferenceExpression methodRefExpr) {
                                                    String methodName = methodRefExpr.getReferencedName();

                                                    if ("addEventListener".equals(methodName) || "removeEventListener".equals(methodName)) {
                                                        JSArgumentList argumentList = expression.getArgumentList();
                                                        JSExpression[] params =
                                                            argumentList != null ? argumentList.getArguments() : JSExpression.EMPTY_ARRAY;

                                                        if (params.length >= 2 && params[0] instanceof JSReferenceExpression paramRefExpr) {
                                                            JSExpression eventNameQualifier = paramRefExpr.getQualifier();
                                                            if (eventNameQualifier != null) {
                                                                fixes.add(new CreateJSEventMethod(invokedName, eventNameQualifier));
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    if (qualifier != null && !ecma) {
                                        fixes.add(new CreateJSNamespaceIntentionAction(referencedName));
                                    }

                                    if (ecma) {
                                        if (qualifier == null) {
                                            fixes.add(new AddImportECMAScriptClassOrFunctionAction(null, node));
                                            fixes.add(new CreateClassOrInterfaceAction(node, false));
                                            fixes.add(new CreateClassOrInterfaceAction(node, true));
                                        }
                                        else {
                                            fixes.add(new AddImportECMAScriptClassOrFunctionAction(null, node));
                                        }
                                    }
                                }
                            }

                            holder.newProblem(
                                    node.getQualifier() == null
                                        ? JSResolveUtil.isExprInTypeContext(node)
                                        ? JavaScriptLocalize.javascriptUnresolvedTypeNameMessage(node.getReferencedName())
                                        : JavaScriptLocalize.javascriptUnresolvedVariableOrTypeNameMessage(node.getReferencedName())
                                        : JavaScriptLocalize.javascriptUnresolvedVariableNameMessage(node.getReferencedName())
                                )
                                .range(nameIdentifier)
                                .highlightType(JSUnresolvedFunctionInspection.getUnresolveReferenceHighlightType(qualifier, node))
                                .withFixes(fixes.size() > 0 ? fixes.toArray(new LocalQuickFix[fixes.size()]) : null)
                                .create();
                        }
                    }
                }
                super.visitJSReferenceExpression(node);
            }
        };
    }

    private abstract static class BaseCreateJSVariableIntentionAction extends BaseCreateFix {
        protected final String myReferencedName;

        BaseCreateJSVariableIntentionAction(String referencedName) {
            myReferencedName = referencedName;
        }

        @Override
        @Nonnull
        public String getFamilyName() {
            return JavaScriptLocalize.javascriptCreateVariableIntentionFamily().get();
        }
    }

    private static class CreateJSNamespaceIntentionAction extends BaseCreateJSVariableIntentionAction {
        CreateJSNamespaceIntentionAction(String referencedName) {
            super(referencedName);
        }

        @Override
        @Nonnull
        public String getName() {
            return JavaScriptLocalize.javascriptCreateNamespaceIntentionName(myReferencedName).get();
        }

        @RequiredReadAction
        @Override
        protected void buildTemplate(
            Template template,
            JSReferenceExpression referenceExpression,
            Set<JavaScriptFeature> features,
            boolean staticContext,
            PsiFile file,
            PsiElement anchorParent
        ) {
            template.addTextSegment("/** @namespace ");
            template.addTextSegment(referenceExpression.getText() + " */");
            template.addEndVariable();
        }
    }

    private static class CreateJSVariableIntentionAction extends BaseCreateJSVariableIntentionAction {
        private static final String VAR_STATEMENT_START = "var ";
        private static final String CONSTANT_STATEMENT_START = "const ";
        private boolean isField;
        private boolean isConstant;

        CreateJSVariableIntentionAction(String referencedName, boolean isField, boolean isConstant) {
            super(referencedName);
            this.isField = isField;
            this.isConstant = isConstant;
        }

        @Override
        @Nonnull
        public String getName() {
            return isField
                ? isConstant
                ? JavaScriptLocalize.javascriptCreateConstantFieldIntentionName(myReferencedName).get()
                : JavaScriptLocalize.javascriptCreatePropertyIntentionName(myReferencedName).get()
                : isConstant
                ? JavaScriptLocalize.javascriptCreateConstantIntentionName(myReferencedName).get()
                : JavaScriptLocalize.javascriptCreateVariableIntentionName(myReferencedName).get();
        }

        @RequiredReadAction
        @Override
        protected void buildTemplate(
            Template template,
            JSReferenceExpression referenceExpression,
            Set<JavaScriptFeature> features,
            boolean staticContext,
            PsiFile file,
            PsiElement anchorParent
        ) {
            boolean classFeature = features.contains(JavaScriptFeature.CLASS);

            JSExpression qualifier = addAccessModifier(template, referenceExpression, classFeature, staticContext);
            if (qualifier == null || classFeature) {
                template.addTextSegment(isConstant ? CONSTANT_STATEMENT_START : VAR_STATEMENT_START);
            }

            template.addTextSegment(classFeature ? referenceExpression.getReferencedName() : referenceExpression.getText());
            template.addEndVariable();
            if (classFeature) {
                template.addTextSegment(":");
            }
            else {
                template.addTextSegment(" = ");
            }

            if (classFeature) {
                guessTypeAndAddTemplateVariable(template, referenceExpression, file);
                if (isConstant) {
                    template.addTextSegment(" = ");
                    addCompletionVar(template);
                }
            }
            else {
                addCompletionVar(template);
            }
            addSemicolonSegment(template, file);
        }

    }

    private static class CreateJSPropertyAccessorIntentionAction extends CreateJSFunctionFixBase {
        private final boolean myIsGetter;

        public CreateJSPropertyAccessorIntentionAction(String name, boolean getter) {
            super(
                getter
                    ? JavaScriptLocalize.javascriptCreateGetPropertyIntentionName(name)
                    : JavaScriptLocalize.javascriptCreateSetPropertyIntentionName(name)
            );
            myIsGetter = getter;
        }

        @Override
        protected void writeFunctionAndName(Template template, String referencedName, Set<JavaScriptFeature> features) {
            template.addTextSegment("function ");
            template.addTextSegment(myIsGetter ? "get " : "set ");
            template.addTextSegment(referencedName);
        }

        @Override
        @RequiredReadAction
        protected void addParameters(Template template, JSReferenceExpression refExpr, PsiFile file, Set<JavaScriptFeature> features) {
            if (!myIsGetter) {
                template.addTextSegment(refExpr.getReferencedName() + ":");
                guessTypeAndAddTemplateVariable(template, refExpr, file);
            }
        }

        @Override
        @RequiredReadAction
        protected void addReturnType(Template template, JSReferenceExpression referenceExpression, PsiFile file) {
            if (myIsGetter) {
                guessTypeAndAddTemplateVariable(template, referenceExpression, file);
            }
            else {
                template.addTextSegment("void");
            }
        }

        @Override
        protected void addBody(Template template, JSReferenceExpression refExpr, PsiFile file) {
            String varName = refExpr.getReferencedName();
            String paramName = varName;
            JSCodeStyleSettings settings =
                CodeStyleSettingsManager.getInstance(file.getProject()).getCurrentSettings().getCustomSettings(JSCodeStyleSettings
                    .class);
            varName = settings.FIELD_PREFIX + varName;

            if (varName.equals(paramName)) {
                varName = StringUtil.fixVariableNameDerivedFromPropertyName(varName);
            }

            if (myIsGetter) {
                template.addTextSegment("return ");

                addVarName(template, varName);
                template.addEndVariable();
            }
            else {
                addVarName(template, varName);
                template.addEndVariable();
                template.addTextSegment(" = " + paramName);
            }
            addSemicolonSegment(template, file);
        }

        private static void addVarName(Template template, String varName) {
            MyExpression expression = new MyExpression(varName);
            template.addVariable("name", expression, expression, true);
        }

    }

    private static class CreateJSEventMethod extends CreateJSFunctionFixBase {
        private JSExpression myEventQualifier;

        public CreateJSEventMethod(String invokedName, JSExpression eventNameQualifier) {
            super(JavaScriptLocalize.javascriptCreateEventHandlerIntentionName(invokedName));
            myEventQualifier = eventNameQualifier;
        }


        @Override
        @RequiredReadAction
        protected void addParameters(Template template, JSReferenceExpression refExpr, PsiFile file, Set<JavaScriptFeature> features) {
            template.addTextSegment("event:");
            template.addTextSegment(myEventQualifier.getText());
        }

        @Override
        @RequiredReadAction
        protected void addReturnType(Template template, JSReferenceExpression referenceExpression, PsiFile psifile) {
            template.addTextSegment("void");
        }

        @Override
        protected void addBody(Template template, JSReferenceExpression refExpr, PsiFile file) {
            template.addEndVariable();
        }
    }
}
