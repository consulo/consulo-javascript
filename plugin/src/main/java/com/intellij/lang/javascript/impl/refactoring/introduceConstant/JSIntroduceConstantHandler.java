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

package com.intellij.lang.javascript.impl.refactoring.introduceConstant;

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.impl.refactoring.JSBaseIntroduceHandler;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.codeEditor.Editor;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.refactoring.util.CommonRefactoringUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.util.lang.ref.SimpleReference;
import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 * @since 2008-05-29
 */
public class JSIntroduceConstantHandler extends JSBaseIntroduceHandler<JSElement, JSIntroduceConstantSettings, JSIntroduceConstantDialog> {
    @Override
    protected String getRefactoringName() {
        return JavaScriptLocalize.javascriptIntroduceConstantTitle().get();
    }

    @Override
    protected LocalizeValue getCannotIntroduceMessage() {
        return JavaScriptLocalize.javascriptIntroduceConstantErrorNoExpressionSelected();
    }

    @Override
    protected JSIntroduceConstantDialog createDialog(Project project, JSExpression expression, JSExpression[] occurrences) {
        return new JSIntroduceConstantDialog(project, occurrences, expression);
    }

    @Override
    protected String getDeclText(JSIntroduceConstantSettings settings) {
        String baseDeclText = "static const " + settings.getVariableName();
        JSAttributeList.AccessType type = settings.getAccessType();
        if (type != JSAttributeList.AccessType.PACKAGE_LOCAL) {
            baseDeclText = type.toString().toLowerCase() + " " + baseDeclText;
        }

        return baseDeclText;
    }

    @Override
    protected JSElement findAnchor(BaseIntroduceContext<JSIntroduceConstantSettings> context, boolean replaceAllOccurences) {
        return findClassAnchor(context.expression);
    }

    @Override
    @RequiredWriteAction
    protected JSElement addStatementBefore(JSElement anchorStatement, JSVarStatement declaration) throws IncorrectOperationException {
        return addToClassAnchor(anchorStatement, declaration);
    }

    @Override
    @RequiredUIAccess
    protected JSExpression findIntroducedExpression(PsiFile file, int start, int end, Editor editor) {
        if (file.getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
            CommonRefactoringUtil.showErrorHint(
                file.getProject(),
                editor,
                JavaScriptLocalize.javascriptIntroduceConstantErrorNotAvailableInJavascriptCode().get(),
                getRefactoringName(),
                null
            );
            return null;
        }

        JSExpression expression = super.findIntroducedExpression(file, start, end, editor);
        if (expression == null) {
            return null;
        }

        final SimpleReference<Boolean> hasAccesibilityProblem = new SimpleReference<>();
        expression.accept(new JSElementVisitor() {
            @Override
            @RequiredReadAction
            public void visitJSReferenceExpression(@Nonnull JSReferenceExpression node) {
                if (node.getQualifier() == null) {
                    PsiElement element = node.resolve();

                    if (element instanceof JSAttributeListOwner attributeListOwner && !(element instanceof JSClass)) {
                        JSAttributeList attributeList = attributeListOwner.getAttributeList();
                        if (attributeList == null || !attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)) {
                            hasAccesibilityProblem.set(Boolean.TRUE);
                        }
                    }
                    else if (element == null) {
                        hasAccesibilityProblem.set(Boolean.TRUE);
                    }
                }
                super.visitJSReferenceExpression(node);
            }

            @Override
            public void visitJSElement(@Nonnull JSElement node) {
                node.acceptChildren(this);
            }
        });

        if (Boolean.TRUE.equals(hasAccesibilityProblem.get())) {
            CommonRefactoringUtil.showErrorHint(
                file.getProject(),
                editor,
                JavaScriptLocalize.javascriptIntroduceConstantErrorNotConstantExpressionSelected().get(),
                getRefactoringName(),
                null
            );
            return null;
        }
        return expression;
    }
}
