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

package com.intellij.lang.javascript.impl.refactoring.introduceField;

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.impl.refactoring.JSBaseIntroduceHandler;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.annotation.access.RequiredWriteAction;
import consulo.codeEditor.Editor;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.editor.refactoring.util.CommonRefactoringUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.xml.psi.xml.XmlFile;

/**
 * @author Maxim.Mossienko
 * @since 2008-05-29
 */
public class JSIntroduceFieldHandler extends JSBaseIntroduceHandler<JSElement, JSIntroduceFieldSettings, JSIntroduceFieldDialog> {
    @Override
    protected String getRefactoringName() {
        return JavaScriptLocalize.javascriptIntroduceFieldTitle().get();
    }

    @Override
    protected LocalizeValue getCannotIntroduceMessage() {
        return JavaScriptLocalize.javascriptIntroduceFieldErrorNoExpressionSelected();
    }

    @Override
    @RequiredUIAccess
    protected JSIntroduceFieldDialog createDialog(Project project, JSExpression expression, JSExpression[] occurrences) {
        return new JSIntroduceFieldDialog(project, occurrences, expression);
    }

    @Override
    protected JSElement findAnchor(BaseIntroduceContext<JSIntroduceFieldSettings> context, boolean replaceAllOccurences) {
        return findClassAnchor(context.expression);
    }

    @Override
    @RequiredWriteAction
    protected JSElement addStatementBefore(JSElement anchorStatement, JSVarStatement declaration) throws IncorrectOperationException {
        return addToClassAnchor(anchorStatement, declaration);
    }

    @Override
    protected String getDeclText(final JSIntroduceFieldSettings settings) {
        String baseDeclText = super.getDeclText(settings);
        JSAttributeList.AccessType type = settings.getAccessType();
        if (type != JSAttributeList.AccessType.PACKAGE_LOCAL) {
            baseDeclText = type.toString().toLowerCase() + " " + baseDeclText;
        }

        return baseDeclText;
    }

    @Override
    @RequiredUIAccess
    protected JSExpression findIntroducedExpression(PsiFile file, int start, int end, Editor editor) {
        if (file.getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
            CommonRefactoringUtil.showErrorHint(
                file.getProject(),
                editor,
                JavaScriptLocalize.javascriptIntroduceFieldErrorNotAvailableInJavascriptCode().get(),
                getRefactoringName(),
                null
            );
            return null;
        }

        return super.findIntroducedExpression(file, start, end, editor);
    }

    @Override
    @RequiredWriteAction
    protected JSVarStatement prepareDeclaration(
        String varDeclText,
        BaseIntroduceContext<JSIntroduceFieldSettings> context,
        Project project
    ) throws IncorrectOperationException {
        JSIntroduceFieldSettings.InitializationPlace place = context.settings.getInitializationPlace();

        if (place == JSIntroduceFieldSettings.InitializationPlace.FieldDeclaration) {
            return super.prepareDeclaration(varDeclText, context, project);
        }
        else {
            String assignmentText =
                context.settings.getVariableName() + "=" + context.expression.getText() + JSChangeUtil.getSemicolon(project);
            PsiElement psiToInsert = JSChangeUtil.createStatementFromText(project, assignmentText).getPsi();

            if (place == JSIntroduceFieldSettings.InitializationPlace.CurrentMethod) {
                JSElement element = super.findAnchor(context, context.settings.isReplaceAllOccurences());
                PsiElement parent = element.getParent();
                PsiElement addedElement = parent.addBefore(psiToInsert, element);
                CodeStyleManager.getInstance(project).reformatNewlyAddedElement(parent.getNode(), addedElement.getNode());
            }
            else {
                PsiElement parent = PsiTreeUtil.getParentOfType(context.expression, JSClass.class, JSFile.class);
                if (parent instanceof JSFile jsFile) {
                    PsiFile containingFile = jsFile.getContext().getContainingFile();
                    assert containingFile instanceof XmlFile;
                    parent = XmlBackedJSClassImpl.getXmlBackedClass((XmlFile)containingFile);
                }

                assert parent instanceof JSClass;
                JSClass clazz = (JSClass)parent;
                JSFunction fun = clazz.findFunctionByName(clazz.getName());

                if (fun == null) {
                    String constr = "function " + clazz.getName() + "() {}";
                    if (clazz.getAttributeList() != null && clazz.getAttributeList().getAccessType() == JSAttributeList.AccessType.PUBLIC) {
                        constr = "public " + constr;
                    }

                    fun = (JSFunction)clazz.add(JSChangeUtil.createJSTreeFromText(project, constr).getPsi());
                }

                fun.getBody()[0].add(psiToInsert);
            }

            return (JSVarStatement)JSChangeUtil.createStatementFromText(project, varDeclText + JSChangeUtil.getSemicolon(project)).getPsi();
        }
    }
}