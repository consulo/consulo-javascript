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

/*
 * @author max
 */
package com.intellij.lang.javascript.impl.navigation;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.component.ExtensionImpl;
import consulo.codeEditor.Editor;
import consulo.codeEditor.EditorPopupHelper;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.action.GotoSuperActionHander;
import consulo.language.editor.ui.PopupNavigationUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.navigation.NavigationItem;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.ex.popup.JBPopup;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class JavaScriptGotoSuperHandler implements GotoSuperActionHander {
    @Override
    @RequiredUIAccess
    public void invoke(Project project, Editor editor, PsiFile file) {
        PsiElement at = file.findElementAt(editor.getCaretModel().getOffset());
        if (at == null) {
            return;
        }
        JSNamedElement namedElement = PsiTreeUtil.getParentOfType(at, JSNamedElement.class);

        if (namedElement instanceof JSDefinitionExpression definition
            && definition.getParent() instanceof JSAssignmentExpression assignment) {
            PsiElement rOperand = assignment.getROperand();
            if (rOperand instanceof JSFunctionExpression functionExpr) {
                namedElement = functionExpr;
            }
        }

        if (namedElement instanceof JSFunction function) {
            String qName = JSResolveUtil.getQNameToStartHierarchySearch(function);

            PsiElement parent = function.getParent();

            if (qName != null) {
                if (parent instanceof JSFile jsFile) {
                    JSClass xmlBackedClass = JSResolveUtil.getXmlBackedClass(jsFile);
                    if (xmlBackedClass != null) {
                        parent = xmlBackedClass;
                    }
                }
                boolean result = JSResolveUtil.iterateType(
                    function,
                    parent instanceof JSClass jsClass ? jsClass : parent.getContainingFile(),
                    qName,
                    (processor, scope, className) -> {
                        ((NavigationItem)processor.getResult()).navigate(true);
                        return false;
                    }
                );

                if (!result) {
                    return;
                }
            }

            if (parent instanceof JSClass jsClass) {
                JSResolveUtil.processInterfaceMethods(
                    jsClass,
                    new JSResolveUtil.CollectMethodsToImplementProcessor(function.getName(), function) {
                        @Override
                        protected boolean process(ResolveProcessor processor) {
                            ((NavigationItem)processor.getResult()).navigate(true);
                            return true;
                        }
                    }
                );
            }
        }
        else if (namedElement instanceof JSClass jsClass) {
            JSClass[] classes = jsClass.getSupers();

            if (classes.length == 0) {
                return;
            }
            if (classes.length == 1) {
                classes[0].navigate(true);
            }
            else {
                JBPopup psiElementPopup = PopupNavigationUtil.getPsiElementPopup(classes, "Choose super class or interface");

                EditorPopupHelper.getInstance().showPopupInBestPositionFor(editor, psiElementPopup);
            }
        }
    }

    @Override
    public boolean startInWriteAction() {
        return false;
    }

    @Override
    public boolean isValidFor(Editor editor, PsiFile file) {
        return true;
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
