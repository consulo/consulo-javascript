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

package com.intellij.lang.javascript.impl.structureView;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.icon.IconDescriptorUpdaters;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNamedElement;
import consulo.ui.image.Image;
import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 * @since 2008-07-23
 */
public class JSStructureItemPresentation extends JSStructureViewElement.JSStructureItemPresentationBase {
    public JSStructureItemPresentation(JSStructureViewElement jsStructureViewElement) {
        super(jsStructureViewElement);
    }

    @Override
    @RequiredReadAction
    public String getPresentableText() {
        PsiElement psiElement = element.getUpToDateElement();
        if (psiElement == null || !psiElement.isValid()) {
            return "*invalid*";
        }

        return getName(psiElement);
    }

    @RequiredReadAction
    public static String getName(@Nonnull PsiElement psiElement) {
        if (psiElement instanceof JSObjectLiteralExpression objectLiteral) {
            if (objectLiteral.getParent() instanceof JSAssignmentExpression assignment) {
                JSDefinitionExpression lOperand = (JSDefinitionExpression)assignment.getLOperand();
                return JSResolveUtil.findClassIdentifier(lOperand.getExpression()).getText();
            }
            else {
                return JavaScriptLocalize.javascriptLanguageTermPrototype().get();
            }
        }

        if (psiElement instanceof JSDefinitionExpression definition) {
            psiElement = definition.getExpression();
        }

        if (psiElement instanceof JSReferenceExpression refExpr) {
            String s = refExpr.getReferencedName();

            if (JSResolveUtil.PROTOTYPE_FIELD_NAME.equals(s)
                && refExpr.getQualifier() instanceof JSReferenceExpression qualifierRefExpr) {
                s = qualifierRefExpr.getReferencedName();
            }
            return s;
        }

        if (!(psiElement instanceof PsiNamedElement)) {
            return psiElement.getText();
        }

        String name = ((PsiNamedElement)psiElement).getName();

        if (psiElement instanceof JSProperty property) {
            psiElement = property.getValue();
        }

        if (psiElement instanceof JSFunction function) {
            if (name == null) {
                name = "<anonymous>";
            }
            name += "(";
            JSParameterList parameterList = function.getParameterList();
            if (parameterList != null) {
                for (JSParameter p : parameterList.getParameters()) {
                    if (!name.endsWith("(")) {
                        name += ", ";
                    }
                    name += p.getName();
                    final String variableType = p.getTypeString();
                    if (variableType != null) {
                        name += ":" + variableType;
                    }
                }
            }
            name += ")";

            String type = function.getReturnTypeString();
            if (type != null) {
                name += ":" + type;
            }
        }

        if (name == null && psiElement.getParent() instanceof JSAssignmentExpression assignment) {
            JSExpression lOperand = ((JSDefinitionExpression)assignment.getLOperand()).getExpression();
            lOperand = JSResolveUtil.findClassIdentifier(lOperand);
            if (lOperand instanceof JSReferenceExpression lOperandRefExpr) {
                return lOperandRefExpr.getReferencedName();
            }
            return lOperand.getText();
        }
        return name;
    }

    @Override
    @RequiredReadAction
    public Image getIcon() {
        PsiElement psiElement = this.element.getRealElement();
        if (!psiElement.isValid()) {
            return null;
        }
        return IconDescriptorUpdaters.getIcon(psiElement, 0);
    }
}
