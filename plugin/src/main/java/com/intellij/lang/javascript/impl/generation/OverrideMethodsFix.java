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

package com.intellij.lang.javascript.impl.generation;

import com.intellij.lang.javascript.impl.validation.BaseCreateMethodsFix;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.PsiElement;

/**
 * @author Maxim.Mossienko
 * @since 2008-07-17
 */
public class OverrideMethodsFix extends BaseCreateMethodsFix<JSFunction> {
    public OverrideMethodsFix(final JSClass jsClass) {
        super(jsClass);
    }

    @Override
    @RequiredReadAction
    protected String buildFunctionBodyText(final String retType, final JSParameterList parameterList, final JSFunction func) {
        StringBuilder functionText = new StringBuilder();
        functionText.append("{\n");

        if (!"void".equals(retType)) {
            functionText.append("  return");
        }
        else {
            functionText.append(" ");
        }

        functionText.append(" super.").append(func.getName());

        if (func.isGetProperty()) {
        }
        else if (func.isSetProperty()) {
            functionText.append(" = ").append(parameterList.getParameters()[0].getName());
        }
        else {
            functionText.append("(");
            boolean first = true;
            for (JSParameter param : parameterList.getParameters()) {
                if (!first) {
                    functionText.append(",");
                }
                first = false;
                functionText.append(param.getName());
            }
            functionText.append(")");
        }

        functionText.append(JSChangeUtil.getSemicolon(func.getProject())).append("\n}");
        return functionText.toString();
    }

    @Override
    @RequiredReadAction
    protected String buildFunctionAttrText(String attrText, final JSAttributeList attributeList, final JSFunction function) {
        attrText = super.buildFunctionAttrText(attrText, attributeList, function);
        final PsiElement element = JSResolveUtil.findParent(function);
        if (attributeList == null || !attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE)) {
            if (element instanceof JSClass jsClass && !"Object".equals(jsClass.getQualifiedName())) {
                final PsiElement typeElement = attributeList != null ? attributeList.findAccessTypeElement() : null;
                if (typeElement == null) {
                    attrText += " override";
                }
                else {
                    final int index = attrText.indexOf(typeElement.getText());
                    attrText = attrText.substring(0, index) + ((index > 0) ? " " : "") + "override " + attrText.substring(index);
                }
            }
        }

        return attrText;
    }
}
