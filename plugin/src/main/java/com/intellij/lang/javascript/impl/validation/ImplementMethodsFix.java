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

import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.codeEditor.Editor;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.intention.SyntheticIntentionAction;
import consulo.language.psi.PsiFile;
import consulo.project.Project;
import jakarta.annotation.Nonnull;
import org.jetbrains.annotations.NonNls;

/**
 * @author Maxim.Mossienko
 * @since 2008-07-17
 */
public class ImplementMethodsFix extends BaseCreateMethodsFix<JSFunction> implements SyntheticIntentionAction {
    public ImplementMethodsFix(final JSClass jsClass) {
        super(jsClass);
    }

    @Override
    @Nonnull
    public String getText() {
        return JavaScriptLocalize.javascriptFixImplementMethods().get();
    }

    @Override
    public boolean isAvailable(@Nonnull final Project project, final Editor editor, final PsiFile file) {
        return myJsClass.isValid();
    }

    @Override
    protected String buildFunctionAttrText(String attrText, final JSAttributeList attributeList, final JSFunction function) {
        return attributeList == null || attributeList.getAccessType() != JSAttributeList.AccessType.PUBLIC
            ? "public"
            : super.buildFunctionAttrText(attrText, attributeList, function);
    }

    @Override
    protected String buildFunctionBodyText(final String retType, final JSParameterList parameterList, final JSFunction func) {
        StringBuilder s = new StringBuilder("{\n");
        if (retType != null && !"void".equals(retType)) {
            s.append("return ").append(defaultValueOfType(retType)).append(JSChangeUtil.getSemicolon(func.getProject())).append("\n");
        }
        return s.append("}").toString();
    }

    private static String defaultValueOfType(final String retType) {
        return switch (retType) {
            case "int", "uint", "Number" -> "0";
            case "Boolean" -> "false";
            default -> "null";
        };
    }

    @Override
    public boolean startInWriteAction() {
        return true;
    }
}