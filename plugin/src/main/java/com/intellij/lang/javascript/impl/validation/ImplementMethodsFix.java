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
 * Date: Jul 17, 2008
 * Time: 9:39:02 PM
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
    protected
    @NonNls
    String buildFunctionAttrText(@NonNls String attrText, final JSAttributeList attributeList, final JSFunction function) {
        attrText = super.buildFunctionAttrText(attrText, attributeList, function);
        if (attributeList == null || attributeList.getAccessType() != JSAttributeList.AccessType.PUBLIC) {
            attrText = "public";
        }
        return attrText;
    }

    @Override
    protected String buildFunctionBodyText(final String retType, final JSParameterList parameterList, final JSFunction func) {
        @NonNls String s = "{\n";
        if (retType != null && !"void".equals(retType)) {
            s += "return " + defaultValueOfType(retType) + JSChangeUtil.getSemicolon(func.getProject()) + "\n";
        }
        s += "}";
        return s;
    }

    private static
    @NonNls
    String defaultValueOfType(final @NonNls String retType) {
        if ("int".equals(retType) || "uint".equals(retType) || "Number".equals(retType)) {
            return "0";
        }
        if ("Boolean".equals(retType)) {
            return "false";
        }
        return "null";
    }

    @Override
    public boolean startInWriteAction() {
        return true;
    }
}