/*
 * Copyright 2000-2005 JetBrains s.r.o
 * Copyright 2013-2016 must-be.org
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

package consulo.javascript.inspections.qucikFixes;

import com.intellij.lang.javascript.inspections.qucikFixes.BaseCreateFix;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.template.Template;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.localize.LocalizeValue;

import jakarta.annotation.Nonnull;

import java.util.Set;

/**
 * @author VISTALL
 * @since 2016-02-24
 */
public abstract class CreateJSFunctionFixBase extends BaseCreateFix {
    private final LocalizeValue myMessage;

    public CreateJSFunctionFixBase(LocalizeValue message) {
        myMessage = message;
    }

    @Override
    @Nonnull
    public String getName() {
        return myMessage.get();
    }

    @Override
    @Nonnull
    public String getFamilyName() {
        return JavaScriptLocalize.javascriptCreateFunctionIntentionFamily().get();
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
        String referencedName = classFeature ? referenceExpression.getReferencedName() : referenceExpression.getText();
        BaseCreateFix.addAccessModifier(template, referenceExpression, classFeature, staticContext);
        writeFunctionAndName(template, referencedName, features);
        template.addTextSegment("(");

        addParameters(template, referenceExpression, file, features);

        template.addTextSegment(")");

        if (classFeature) {
            template.addTextSegment(":");
            addReturnType(template, referenceExpression, file);
        }

        JSClass clazz = BaseCreateFix.findClass(file, anchorParent);
        if (clazz == null || !clazz.isInterface()) {
            template.addTextSegment(" {");
            addBody(template, referenceExpression, file);
            template.addTextSegment("}");
        }
        else {
            addSemicolonSegment(template, file);
            template.addEndVariable();
        }
    }

    protected void writeFunctionAndName(Template template, String referencedName, Set<JavaScriptFeature> features) {
        template.addTextSegment("function ");
        template.addTextSegment(referencedName);
    }

    @RequiredReadAction
    protected abstract void addParameters(Template template, JSReferenceExpression refExpr, PsiFile file, Set<JavaScriptFeature> features);

    @RequiredReadAction
    protected abstract void addReturnType(Template template, JSReferenceExpression referenceExpression, PsiFile psifile);

    protected abstract void addBody(Template template, JSReferenceExpression refExpr, PsiFile file);
}
