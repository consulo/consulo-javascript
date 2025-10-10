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

import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.editor.inspection.ProblemsHolder;
import consulo.language.editor.inspection.localize.InspectionLocalize;
import consulo.language.psi.PsiElement;
import consulo.language.psi.ResolveResult;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 */
@ExtensionImpl
public class JSDeprecatedSymbolsInspection extends JSInspection {
    private static final String SHORT_NAME = "JSDeprecatedSymbols";

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return InspectionLocalize.inspectionGeneralToolsGroupName();
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return JavaScriptLocalize.jsDeprecatedSymbolsInspectionName();
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
                for (ResolveResult r : node.multiResolve(false)) {
                    PsiElement element = r.getElement();
                    if (element instanceof JSDefinitionExpression definition && definition.getParent() instanceof JSAssignmentExpression
                        || element == node.getParent()) {
                        continue;
                    }
                    if (JSDocumentationUtils.isDeprecated(element)) {
                        holder.newProblem(JavaScriptLocalize.javascriptDeprecatedSymbolUsedNameMessage())
                            .range(node.getReferenceNameElement())
                            .highlightType(ProblemHighlightType.LIKE_DEPRECATED)
                            .create();
                        break;
                    }
                }
            }
        };
    }
}