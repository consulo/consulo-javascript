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
import com.intellij.lang.javascript.inspections.qucikFixes.BaseCreateFix;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSPsiImplUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.application.Application;
import consulo.codeEditor.Editor;
import consulo.fileEditor.FileEditorManager;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.FileModificationService;
import consulo.language.editor.inspection.LocalQuickFix;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.editor.inspection.ProblemsHolder;
import consulo.language.editor.rawHighlight.HighlightDisplayLevel;
import consulo.language.editor.template.Template;
import consulo.language.editor.template.TemplateManager;
import consulo.language.editor.template.macro.MacroCallNode;
import consulo.language.editor.template.macro.MacroFactory;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.localize.LocalizeValue;
import consulo.navigation.OpenFileDescriptor;
import consulo.navigation.OpenFileDescriptorFactory;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

import java.util.Collections;

/**
 * @author Maxim.Mossienko
 */
@ExtensionImpl
public class JSUntypedDeclarationInspection extends JSInspection {
    public static final String SHORT_NAME = "JSUntypedDeclaration";

    @Nonnull
    @Override
    public String getGroupDisplayName() {
        return "General";
    }

    @Nonnull
    @Override
    public String getDisplayName() {
        return JavaScriptLocalize.jsUntypedDeclarationInspectionName().get();
    }

    @Nonnull
    @Override
    public HighlightDisplayLevel getDefaultLevel() {
        return HighlightDisplayLevel.WARNING;
    }

    @Override
    @Nonnull
    public String getShortName() {
        return SHORT_NAME;
    }

    @Override
    protected JSElementVisitor createVisitor(final ProblemsHolder holder) {
        return new JSElementVisitor() {
            @Override
            @RequiredReadAction
            public void visitJSVariable(@Nonnull JSVariable node) {
                process(node, holder);
            }

            @Override
            @RequiredReadAction
            public void visitJSFunctionExpression(@Nonnull JSFunctionExpression node) {
                process(node.getFunction(), holder);
            }

            @Override
            @RequiredReadAction
            public void visitJSFunctionDeclaration(@Nonnull JSFunction node) {
                if (node.isConstructor() || node.isSetProperty()) {
                    return;
                }
                process(node, holder);
            }
        };
    }

    @RequiredReadAction
    private static void process(JSNamedElement node, ProblemsHolder holder) {
        if (node.getContainingFile().getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
            return;
        }
        PsiElement nameIdentifier = node.getNameIdentifier();

        if (nameIdentifier != null
            && JSPsiImplUtils.getTypeFromDeclaration(node) == null
            && !(node instanceof JSParameter parameter && parameter.isRest())) {
            LocalizeValue description = node instanceof JSFunction
                ? JavaScriptLocalize.jsUntypedFunctionProblem(nameIdentifier.getText())
                : JavaScriptLocalize.jsUntypedVariableProblem(nameIdentifier.getText());

            holder.newProblem(description)
                .range(nameIdentifier)
                .highlightType(ProblemHighlightType.GENERIC_ERROR_OR_WARNING)
                .withFix(new AddTypeToDclFix())
                .create();
        }
    }

    private static class AddTypeToDclFix implements LocalQuickFix {
        @Nonnull
        @Override
        public String getName() {
            return JavaScriptLocalize.jsUntypedDeclarationProblemAddtypeFix().get();
        }

        @Nonnull
        @Override
        public String getFamilyName() {
            return getName();
        }

        @Override
        @RequiredReadAction
        public void applyFix(@Nonnull Project project, @Nonnull ProblemDescriptor descriptor) {
            PsiElement anchor = descriptor.getPsiElement();
            PsiFile containingFile = anchor.getContainingFile();
            if (!FileModificationService.getInstance().prepareFileForWrite(containingFile)) {
                return;
            }

            if (anchor.getParent() instanceof JSFunction function) {
                anchor = function.getParameterList();
            }

            OpenFileDescriptor openDescriptor = OpenFileDescriptorFactory.getInstance(project)
                .builder(containingFile.getVirtualFile())
                .offset(anchor.getTextRange().getEndOffset())
                .build();
            openDescriptor.navigate(true);
            Editor textEditor = FileEditorManager.getInstance(project).getSelectedTextEditor();
            TemplateManager templateManager = TemplateManager.getInstance(project);

            Template t = templateManager.createTemplate("", "");
            t.addTextSegment(":");
            boolean hasDetectedTypeFromUsage = false;
            PsiElement anchorParent = anchor.getParent();

            if (anchorParent instanceof JSVariable variable) {
                JSExpression expression = variable.getInitializer();

                if (expression != null) {
                    BaseCreateFix.guessExprTypeAndAddSuchVariable(
                        expression,
                        t,
                        "a",
                        containingFile,
                        Collections.singleton(JavaScriptFeature.CLASS)
                    );
                    hasDetectedTypeFromUsage = true;
                }
            }

            if (!hasDetectedTypeFromUsage) {
                String defaultValue = "uint";
                if (Application.get().isUnitTestMode()) {
                    t.addTextSegment(defaultValue);
                }
                else {
                    t.addVariable(
                        "a",
                        new MacroCallNode(MacroFactory.createMacro("complete")),
                        new BaseCreateFix.MyExpression(defaultValue),
                        true
                    );
                }
            }

            templateManager.startTemplate(textEditor, t);
        }
    }
}
