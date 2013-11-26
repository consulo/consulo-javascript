/*
 * Copyright 2000-2006 JetBrains s.r.o.
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

package com.intellij.lang.javascript.inspections;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.codeHighlighting.HighlightDisplayLevel;
import com.intellij.codeInsight.CodeInsightUtilBase;
import com.intellij.codeInsight.template.Template;
import com.intellij.codeInsight.template.TemplateManager;
import com.intellij.codeInsight.template.impl.MacroCallNode;
import com.intellij.codeInsight.template.macro.MacroFactory;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.impl.JSPsiImplUtils;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;

/**
 * @author Maxim.Mossienko
 */
public class JSUntypedDeclarationInspection extends JSInspection {
  @NonNls public static final String SHORT_NAME = "JSUntypedDeclaration";

  @NotNull
  public String getGroupDisplayName() {
    return JSBundle.message("js.inspection.group.name");
  }

  @NotNull
  public String getDisplayName() {
    return JSBundle.message("js.untyped.declaration.inspection.name");
  }

  @NotNull
  @Override
  public HighlightDisplayLevel getDefaultLevel() {
    return HighlightDisplayLevel.WARNING;
  }

  @NotNull
  @NonNls
  public String getShortName() {
    return SHORT_NAME;
  }

  protected JSElementVisitor createVisitor(final ProblemsHolder holder) {
    return new JSElementVisitor() {
      @Override public void visitJSVariable(final JSVariable node) {
        process(node, holder);
      }

      @Override public void visitJSFunctionExpression(final JSFunctionExpression node) {
        process(node.getFunction(), holder);
      }

      @Override public void visitJSFunctionDeclaration(final JSFunction node) {
        if (node.isConstructor() || node.isSetProperty()) return;
        process(node, holder);
      }
    };
  }

  private static void process(final JSNamedElement node, final ProblemsHolder holder) {
    if (node.getContainingFile().getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4) return;
    ASTNode nameIdentifier = node.findNameIdentifier();
    
    if (nameIdentifier != null &&
        JSPsiImplUtils.getTypeFromDeclaration(node) == null &&
        (!(node instanceof JSParameter) || !((JSParameter)node).isRest())
      ) {
      holder.registerProblem(
        nameIdentifier.getPsi(),
        JSBundle.message(node instanceof JSFunction ? "js.untyped.function.problem":"js.untyped.variable.problem", nameIdentifier.getText()),
        ProblemHighlightType.GENERIC_ERROR_OR_WARNING,
        new AddTypeToDclFix()
      );
    }
  }

  private static class AddTypeToDclFix implements LocalQuickFix {

    @NotNull
    public String getName() {
      return JSBundle.message("js.untyped.declaration.problem.addtype.fix");
    }

    @NotNull
    public String getFamilyName() {
      return getName();
    }

    public void applyFix(@NotNull final Project project, @NotNull final ProblemDescriptor descriptor) {
      PsiElement anchor = descriptor.getPsiElement();
      PsiFile containingFile = anchor.getContainingFile();
      if (!CodeInsightUtilBase.getInstance().prepareFileForWrite(containingFile)) return;

      if (anchor.getParent() instanceof JSFunction) {
        anchor = ((JSFunction)anchor.getParent()).getParameterList();
      }

      OpenFileDescriptor openDescriptor = new OpenFileDescriptor(project, containingFile.getVirtualFile(), anchor.getTextRange().getEndOffset());
      openDescriptor.navigate(true);
      Editor textEditor = FileEditorManager.getInstance(project).getSelectedTextEditor();
      TemplateManager templateManager = TemplateManager.getInstance(project);

      Template t = templateManager.createTemplate("","");
      t.addTextSegment(":");
      boolean hasDetectedTypeFromUsage = false;
      final PsiElement anchorParent = anchor.getParent();

      if (anchorParent instanceof JSVariable) {
        final JSExpression expression = ((JSVariable)anchorParent).getInitializer();
        
        if (expression != null) {
          BaseCreateFix.guessExprTypeAndAddSuchVariable(expression, t, "a", containingFile, true);
          hasDetectedTypeFromUsage = true;
        }
      }

      if (!hasDetectedTypeFromUsage) {
        String defaultValue = "uint";
        if (ApplicationManager.getApplication().isUnitTestMode()) {
          t.addTextSegment(defaultValue);
        } else {
          t.addVariable("a", new MacroCallNode(MacroFactory.createMacro("complete")), new BaseCreateFix.MyExpression(defaultValue), true);
        }
      }
      
      templateManager.startTemplate(textEditor, t);
    }
  }
}
