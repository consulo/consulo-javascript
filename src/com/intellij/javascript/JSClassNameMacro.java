package com.intellij.javascript;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.template.Expression;
import com.intellij.codeInsight.template.ExpressionContext;
import com.intellij.codeInsight.template.Macro;
import com.intellij.codeInsight.template.Result;
import com.intellij.codeInsight.template.TextResult;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;

public class JSClassNameMacro extends Macro {
  @NonNls
  public String getName() {
    return "jsClassName";
  }

	@Override
	public String getPresentableName()
	{
		return JSBundle.message("js.classname.macro.description");
	}

  @NonNls
  public String getDefaultValue() {
    return "";
  }

  public Result calculateResult(@NotNull final Expression[] params, final ExpressionContext context) {
    final PsiElement elementAtCaret = findElementAtCaret(context);
    final JSResolveUtil.ContextResolver resolver = new JSResolveUtil.ContextResolver(elementAtCaret);

    String text = resolver.getQualifierAsString();
    if (text == null) {
      final JSFunction previousFunction = PsiTreeUtil.getPrevSiblingOfType(elementAtCaret, JSFunction.class);

      if (previousFunction != null) {
        text = previousFunction.getName();
      }
    }

    if (text != null) return new TextResult(text);

    return null;
  }

  public static PsiElement findElementAtCaret(final ExpressionContext context) {
    Project project = context.getProject();
    int templateStartOffset = context.getTemplateStartOffset();
    int offset = templateStartOffset > 0 ? context.getTemplateStartOffset() - 1 : context.getTemplateStartOffset();

    PsiDocumentManager.getInstance(project).commitAllDocuments();

    PsiFile file = PsiDocumentManager.getInstance(project).getPsiFile(context.getEditor().getDocument());
    return file.findElementAt(offset);
  }

  public Result calculateQuickResult(@NotNull final Expression[] params, final ExpressionContext context) {
    return null;
  }

  public LookupElement[] calculateLookupItems(@NotNull final Expression[] params, final ExpressionContext context) {
    return null;
  }
}
