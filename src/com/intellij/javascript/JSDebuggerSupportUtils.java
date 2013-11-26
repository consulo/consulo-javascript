package com.intellij.javascript;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.JSElementFactory;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileTypes.StdFileTypes;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Feb 27, 2008
 * Time: 7:45:08 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSDebuggerSupportUtils {
  @Nullable
  public static TextRange getExpressionAtOffset(@NotNull Project project, @NotNull Document document, final int offset) {
    PsiFile file = PsiDocumentManager.getInstance(project).getPsiFile(document);
    if (file == null) return null;

    int injectionOffsetCorrection = 0;
    PsiReference ref = file.findReferenceAt(offset);
    
    if (ref == null) {
      final PsiElement at = file.findElementAt(offset);

      TextRange rangeForNamedElement = getRangeForNamedElement(at, 0);
      if (rangeForNamedElement != null) return rangeForNamedElement;

      final PsiLanguageInjectionHost psiLanguageInjectionHost = PsiTreeUtil.getParentOfType(at, PsiLanguageInjectionHost.class);

      if (psiLanguageInjectionHost != null) {
        final Ref<PsiReference> result = new Ref<PsiReference>();
        final Ref<PsiElement> eltInInjected = new Ref<PsiElement>();
        final int[] injectedOffset = new int[1];

        psiLanguageInjectionHost.processInjectedPsi(new PsiLanguageInjectionHost.InjectedPsiVisitor() {
          public void visit(@NotNull final PsiFile injectedPsi, @NotNull final List<PsiLanguageInjectionHost.Shred> places) {
            final PsiLanguageInjectionHost.Shred shred = places.get(0);
            final int injectedStart = shred.getRangeInsideHost().getStartOffset() + shred.host.getTextOffset();
            final int offsetInInjected = offset - injectedStart;

            result.set(injectedPsi.findReferenceAt(offsetInInjected));
            eltInInjected.set(injectedPsi.findElementAt(offsetInInjected));
            injectedOffset[0] = injectedStart;
          }
        });

        ref =  result.get();

        if (ref == null) {
          rangeForNamedElement = getRangeForNamedElement(eltInInjected.get(), injectedOffset[0]);
          if (rangeForNamedElement != null) return rangeForNamedElement;
        } else {
          injectionOffsetCorrection = injectedOffset[0];
        }
      }

      if (ref == null) {
        return null;
      }
    }

    final PsiElement element = ref.getElement();
    if (!element.getLanguage().isKindOf(JavaScriptSupportLoader.JAVASCRIPT.getLanguage())) return null;
    
    return element.getTextRange().shiftRight(injectionOffsetCorrection);
  }

  private static TextRange getRangeForNamedElement(final PsiElement at, int offset) {
    final PsiElement parent = at != null ? at.getParent():null;

    if (parent instanceof JSNamedElement) {
      final ASTNode node = ((JSNamedElement)parent).findNameIdentifier();

      if (node != null) {
        return node.getPsi().getTextRange().shiftRight(offset);
      }
    }
    return null;
  }

  public static Document createDocument(final String text,final Project project, @Nullable VirtualFile contextVirtualFile, int contextOffset) {
    PsiElement context = null;
    if (contextVirtualFile != null) {
      context = getContextElement(contextVirtualFile, contextOffset, project);
    }
    JSFile file = JSElementFactory.createExpressionCodeFragment(project, text, context, true);
    return PsiDocumentManager.getInstance(project).getDocument(file);
  }

  @Nullable
  public static PsiElement getContextElement(VirtualFile virtualFile, int offset,final @NotNull Project project) {
    Document document = FileDocumentManager.getInstance().getDocument(virtualFile);
    PsiFile file = PsiManager.getInstance(project).findFile(virtualFile);
    if (file == null || document == null) {
      return null;
    }

    if (offset < 0) offset = 0;
    if (offset > document.getTextLength()) offset = document.getTextLength();
    int startOffset = offset;

    int lineEndOffset = document.getLineEndOffset(document.getLineNumber(offset));
    PsiElement result = null;
    do {
      PsiElement element = file.findElementAt(offset);
      if (!(element instanceof PsiWhiteSpace) && !(element instanceof PsiComment)) {
        result = element;
        break;
      }

      offset = element.getTextRange().getEndOffset() + 1;
    }
    while (offset < lineEndOffset);

    if (result == null) {
      result = file.findElementAt(startOffset);
    }

    if (result != null && StdFileTypes.XML.getLanguage().equals(result.getLanguage())) {
      PsiLanguageInjectionHost parent = PsiTreeUtil.getParentOfType(result, PsiLanguageInjectionHost.class);

      if (parent != null) {
        final int finalOffset = offset;
        final Ref<PsiElement> resultInInjected = new Ref<PsiElement>();

        parent.processInjectedPsi(new PsiLanguageInjectionHost.InjectedPsiVisitor() {
          public void visit(@NotNull final PsiFile injectedPsi, @NotNull final List<PsiLanguageInjectionHost.Shred> places) {
            final PsiLanguageInjectionHost.Shred shred = places.get(0);
            final int injectedStart = shred.getRangeInsideHost().getStartOffset() + shred.host.getTextOffset();
            final int offsetInInjected = finalOffset - injectedStart;

            resultInInjected.set(injectedPsi.findElementAt(offsetInInjected));
          }
        });

        result = resultInInjected.get();
      }
    }
    return result;
  }
}
