package com.intellij.lang.javascript;

import com.intellij.codeInsight.template.TemplateContextType;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.xml.XMLLanguage;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.openapi.util.Ref;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiLanguageInjectionHost;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

/**
 * @author Maxim.Mossienko
 *         Date: Apr 14, 2008
 *         Time: 6:01:09 PM
 */
public class JavaScriptCodeContextType extends TemplateContextType {
  private static final String JAVA_SCRIPT = "JAVA_SCRIPT";

  public JavaScriptCodeContextType() {
    super(JAVA_SCRIPT, JSBundle.message("javascript.template.context.type"));
  }

  public boolean isInContext(@NotNull final PsiFile file, final int offset) {
    PsiElement at = file.findElementAt(offset);
    if (at == null && offset == file.getTextLength()) at = file.findElementAt(offset - 1);
    Language language = at != null ? at.getParent().getLanguage():null;

    if (language instanceof XMLLanguage) {
      final PsiLanguageInjectionHost host = PsiTreeUtil.getParentOfType(at, PsiLanguageInjectionHost.class, false);

      if (host != null) {
        final Ref<Boolean> hasJsInjection = new Ref<Boolean>(Boolean.FALSE);

        host.processInjectedPsi(new JSResolveUtil.JSInjectedFilesVisitor() {
          protected void process(final JSFile file) {
            hasJsInjection.set(Boolean.TRUE);
          }
        });

        if (hasJsInjection.get()) {
          language = JavaScriptSupportLoader.JAVASCRIPT.getLanguage();
        }
      }
    }
    return language != null && language.isKindOf(JavaScriptSupportLoader.JAVASCRIPT.getLanguage());
  }

  public boolean isInContext(@NotNull final FileType fileType) {
    return fileType instanceof JavaScriptFileType;
  }

  public SyntaxHighlighter createHighlighter() {
    final SyntaxHighlighterFactory factory = SyntaxHighlighterFactory.LANGUAGE_FACTORY.forLanguage(JavaScriptSupportLoader.ECMA_SCRIPT_L4);
    return factory.getSyntaxHighlighter(JavaScriptSupportLoader.ECMA_SCRIPT_L4, null, null);
  }
}
