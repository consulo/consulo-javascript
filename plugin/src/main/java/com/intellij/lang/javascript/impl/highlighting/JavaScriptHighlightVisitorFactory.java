package com.intellij.lang.javascript.impl.highlighting;

import com.intellij.lang.javascript.psi.JSFile;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.rawHighlight.HighlightVisitor;
import consulo.language.editor.rawHighlight.HighlightVisitorFactory;
import consulo.language.psi.PsiFile;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2023-03-25
 */
@ExtensionImpl
public class JavaScriptHighlightVisitorFactory implements HighlightVisitorFactory {
    @Override
    public boolean suitableForFile(@Nonnull PsiFile psiFile) {
        return psiFile instanceof JSFile;
    }

    @Nonnull
    @Override
    public HighlightVisitor createVisitor() {
        return new JavaScriptHighlightVisitor();
    }
}
