package consulo.javascript.jsx.codeInsight;

import consulo.annotation.component.ExtensionImpl;
import consulo.codeEditor.Editor;
import consulo.codeEditor.EditorEx;
import consulo.javascript.jsx.language.JSXJavaScriptVersion;
import consulo.language.editor.Pass;
import consulo.language.editor.impl.highlight.TextEditorHighlightingPass;
import consulo.language.editor.impl.highlight.TextEditorHighlightingPassFactory;
import consulo.language.psi.PsiFile;
import consulo.xml.codeInsight.daemon.impl.tagTreeHighlighting.XmlTagTreeHighlightingPass;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-15
 */
@ExtensionImpl
public class JSXTagTreeHighlightingPassFactory implements TextEditorHighlightingPassFactory {
    @Override
    public void register(@Nonnull Registrar registrar) {
        registrar.registerTextEditorHighlightingPass(this, new int[]{Pass.UPDATE_ALL}, null, false, -1);
    }

    @Override
    public TextEditorHighlightingPass createHighlightingPass(@Nonnull final PsiFile file, @Nonnull final Editor editor) {
        if (editor.isOneLineMode() || !(editor instanceof EditorEx) || !(file.getLanguageVersion() instanceof JSXJavaScriptVersion)) {
            return null;
        }

        return new XmlTagTreeHighlightingPass(file, (EditorEx)editor);
    }
}


