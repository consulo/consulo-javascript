package consulo.javascript.jsx.codeInsight.daemon.impl.tagTreeHighlighting;

import com.intellij.codeHighlighting.Pass;
import com.intellij.codeHighlighting.TextEditorHighlightingPass;
import com.intellij.codeHighlighting.TextEditorHighlightingPassFactory;
import com.intellij.codeInsight.daemon.impl.tagTreeHighlighting.XmlTagTreeHighlightingPass;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.ex.EditorEx;
import com.intellij.psi.PsiFile;
import consulo.javascript.lang.EcmaScript6JavaScriptVersion;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-15
 */
public class JSXTagTreeHighlightingPassFactory implements TextEditorHighlightingPassFactory
{
	@Override
	public void register(@Nonnull Registrar registrar)
	{
		registrar.registerTextEditorHighlightingPass(this, new int[]{Pass.UPDATE_ALL}, null, false, -1);
	}

	@Override
	public TextEditorHighlightingPass createHighlightingPass(@Nonnull final PsiFile file, @Nonnull final Editor editor)
	{
		if(editor.isOneLineMode())
		{
			return null;
		}

		if(!(editor instanceof EditorEx) || !(file.getLanguageVersion() instanceof EcmaScript6JavaScriptVersion))
		{
			return null;
		}

		return new XmlTagTreeHighlightingPass(file, (EditorEx) editor);
	}
}


