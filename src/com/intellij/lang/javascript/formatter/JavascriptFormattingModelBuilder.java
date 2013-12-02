/*
 * @author max
 */
package com.intellij.lang.javascript.formatter;

import org.jetbrains.annotations.NotNull;
import com.intellij.formatting.FormattingModel;
import com.intellij.formatting.FormattingModelBuilder;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.formatter.blocks.JSBlock;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleSettings;

public class JavascriptFormattingModelBuilder implements FormattingModelBuilder
{
	@NotNull
	public FormattingModel createModel(final PsiElement element, final CodeStyleSettings settings)
	{
		final PsiFile psiFile = element.getContainingFile();

		return new JSFormattingModel(psiFile, settings, new JSBlock(psiFile instanceof JSFile ? psiFile.getNode() : element.getNode(), null, null, null,
				settings));
	}

	public TextRange getRangeAffectingIndent(PsiFile file, int offset, ASTNode elementAtOffset)
	{
		return null;
	}
}