package com.intellij.lang.javascript.formatter;

import com.intellij.application.options.IndentOptionsEditor;
import com.intellij.application.options.SmartIndentOptionsEditor;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.FileTypeIndentOptionsProvider;

/**
 * @author yole
 */
public class JavaScriptIndentOptionsProvider implements FileTypeIndentOptionsProvider
{
	public CodeStyleSettings.IndentOptions createIndentOptions()
	{
		return new CodeStyleSettings.IndentOptions();
	}

	public FileType getFileType()
	{
		return JavaScriptSupportLoader.JAVASCRIPT;
	}

	public IndentOptionsEditor createOptionsEditor()
	{
		return new SmartIndentOptionsEditor();
	}

	public String getPreviewText()
	{
		return "function a() {\n" +
				"    alert(\"test\");\n" +
				"}";
	}

	public void prepareForReformat(final PsiFile psiFile)
	{
	}
}
