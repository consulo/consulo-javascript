/*
 * @author max
 */
package com.intellij.lang.javascript;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.openapi.fileTypes.SingleLazyInstanceSyntaxHighlighterFactory;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;

public class ECMAL4LanguageDialect extends JSLanguageDialect
{
	public static final DialectOptionHolder DIALECT_OPTION_HOLDER = new DialectOptionHolder(true, false);

	public ECMAL4LanguageDialect()
	{
		super("ECMA Script Level 4");

		SyntaxHighlighterFactory.LANGUAGE_FACTORY.addExplicitExtension(this, new SingleLazyInstanceSyntaxHighlighterFactory()
		{
			@Override
			@NotNull
			protected SyntaxHighlighter createHighlighter()
			{
				return new JSHighlighter(DIALECT_OPTION_HOLDER);
			}
		});
	}

	@Override
	public String getFileExtension()
	{
		return EcmaScriptFileType.INSTANCE.getDefaultExtension();
	}

}