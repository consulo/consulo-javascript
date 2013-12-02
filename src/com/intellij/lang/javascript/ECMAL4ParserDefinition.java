/*
 * @author max
 */
package com.intellij.lang.javascript;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.LanguageVersion;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.parsing.JSParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.tree.IFileElementType;

public class ECMAL4ParserDefinition extends JavascriptParserDefinition
{
	@NotNull
	public Lexer createLexer(final Project project, LanguageVersion languageVersion)
	{
		return new JavaScriptParsingLexer(ECMAL4LanguageDialect.DIALECT_OPTION_HOLDER);
	}

	public IFileElementType getFileNodeType()
	{
		return JSElementTypes.ECMA4_FILE;
	}


	@NotNull
	public PsiParser createParser(final Project project, LanguageVersion languageVersion)
	{
		return new JSParser(JavaScriptSupportLoader.ECMA_SCRIPT_L4);
	}
}