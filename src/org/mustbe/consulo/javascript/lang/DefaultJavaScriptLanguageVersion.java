package org.mustbe.consulo.javascript.lang;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.LanguageVersionWithDefinition;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class DefaultJavaScriptLanguageVersion extends BaseJavaScriptLanguageVersion implements LanguageVersionWithDefinition<JavaScriptLanguage>
{
	public DefaultJavaScriptLanguageVersion()
	{
		super("DEFAULT");
	}

	@Override
	public boolean isMyElement(@Nullable PsiElement element)
	{
		return true;
	}

	@Override
	public boolean isMyFile(@Nullable Project project, @Nullable VirtualFile virtualFile)
	{
		return true;
	}

	@NotNull
	@Override
	public Lexer createLexer(@Nullable Project project)
	{
		return new JavaScriptParsingLexer(new DialectOptionHolder(false, false));
	}

	@NotNull
	@Override
	public JSHighlighter getSyntaxHighlighter()
	{
		return new JSHighlighter(new DialectOptionHolder(false, false));
	}
}
