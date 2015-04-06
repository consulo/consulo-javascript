package org.mustbe.consulo.actionscript.lang;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.actionscript.ActionScriptFileType;
import org.mustbe.consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import org.mustbe.consulo.javascript.lang.parsing.EcmaScript4Parser;
import com.intellij.lang.LanguageVersionWithDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingLexer;
import com.intellij.lang.javascript.JavascriptLanguage;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;

/**
 * @author VISTALL
 * @since 06.04.2015
 */
public class ActionScriptLanguageVersion extends BaseJavaScriptLanguageVersion implements LanguageVersionWithDefinition<JavascriptLanguage>
{
	private final DialectOptionHolder myDialectOptionHolder = new DialectOptionHolder(true, false);

	public ActionScriptLanguageVersion()
	{
		super("ACTIONSCRIPT");
	}

	@NotNull
	@Override
	public PsiParser createParser(@Nullable Project project)
	{
		return new EcmaScript4Parser();
	}

	@NotNull
	@Override
	public JSHighlighter getSyntaxHighlighter()
	{
		return new JSHighlighter(myDialectOptionHolder);
	}

	@NotNull
	@Override
	public Lexer createLexer(@Nullable Project project)
	{
		return new JavaScriptParsingLexer(myDialectOptionHolder);
	}

	@Override
	public boolean isMyElement(@Nullable PsiElement element)
	{
		return element != null && element.getContainingFile().getFileType() == ActionScriptFileType.INSTANCE;
	}

	@Override
	public boolean isMyFile(@Nullable Project project, @Nullable VirtualFile virtualFile)
	{
		return !(project == null || virtualFile == null) && virtualFile.getFileType() == ActionScriptFileType.INSTANCE;
	}
}
