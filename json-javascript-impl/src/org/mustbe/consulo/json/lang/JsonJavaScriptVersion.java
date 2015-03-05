package org.mustbe.consulo.json.lang;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import org.mustbe.consulo.json.JsonFileType;
import com.intellij.lang.LanguageVersionWithDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JSONLexer;
import com.intellij.lang.javascript.JavaScriptParsingLexer;
import com.intellij.lang.javascript.JavascriptLanguage;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JsonJavaScriptVersion extends BaseJavaScriptLanguageVersion implements LanguageVersionWithDefinition<JavascriptLanguage>
{
	private DialectOptionHolder myDialectOptionHolder = new DialectOptionHolder(false, false);

	public JsonJavaScriptVersion()
	{
		super("JSON");
	}

	@NotNull
	@Override
	public JSHighlighter getSyntaxHighlighter()
	{
		return new JSHighlighter(myDialectOptionHolder)
		{
			@Override
			@NotNull
			public Lexer getHighlightingLexer()
			{
				return new JSONLexer(super.getHighlightingLexer());
			}
		};
	}

	@NotNull
	@Override
	public Lexer createLexer(@Nullable Project project)
	{
		return new JSONLexer(new JavaScriptParsingLexer(myDialectOptionHolder));
	}

	@NotNull
	@Override
	public PsiParser createParser(@Nullable Project project)
	{
		return new JsonJavaScriptParser();
	}

	@Override
	public boolean isMyElement(@Nullable PsiElement element)
	{
		return element != null && element.getContainingFile().getFileType() == JsonFileType.INSTANCE;
	}

	@Override
	public boolean isMyFile(@Nullable Project project, @Nullable VirtualFile virtualFile)
	{
		return !(project == null || virtualFile == null) && virtualFile.getFileType() == JsonFileType.INSTANCE;
	}
}
