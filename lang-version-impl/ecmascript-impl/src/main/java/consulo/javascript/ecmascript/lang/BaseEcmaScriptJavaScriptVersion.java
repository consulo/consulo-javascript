package consulo.javascript.ecmascript.lang;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.ecmascript.lang.lexer._EcmaScript6Lexer;
import consulo.javascript.ecmascript.lang.parsing.EcmaScriptParser;
import consulo.javascript.ecmascript.psi.impl.resolve.EcmaScript6ResolveHelper;
import consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.javascript.lang.lexer.JavaScriptFlexAdapter;
import consulo.javascript.lang.psi.impl.resolve.JavaScriptVersionWithHelper;
import consulo.javascript.lang.psi.impl.resolve.ResolveHelper;
import consulo.javascript.language.StandardJavaScriptVersion;
import consulo.language.ast.TokenSet;
import consulo.language.editor.highlight.SyntaxHighlighter;
import consulo.language.lexer.Lexer;
import consulo.language.lexer.MergingLexerAdapter;
import consulo.language.parser.PsiParser;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
public abstract class BaseEcmaScriptJavaScriptVersion extends BaseJavaScriptLanguageVersion implements StandardJavaScriptVersion, JavaScriptVersionWithHelper
{
	protected BaseEcmaScriptJavaScriptVersion(@Nonnull String id)
	{
		super(id);
	}

	protected BaseEcmaScriptJavaScriptVersion(String name, String... mimeTypes)
	{
		super(name, mimeTypes);
	}

	@Nonnull
	@Override
	public Lexer createLexer()
	{
		return createLexer(false);
	}

	@Nonnull
	@Override
	public SyntaxHighlighter getSyntaxHighlighter()
	{
		return new JavaScriptHighlighter(() -> createLexer(true));
	}

	private Lexer createLexer(boolean hightlight)
	{
		return new MergingLexerAdapter(new JavaScriptFlexAdapter(new _EcmaScript6Lexer(hightlight)), TokenSet.create(JSTokenTypes.XML_JS_SCRIPT));
	}

	@Nonnull
	@Override
	public PsiParser createParser()
	{
		return new EcmaScriptParser();
	}

	@Nonnull
	@Override
	public ResolveHelper getHelper()
	{
		return EcmaScript6ResolveHelper.INSTANCE;
	}
}