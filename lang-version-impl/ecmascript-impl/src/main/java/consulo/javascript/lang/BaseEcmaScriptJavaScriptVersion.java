package consulo.javascript.lang;

import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lexer.Lexer;
import com.intellij.lexer.MergingLexerAdapter;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.psi.tree.TokenSet;
import consulo.javascript.ecmascript6.psi.impl.resolve.EcmaScript6ResolveHelper;
import consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import consulo.javascript.lang.lexer.JavaScriptFlexAdapter;
import consulo.javascript.lang.lexer._EcmaScript6Lexer;
import consulo.javascript.lang.parsing.EcmaScriptParser;
import consulo.javascript.lang.psi.impl.resolve.JavaScriptVersionWithHelper;
import consulo.javascript.lang.psi.impl.resolve.ResolveHelper;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
public abstract class BaseEcmaScriptJavaScriptVersion extends BaseJavaScriptLanguageVersion implements StandardJavaScriptVersions.Marker, JavaScriptVersionWithHelper
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