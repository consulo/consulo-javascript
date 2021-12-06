package consulo.javascript.jsx.language;

import consulo.javascript.lang.EcmaScript6JavaScriptVersion;
import consulo.javascript.lang.JavaScriptLanguage;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-16
 */
public class JSXJavaScriptVersion extends EcmaScript6JavaScriptVersion
{
	@Nonnull
	public static EcmaScript6JavaScriptVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(JSXJavaScriptVersion.class);
	}

	public JSXJavaScriptVersion()
	{
		super("JSX");
	}

	@Override
	public int getWeight()
	{
		return 60;
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "JSX";
	}
}
