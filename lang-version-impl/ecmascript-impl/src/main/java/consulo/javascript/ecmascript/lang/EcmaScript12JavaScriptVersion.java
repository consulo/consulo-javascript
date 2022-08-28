package consulo.javascript.ecmascript.lang;

import consulo.javascript.language.JavaScriptLanguage;
import jakarta.inject.Inject;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
public class EcmaScript12JavaScriptVersion extends EcmaScript6JavaScriptVersion
{
	@Nonnull
	public static EcmaScript12JavaScriptVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(EcmaScript12JavaScriptVersion.class);
	}

	@Inject
	public EcmaScript12JavaScriptVersion()
	{
		super("ECMASCRIPT_12");
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "ECMAScript 12";
	}

	@Override
	public int getWeight()
	{
		return 1200;
	}
}
