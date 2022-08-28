package consulo.javascript.ecmascript.lang;

import consulo.javascript.language.JavaScriptFeature;
import jakarta.inject.Inject;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
public class EcmaScript7JavaScriptVersion extends EcmaScript6JavaScriptVersion
{
	@Inject
	public EcmaScript7JavaScriptVersion()
	{
		super("ECMASCRIPT_7");

		addFeature(JavaScriptFeature.EXPONENTIATION_OPERATOR);
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "ECMAScript 7";
	}

	@Override
	public int getWeight()
	{
		return 700;
	}
}
