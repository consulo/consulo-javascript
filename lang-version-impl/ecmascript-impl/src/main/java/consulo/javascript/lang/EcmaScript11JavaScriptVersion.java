package consulo.javascript.lang;

import jakarta.inject.Inject;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
public class EcmaScript11JavaScriptVersion extends EcmaScript6JavaScriptVersion
{
	@Inject
	public EcmaScript11JavaScriptVersion()
	{
		super("ECMASCRIPT_11");
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "ECMAScript 11";
	}

	@Override
	public int getWeight()
	{
		return 1100;
	}
}
