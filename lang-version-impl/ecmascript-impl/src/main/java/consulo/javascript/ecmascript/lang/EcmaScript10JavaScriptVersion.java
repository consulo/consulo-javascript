package consulo.javascript.ecmascript.lang;

import jakarta.inject.Inject;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
public class EcmaScript10JavaScriptVersion extends EcmaScript6JavaScriptVersion
{
	@Inject
	public EcmaScript10JavaScriptVersion()
	{
		super("ECMASCRIPT_10");
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "ECMAScript 10";
	}

	@Override
	public int getWeight()
	{
		return 1000;
	}
}
