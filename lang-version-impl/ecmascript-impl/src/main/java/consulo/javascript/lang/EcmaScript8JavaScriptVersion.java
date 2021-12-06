package consulo.javascript.lang;

import jakarta.inject.Inject;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
public class EcmaScript8JavaScriptVersion extends EcmaScript6JavaScriptVersion
{
	@Inject
	public EcmaScript8JavaScriptVersion()
	{
		super("ECMASCRIPT_8");
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "ECMAScript 8";
	}

	@Override
	public int getWeight()
	{
		return 800;
	}
}
