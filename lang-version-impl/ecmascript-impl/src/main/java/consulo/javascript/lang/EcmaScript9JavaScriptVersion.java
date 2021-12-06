package consulo.javascript.lang;

import jakarta.inject.Inject;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
public class EcmaScript9JavaScriptVersion extends EcmaScript6JavaScriptVersion
{
	@Inject
	public EcmaScript9JavaScriptVersion()
	{
		super("ECMASCRIPT_9");
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "ECMAScript 9";
	}

	@Override
	public int getWeight()
	{
		return 900;
	}
}
