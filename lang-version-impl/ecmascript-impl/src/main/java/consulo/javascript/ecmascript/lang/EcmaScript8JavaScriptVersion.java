package consulo.javascript.ecmascript.lang;

import consulo.annotation.component.ExtensionImpl;
import jakarta.inject.Inject;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
@ExtensionImpl
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
