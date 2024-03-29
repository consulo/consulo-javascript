package consulo.javascript.ecmascript.lang;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptFeature;
import jakarta.inject.Inject;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
@ExtensionImpl
public class EcmaScript11JavaScriptVersion extends EcmaScript6JavaScriptVersion
{
	@Inject
	public EcmaScript11JavaScriptVersion()
	{
		super("ECMASCRIPT_11");

		addFeature(JavaScriptFeature.OPTIONAL_CHAINING_OPERATOR);
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
