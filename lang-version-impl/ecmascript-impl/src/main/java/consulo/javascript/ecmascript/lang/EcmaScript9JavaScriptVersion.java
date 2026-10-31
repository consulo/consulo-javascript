package consulo.javascript.ecmascript.lang;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptFeature;
import jakarta.inject.Inject;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
@ExtensionImpl
public class EcmaScript9JavaScriptVersion extends EcmaScript8JavaScriptVersion
{
	@Inject
	public EcmaScript9JavaScriptVersion()
	{
		this("ECMASCRIPT_9");
	}

	/**
	 * For the next versions: each version has the features of the previous ones.
	 */
	protected EcmaScript9JavaScriptVersion(String id)
	{
		super(id);
		addFeature(JavaScriptFeature.SPREAD_OPERATOR);
	}

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
