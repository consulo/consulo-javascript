package consulo.typescript.version;

import consulo.javascript.lang.BaseEcmaScriptJavaScriptVersion;
import consulo.javascript.lang.JavaScriptFeature;
import consulo.javascript.lang.JavaScriptLanguage;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 20-Jul-22
 */
public class TypeScriptLanguageVersion extends BaseEcmaScriptJavaScriptVersion
{
	@Nonnull
	public static TypeScriptLanguageVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(TypeScriptLanguageVersion.class);
	}

	public TypeScriptLanguageVersion()
	{
		super("TYPESCRIPT", "application/typescript");
		addFeature(JavaScriptFeature.CLASS);
		addFeature(JavaScriptFeature.BINARY_LITERAL);
		addFeature(JavaScriptFeature.OCTAL_LITERAL);
		addFeature(JavaScriptFeature.PARAMETER_DEFAULT_VALUE);
		addFeature(JavaScriptFeature.REST_PARAMETER);
		addFeature(JavaScriptFeature.FUNCTION_PROPERTY);
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "TypeScript";
	}
}
