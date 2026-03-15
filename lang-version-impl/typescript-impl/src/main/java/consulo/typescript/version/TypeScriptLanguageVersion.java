package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.ecmascript.lang.BaseEcmaScriptJavaScriptVersion;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.language.JavaScriptLanguage;


/**
 * @author VISTALL
 * @since 20-Jul-22
 */
@ExtensionImpl
public class TypeScriptLanguageVersion extends BaseEcmaScriptJavaScriptVersion {
    public static TypeScriptLanguageVersion getInstance() {
        return JavaScriptLanguage.INSTANCE.findVersionByClass(TypeScriptLanguageVersion.class);
    }

    public TypeScriptLanguageVersion() {
        super("TYPESCRIPT", "application/typescript");
        addFeature(JavaScriptFeature.CLASS);
        addFeature(JavaScriptFeature.BINARY_LITERAL);
        addFeature(JavaScriptFeature.OCTAL_LITERAL);
        addFeature(JavaScriptFeature.PARAMETER_DEFAULT_VALUE);
        addFeature(JavaScriptFeature.REST_PARAMETER);
        addFeature(JavaScriptFeature.FUNCTION_PROPERTY);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript";
    }
}
