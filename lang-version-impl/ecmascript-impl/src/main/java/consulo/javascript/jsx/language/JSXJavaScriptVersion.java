package consulo.javascript.jsx.language;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.ecmascript.lang.EcmaScript6JavaScriptVersion;
import consulo.javascript.language.JavaScriptLanguage;

/**
 * @author VISTALL
 * @since 2019-12-16
 */
@ExtensionImpl
public class JSXJavaScriptVersion extends EcmaScript6JavaScriptVersion {
    public static EcmaScript6JavaScriptVersion getInstance() {
        return JavaScriptLanguage.INSTANCE.findVersionByClass(JSXJavaScriptVersion.class);
    }

    public JSXJavaScriptVersion() {
        super("JSX");
    }

    @Override
    public int getWeight() {
        return 60;
    }

    @Override
    public String getPresentableName() {
        return "JSX";
    }
}
