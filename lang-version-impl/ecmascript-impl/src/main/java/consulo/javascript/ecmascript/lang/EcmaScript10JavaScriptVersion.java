package consulo.javascript.ecmascript.lang;

import consulo.annotation.component.ExtensionImpl;
import jakarta.inject.Inject;


/**
 * @author VISTALL
 * @since 06/12/2021
 */
@ExtensionImpl
public class EcmaScript10JavaScriptVersion extends EcmaScript6JavaScriptVersion {
    @Inject
    public EcmaScript10JavaScriptVersion() {
        super("ECMASCRIPT_10");
    }

    @Override
    public String getPresentableName() {
        return "ECMAScript 10";
    }

    @Override
    public int getWeight() {
        return 1000;
    }
}
