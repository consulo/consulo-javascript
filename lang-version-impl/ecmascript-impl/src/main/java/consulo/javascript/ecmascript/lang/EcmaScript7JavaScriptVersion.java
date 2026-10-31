package consulo.javascript.ecmascript.lang;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptFeature;
import jakarta.inject.Inject;


/**
 * @author VISTALL
 * @since 06/12/2021
 */
@ExtensionImpl
public class EcmaScript7JavaScriptVersion extends EcmaScript6JavaScriptVersion {
    @Inject
    public EcmaScript7JavaScriptVersion() {
        this("ECMASCRIPT_7");
    }

    /**
     * For the next versions: each version has the features of the previous ones.
     */
    protected EcmaScript7JavaScriptVersion(String id) {
        super(id);
        addFeature(JavaScriptFeature.EXPONENTIATION_OPERATOR);
    }

    @Override
    public String getPresentableName() {
        return "ECMAScript 7";
    }

    @Override
    public int getWeight() {
        return 700;
    }
}
