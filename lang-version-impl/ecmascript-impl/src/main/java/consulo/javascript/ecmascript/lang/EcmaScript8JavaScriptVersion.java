package consulo.javascript.ecmascript.lang;

import consulo.annotation.component.ExtensionImpl;
import jakarta.inject.Inject;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
@ExtensionImpl
public class EcmaScript8JavaScriptVersion extends EcmaScript7JavaScriptVersion {
    @Inject
    public EcmaScript8JavaScriptVersion() {
        this("ECMASCRIPT_8");
    }

    /**
     * For the next versions: each version has the features of the previous ones.
     */
    protected EcmaScript8JavaScriptVersion(String id) {
        super(id);
    }

    @Override
    public String getPresentableName() {
        return "ECMAScript 8";
    }

    @Override
    public int getWeight() {
        return 800;
    }
}
