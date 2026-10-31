package consulo.javascript.ecmascript.lang;

import consulo.annotation.component.ExtensionImpl;
import jakarta.inject.Inject;


/**
 * @author VISTALL
 * @since 06/12/2021
 */
@ExtensionImpl
public class EcmaScript10JavaScriptVersion extends EcmaScript9JavaScriptVersion {
    @Inject
    public EcmaScript10JavaScriptVersion() {
        this("ECMASCRIPT_10");
    }

    /**
     * For the next versions: each version has the features of the previous ones.
     */
    protected EcmaScript10JavaScriptVersion(String id) {
        super(id);
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
