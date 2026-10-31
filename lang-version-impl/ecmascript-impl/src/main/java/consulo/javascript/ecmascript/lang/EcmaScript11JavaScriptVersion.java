package consulo.javascript.ecmascript.lang;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptFeature;
import jakarta.inject.Inject;

/**
 * @author VISTALL
 * @since 06/12/2021
 */
@ExtensionImpl
public class EcmaScript11JavaScriptVersion extends EcmaScript10JavaScriptVersion {
    @Inject
    public EcmaScript11JavaScriptVersion() {
        this("ECMASCRIPT_11");
    }

    /**
     * For the next versions: each version has the features of the previous ones.
     */
    protected EcmaScript11JavaScriptVersion(String id) {
        super(id);
        addFeature(JavaScriptFeature.OPTIONAL_CHAINING_OPERATOR);
    }

    @Override
    public String getPresentableName() {
        return "ECMAScript 11";
    }

    @Override
    public int getWeight() {
        return 1100;
    }
}
