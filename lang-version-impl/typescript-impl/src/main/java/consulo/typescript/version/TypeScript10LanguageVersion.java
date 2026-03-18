package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript10LanguageVersion extends BaseTypeScriptLanguageVersion {
    @Inject
    public TypeScript10LanguageVersion() {
        this("TYPESCRIPT_1_0");
    }

    public TypeScript10LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.INTERFACE);
        addTypeScriptFeature(TypeScriptFeature.ENUM);
        addTypeScriptFeature(TypeScriptFeature.GENERIC);
        addTypeScriptFeature(TypeScriptFeature.TYPE_ANNOTATION);
        addTypeScriptFeature(TypeScriptFeature.ACCESS_MODIFIER);
        addTypeScriptFeature(TypeScriptFeature.NAMESPACE);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 1.0";
    }

    @Override
    public int getWeight() {
        return 100;
    }
}
