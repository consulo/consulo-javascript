package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript30LanguageVersion extends TypeScript28LanguageVersion {
    @Inject
    public TypeScript30LanguageVersion() {
        this("TYPESCRIPT_3_0");
    }

    public TypeScript30LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.UNKNOWN_TYPE);
        addTypeScriptFeature(TypeScriptFeature.REST_TUPLE);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 3.0";
    }

    @Override
    public int getWeight() {
        return 300;
    }
}
