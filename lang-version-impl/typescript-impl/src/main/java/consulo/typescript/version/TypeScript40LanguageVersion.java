package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript40LanguageVersion extends TypeScript30LanguageVersion {
    @Inject
    public TypeScript40LanguageVersion() {
        this("TYPESCRIPT_4_0");
    }

    public TypeScript40LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.LABELED_TUPLE);
        addTypeScriptFeature(TypeScriptFeature.VARIADIC_TUPLE);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 4.0";
    }

    @Override
    public int getWeight() {
        return 400;
    }
}
