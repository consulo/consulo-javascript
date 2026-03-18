package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript59LanguageVersion extends TypeScript55LanguageVersion {
    @Inject
    public TypeScript59LanguageVersion() {
        this("TYPESCRIPT_5_9");
    }

    public TypeScript59LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.IMPORT_DEFER);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 5.9";
    }

    @Override
    public int getWeight() {
        return 590;
    }
}
