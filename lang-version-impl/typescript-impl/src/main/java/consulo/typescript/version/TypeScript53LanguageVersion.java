package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript53LanguageVersion extends TypeScript50LanguageVersion {
    @Inject
    public TypeScript53LanguageVersion() {
        this("TYPESCRIPT_5_3");
    }

    public TypeScript53LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.IMPORT_ATTRIBUTES);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 5.3";
    }

    @Override
    public int getWeight() {
        return 530;
    }
}
