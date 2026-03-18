package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript43LanguageVersion extends TypeScript41LanguageVersion {
    @Inject
    public TypeScript43LanguageVersion() {
        this("TYPESCRIPT_4_3");
    }

    public TypeScript43LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.OVERRIDE_MODIFIER);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 4.3";
    }

    @Override
    public int getWeight() {
        return 430;
    }
}
