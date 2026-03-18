package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript20LanguageVersion extends TypeScript15LanguageVersion {
    @Inject
    public TypeScript20LanguageVersion() {
        this("TYPESCRIPT_2_0");
    }

    public TypeScript20LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.READONLY_MODIFIER);
        addTypeScriptFeature(TypeScriptFeature.NEVER_TYPE);
        addTypeScriptFeature(TypeScriptFeature.NON_NULL_ASSERTION);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 2.0";
    }

    @Override
    public int getWeight() {
        return 200;
    }
}
