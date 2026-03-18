package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript21LanguageVersion extends TypeScript20LanguageVersion {
    @Inject
    public TypeScript21LanguageVersion() {
        this("TYPESCRIPT_2_1");
    }

    public TypeScript21LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.MAPPED_TYPE);
        addTypeScriptFeature(TypeScriptFeature.KEYOF_OPERATOR);
        addTypeScriptFeature(TypeScriptFeature.INDEXED_ACCESS_TYPE);
        addTypeScriptFeature(TypeScriptFeature.OBJECT_SPREAD);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 2.1";
    }

    @Override
    public int getWeight() {
        return 210;
    }
}
