package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript60LanguageVersion extends TypeScript59LanguageVersion {
    @Inject
    public TypeScript60LanguageVersion() {
        this("TYPESCRIPT_6_0");
    }

    public TypeScript60LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.SUBPATH_IMPORTS);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 6.0";
    }

    @Override
    public int getWeight() {
        return 600;
    }
}
