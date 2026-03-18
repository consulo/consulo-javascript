package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript50LanguageVersion extends TypeScript49LanguageVersion {
    @Inject
    public TypeScript50LanguageVersion() {
        this("TYPESCRIPT_5_0");
    }

    public TypeScript50LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.CONST_TYPE_PARAMETER);
        addTypeScriptFeature(TypeScriptFeature.DECORATOR_METADATA);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 5.0";
    }

    @Override
    public int getWeight() {
        return 500;
    }
}
