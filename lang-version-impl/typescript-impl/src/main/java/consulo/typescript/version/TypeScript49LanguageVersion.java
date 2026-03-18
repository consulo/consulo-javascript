package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript49LanguageVersion extends TypeScript45LanguageVersion {
    @Inject
    public TypeScript49LanguageVersion() {
        this("TYPESCRIPT_4_9");
    }

    public TypeScript49LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.SATISFIES_OPERATOR);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 4.9";
    }

    @Override
    public int getWeight() {
        return 490;
    }
}
