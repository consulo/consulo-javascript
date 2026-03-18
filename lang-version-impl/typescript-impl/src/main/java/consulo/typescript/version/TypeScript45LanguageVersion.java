package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript45LanguageVersion extends TypeScript43LanguageVersion {
    @Inject
    public TypeScript45LanguageVersion() {
        this("TYPESCRIPT_4_5");
    }

    public TypeScript45LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.TYPE_ONLY_IMPORT_SPECIFIER);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 4.5";
    }

    @Override
    public int getWeight() {
        return 450;
    }
}
