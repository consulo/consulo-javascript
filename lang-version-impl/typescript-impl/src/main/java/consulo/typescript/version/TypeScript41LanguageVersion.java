package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript41LanguageVersion extends TypeScript40LanguageVersion {
    @Inject
    public TypeScript41LanguageVersion() {
        this("TYPESCRIPT_4_1");
    }

    public TypeScript41LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.TEMPLATE_LITERAL_TYPE);
        addTypeScriptFeature(TypeScriptFeature.KEY_REMAPPING);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 4.1";
    }

    @Override
    public int getWeight() {
        return 410;
    }
}
