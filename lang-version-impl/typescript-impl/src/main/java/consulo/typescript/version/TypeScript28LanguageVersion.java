package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript28LanguageVersion extends TypeScript21LanguageVersion {
    @Inject
    public TypeScript28LanguageVersion() {
        this("TYPESCRIPT_2_8");
    }

    public TypeScript28LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.CONDITIONAL_TYPE);
        addTypeScriptFeature(TypeScriptFeature.INFER_KEYWORD);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 2.8";
    }

    @Override
    public int getWeight() {
        return 280;
    }
}
