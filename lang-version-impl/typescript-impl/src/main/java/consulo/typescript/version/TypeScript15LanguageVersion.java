package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import consulo.typescript.language.TypeScriptFeature;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript15LanguageVersion extends TypeScript10LanguageVersion {
    @Inject
    public TypeScript15LanguageVersion() {
        this("TYPESCRIPT_1_5");
    }

    public TypeScript15LanguageVersion(String id) {
        super(id);
        addTypeScriptFeature(TypeScriptFeature.TYPE_ALIAS);
        addTypeScriptFeature(TypeScriptFeature.UNION_TYPE);
        addTypeScriptFeature(TypeScriptFeature.INTERSECTION_TYPE);
        addTypeScriptFeature(TypeScriptFeature.DECORATOR);
        addTypeScriptFeature(TypeScriptFeature.ABSTRACT_CLASS);
        addTypeScriptFeature(TypeScriptFeature.COMPUTED_PROPERTY);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 1.5";
    }

    @Override
    public int getWeight() {
        return 150;
    }
}
