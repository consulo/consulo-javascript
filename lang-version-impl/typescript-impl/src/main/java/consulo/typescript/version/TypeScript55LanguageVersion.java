package consulo.typescript.version;

import consulo.annotation.component.ExtensionImpl;
import jakarta.inject.Inject;

@ExtensionImpl
public class TypeScript55LanguageVersion extends TypeScript53LanguageVersion {
    @Inject
    public TypeScript55LanguageVersion() {
        this("TYPESCRIPT_5_5");
    }

    public TypeScript55LanguageVersion(String id) {
        super(id);
    }

    @Override
    public String getPresentableName() {
        return "TypeScript 5.5";
    }

    @Override
    public int getWeight() {
        return 550;
    }
}
