package consulo.typescript.language.impl.stub;

import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.stub.ObjectStubSerializerProvider;
import consulo.language.psi.stub.StubElementTypeHolder;
import org.jspecify.annotations.Nullable;

import java.lang.reflect.Field;
import java.util.List;

@ExtensionImpl
public class TypeScriptStubElementHolder extends StubElementTypeHolder<TypeScriptStubElementTypes> {
    static final String PREFIX = "ts.";

    @Nullable
    @Override
    public String getExternalIdPrefix() {
        return PREFIX;
    }

    @Override
    public List<ObjectStubSerializerProvider> loadSerializers() {
        return allFromStaticFields(TypeScriptStubElementTypes.class, Field::get);
    }
}
