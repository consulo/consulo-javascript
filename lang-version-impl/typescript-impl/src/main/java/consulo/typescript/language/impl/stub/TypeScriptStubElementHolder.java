package consulo.typescript.language.impl.stub;

import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.stub.ObjectStubSerializerProvider;
import consulo.language.psi.stub.StubElementTypeHolder;
import org.jspecify.annotations.Nullable;

import java.lang.reflect.Field;
import java.util.List;

/**
 * Not lazy: the stub element types are created when the serializers are loaded, before the index is initialized. As a
 * lazy holder, a parser creating them later intermittently failed with "created too late" for {@code EXTENDS_LIST}
 * (several applications in one test JVM).
 */
@ExtensionImpl
public class TypeScriptStubElementHolder extends StubElementTypeHolder<TypeScriptStubElementTypes> {
    static final String PREFIX = "ts.";

    @Nullable
    @Override
    public String getExternalIdPrefix() {
        return null;
    }

    @Override
    public List<ObjectStubSerializerProvider> loadSerializers() {
        return allFromStaticFields(TypeScriptStubElementTypes.class, Field::get);
    }
}
