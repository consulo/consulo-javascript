package com.intellij.lang.javascript;

import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.stub.ObjectStubSerializerProvider;
import consulo.language.psi.stub.StubElementTypeHolder;
import org.jspecify.annotations.Nullable;

import java.lang.reflect.Field;
import java.util.List;

/**
 * @author VISTALL
 * @since 29-Aug-22
 */
@ExtensionImpl
public class JavaScriptStubElementHolder extends StubElementTypeHolder<JSStubElementTypes> {
    @Nullable
    @Override
    public String getExternalIdPrefix() {
        return "js.";
    }

    @Override
    public List<ObjectStubSerializerProvider> loadSerializers() {
        return allFromStaticFields(JSStubElementTypes.class, Field::get);
    }
}
