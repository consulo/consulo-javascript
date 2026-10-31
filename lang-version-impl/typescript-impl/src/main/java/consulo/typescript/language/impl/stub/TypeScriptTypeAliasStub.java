package consulo.typescript.language.impl.stub;

import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.stubs.JSQualifiedStub;
import consulo.index.io.StringRef;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.StubBase;
import consulo.language.psi.stub.StubElement;
import org.jspecify.annotations.Nullable;

/**
 * The stub of {@code type Record<K, V> = ...}: its name and qualified name ({@code Intl.LocalesArgument}).
 *
 * @since 2026-10-31
 */
public class TypeScriptTypeAliasStub extends StubBase<JSQualifiedNamedElement> implements JSQualifiedStub<JSQualifiedNamedElement> {
    private final @Nullable StringRef myName;
    private final @Nullable StringRef myQualifiedName;

    public TypeScriptTypeAliasStub(
        @Nullable StringRef name,
        @Nullable StringRef qualifiedName,
        @Nullable StubElement parent,
        IStubElementType elementType
    ) {
        super(parent, elementType);
        myName = name;
        myQualifiedName = qualifiedName;
    }

    @Nullable
    @Override
    public String getName() {
        return StringRef.toString(myName);
    }

    @Nullable
    @Override
    public String getQualifiedName() {
        return StringRef.toString(myQualifiedName);
    }

    @Override
    public int getFlags() {
        return 0;
    }
}
