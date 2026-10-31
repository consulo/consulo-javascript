package consulo.typescript.language.impl.stub;

import consulo.javascript.module.resolve.JSModuleExport;

import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.types.JSFileElementType;
import consulo.annotation.access.RequiredReadAction;
import consulo.index.io.StringRef;
import consulo.javascript.psi.stubs.JSFileStub;
import consulo.language.psi.PsiFile;
import consulo.language.psi.stub.DefaultStubBuilder;
import consulo.language.psi.stub.StubBuilder;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.StubInputStream;
import consulo.language.psi.stub.StubOutputStream;
import consulo.typescript.language.TypeScriptLanguage;
import consulo.typescript.language.impl.psi.TypeScriptModuleUtil;

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * The file element type of TypeScript: the JavaScript file stub and whether the file is a module
 * ({@link TypeScriptFileStub}).
 *
 * @since 2026-10-31
 */
public class TypeScriptFileElementType extends JSFileElementType {
    /**
     * Bump on every change of the grammar or of the stubs of TypeScript (it adds to the JavaScript version).
     * 1: module flag in the file stub, grammar of the declaration files (interface members, keyword names, ...)
     * 2: stubbed property and construct signatures, interface extends lists, type texts
     * 3: decorators before {@code export} of a class ({@code @Component(...) export class A})
     * 4: {@code declare global} of a module: its names in the file stub, its variables stubbed
     * 5: namespaces stubbed like classes with their members (not global names)
     * 6: type aliases stubbed
     * 7: the exports of a module in the file stub
     * 8: the exports written by the JavaScript file stub (JSModuleExport), before the TypeScript data
     * 9: the return types of generic functions without their type parameters (default, else constraint)
     * 10: the members of classes and interfaces in the member name index
     */
    private static final int STUB_VERSION = 10;

    public TypeScriptFileElementType() {
        super(TypeScriptLanguage.INSTANCE);
    }

    @Override
    public StubBuilder getBuilder() {
        return new DefaultStubBuilder() {
            @Override
            @RequiredReadAction
            protected StubElement createStubForFile(PsiFile file) {
                return file instanceof JSFile jsFile
                    ? new TypeScriptFileStub(jsFile, StringRef.fromString(file.getName()), TypeScriptFileElementType.this,
                    TypeScriptModuleUtil.isModuleByTree(file), TypeScriptModuleUtil.getGlobalNamesByTree(file),
                    TypeScriptModuleUtil.getExportsByTree(file))
                    : super.createStubForFile(file);
            }
        };
    }

    @Override
    public void serialize(JSFileStub stub, StubOutputStream dataStream) throws IOException {
        super.serialize(stub, dataStream);
        TypeScriptFileStub typeScriptStub = stub instanceof TypeScriptFileStub fileStub ? fileStub : null;
        dataStream.writeBoolean(typeScriptStub != null && typeScriptStub.isModule());
        Set<String> globalNames = typeScriptStub != null ? typeScriptStub.getGlobalNames() : Set.of();
        dataStream.writeVarInt(globalNames.size());
        for (String name : globalNames) {
            dataStream.writeName(name);
        }
    }

    @Override
    public JSFileStub deserialize(StubInputStream dataStream, StubElement parentStub) throws IOException {
        StringRef name = dataStream.readName();
        List<JSModuleExport> exports = readExports(dataStream);
        boolean module = dataStream.readBoolean();
        int count = dataStream.readVarInt();
        Set<String> globalNames = new LinkedHashSet<>();
        for (int i = 0; i < count; i++) {
            globalNames.add(StringRef.toString(dataStream.readName()));
        }
        return new TypeScriptFileStub(null, name, this, module, Set.copyOf(globalNames), exports);
    }

    @Override
    public int getStubVersion() {
        return super.getStubVersion() + STUB_VERSION;
    }
}
