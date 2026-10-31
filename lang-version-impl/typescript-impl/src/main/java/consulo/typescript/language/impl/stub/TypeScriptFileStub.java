package consulo.typescript.language.impl.stub;

import consulo.javascript.module.resolve.JSModuleExport;

import com.intellij.lang.javascript.psi.JSFile;
import consulo.index.io.StringRef;
import consulo.javascript.impl.language.psi.stub.JSFileStubImpl;
import consulo.language.psi.stub.IStubFileElementType;
import org.jspecify.annotations.Nullable;

import java.util.List;
import java.util.Set;

/**
 * The stub of a TypeScript file: whether it's an ES module (an import or export at the top level) or a global script,
 * and the names a module declares globally ({@code declare global}), so the global resolve decides without loading the
 * AST of the file (lib.dom.d.ts is large).
 *
 * @since 2026-10-31
 */
public class TypeScriptFileStub extends JSFileStubImpl {
    private final IStubFileElementType myType;
    private final boolean myModule;
    private final Set<String> myGlobalNames;

    public TypeScriptFileStub(
        @Nullable JSFile file,
        StringRef name,
        IStubFileElementType type,
        boolean module,
        Set<String> globalNames,
        List<JSModuleExport> exports
    ) {
        super(file, name, exports);
        myType = type;
        myModule = module;
        myGlobalNames = globalNames;
    }

    public boolean isModule() {
        return myModule;
    }

    /**
     * The names declared by {@code declare global { ... }} of a module: global like the declarations of a script.
     */
    public Set<String> getGlobalNames() {
        return myGlobalNames;
    }


    @Override
    public IStubFileElementType getType() {
        return myType;
    }
}
