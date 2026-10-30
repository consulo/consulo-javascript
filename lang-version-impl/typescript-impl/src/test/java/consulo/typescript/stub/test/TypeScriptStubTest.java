package consulo.typescript.stub.test;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import consulo.language.file.LanguageFileType;
import consulo.language.impl.DebugUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.IStubFileElementType;
import consulo.language.psi.stub.StubElement;
import consulo.test.junit.impl.language.SimpleParsingTest;
import consulo.typescript.TypeScriptFileType;
import org.junit.jupiter.api.Test;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

/**
 * TypeScript declarations build JavaScript stubs (indexing needs the index infrastructure: see the integration tests).
 */
public class TypeScriptStubTest extends SimpleParsingTest<Object> {
    private static final String TEXT = """
        const answer = 42;
        let a = 1, b = 2;
        function helper(x: number) {
            const local = x;
        }
        class Greeter extends Base {
            count = 0;
            static create() {
            }
            constructor(private name: string) {
            }
        }
        interface Shape {
            area(): number;
        }
        export const exported = 1;
        export class Exported {
        }
        """;

    public TypeScriptStubTest() {
        super("stub", "ts");
    }

    @Test
    public void testStubTree(Context context) throws Exception {
        PsiFile file = createFile(context, null, "a.ts", TypeScriptFileType.INSTANCE, TEXT);
        StubElement<?> root = buildStubTree(file);
        assertEquals("""
            JSFileStubImpl
              VAR_STATEMENT:JSVarStatementStubImpl
                VARIABLE_DECLARATION:JSVariableStubImpl
              VAR_STATEMENT:JSVarStatementStubImpl
                VARIABLE_DECLARATION:JSVariableStubImpl
                VARIABLE_DECLARATION:JSVariableStubImpl
              FUNCTION_DECLARATION:JSFunctionStubImpl
              CLASS_DECLARATION:JSClassStubImpl
                PROPERTY_DECLARATION:JSVariableStubImpl
                METHOD_DECLARATION:JSFunctionStubImpl
                CONSTRUCTOR_DECLARATION:JSFunctionStubImpl
              INTERFACE_DECLARATION:JSClassStubImpl
                METHOD_SIGNATURE_MEMBER:JSFunctionStubImpl
              VAR_STATEMENT:JSVarStatementStubImpl
                VARIABLE_DECLARATION:JSVariableStubImpl
              CLASS_DECLARATION:JSClassStubImpl
            """, DebugUtil.stubTreeToString(root).replaceAll("@[0-9a-f]+", "").replaceAll("[ \\t]+\\n", "\n"));
    }

    @Test
    public void testPsiFromStubs(Context context) throws Exception {
        PsiFile file = createFile(context, null, "a.ts", TypeScriptFileType.INSTANCE, TEXT);
        StubElement<?> root = buildStubTree(file);
        StubElement<?> classStub = root.getChildrenStubs().get(3);
        PsiElement psi = ((IStubElementType) classStub.getStubType()).createPsi(classStub);
        assertInstanceOf(JSClass.class, psi);
        assertEquals("Greeter", ((JSClass) psi).getName());
        StubElement<?> functionStub = root.getChildrenStubs().get(2);
        PsiElement function = ((IStubElementType) functionStub.getStubType()).createPsi(functionStub);
        assertInstanceOf(JSFunction.class, function);
        assertEquals("helper", ((JSFunction) function).getName());
    }

    private static StubElement<?> buildStubTree(PsiFile file) {
        IStubFileElementType<?> type = (IStubFileElementType<?>) file.getNode().getElementType();
        return (StubElement<?>) type.getBuilder().buildStubTree(file);
    }

    @Override
    protected LanguageFileType getFileType(Context context, Object o) {
        return TypeScriptFileType.INSTANCE;
    }
}
