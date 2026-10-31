package consulo.typescript.stub.test;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.lang.javascript.psi.stubs.JSReferenceListStub;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import consulo.language.file.LanguageFileType;
import consulo.language.impl.DebugUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.IStubFileElementType;
import consulo.language.psi.stub.StubElement;
import consulo.test.junit.impl.language.SimpleParsingTest;
import consulo.typescript.TypeScriptFileType;
import consulo.typescript.language.impl.stub.TypeScriptFileStub;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;


import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
            TypeScriptFileStub
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
    public void testDeclarationFileStubs(Context context) throws Exception {
        PsiFile file = createFile(context, null, "lib.d.ts", TypeScriptFileType.INSTANCE, """
            interface Element extends Node, ParentNode {
                readonly id: string;
                focus(): void;
            }
            interface DateConstructor {
                new (): Date;
            }
            declare var Image: {
                new (width?: number): HTMLImageElement;
            };
            declare var document: Document;
            """);
        StubElement<?> root = buildStubTree(file);
        assertEquals("""
            TypeScriptFileStub
              INTERFACE_DECLARATION:JSClassStubImpl
                EXTENDS_LIST:JSReferenceListStubImpl
                PROPERTY_SIGNATURE_MEMBER:JSVariableStubImpl
                METHOD_SIGNATURE_MEMBER:JSFunctionStubImpl
              INTERFACE_DECLARATION:JSClassStubImpl
                CONSTRUCT_SIGNATURE_MEMBER:JSFunctionStubImpl
              VAR_STATEMENT:JSVarStatementStubImpl
                VARIABLE_DECLARATION:JSVariableStubImpl
                  CONSTRUCT_SIGNATURE_MEMBER:JSFunctionStubImpl
              VAR_STATEMENT:JSVarStatementStubImpl
                VARIABLE_DECLARATION:JSVariableStubImpl
            """, DebugUtil.stubTreeToString(root).replaceAll("@[0-9a-f]+", "").replaceAll("[ \\t]+\\n", "\n"));

        List<StubElement> interfaces = root.getChildrenStubs();
        assertArrayEquals(new String[]{"Node", "ParentNode"}, ((JSReferenceListStub) interfaces.get(0).getChildrenStubs().get(0)).getReferenceTexts());
        assertEquals("id", ((JSVariableStub) interfaces.get(0).getChildrenStubs().get(1)).getName());
        assertEquals("string", ((JSVariableStub) interfaces.get(0).getChildrenStubs().get(1)).getTypeString());
        assertEquals("Date", ((JSFunctionStub) interfaces.get(1).getChildrenStubs().get(0)).getReturnTypeString());
        StubElement<?> image = (StubElement<?>) root.getChildrenStubs().get(2).getChildrenStubs().get(0);
        assertNull(((JSVariableStub) image).getTypeString(), "an inline object type has no type text");
        assertEquals("HTMLImageElement", ((JSFunctionStub) image.getChildrenStubs().get(0)).getReturnTypeString());
        assertEquals("Document", ((JSVariableStub) root.getChildrenStubs().get(3).getChildrenStubs().get(0)).getTypeString());
    }

    @Test
    public void testGlobalAugmentation(Context context) throws Exception {
        PsiFile file = createFile(context, null, "imports.d.ts", TypeScriptFileType.INSTANCE, """
            export {}
            declare global {
                const useCounter: () => number
                function navigateTo(path: string): void
                interface NuxtApp {
                }
            }
            """);
        TypeScriptFileStub stub = (TypeScriptFileStub) buildStubTree(file);
        assertTrue(stub.isModule());
        assertEquals(Set.of("useCounter", "navigateTo", "NuxtApp"), stub.getGlobalNames());
        // the variable of the block is stubbed (indexed) like a top-level one
        assertEquals("""
            TypeScriptFileStub
              VAR_STATEMENT:JSVarStatementStubImpl
                VARIABLE_DECLARATION:JSVariableStubImpl
              FUNCTION_DECLARATION:JSFunctionStubImpl
              INTERFACE_DECLARATION:JSClassStubImpl
            """, DebugUtil.stubTreeToString(stub).replaceAll("@[0-9a-f]+", "").replaceAll("[ \\t]+\\n", "\n"));
    }

    @Test
    public void testModuleFlag(Context context) throws Exception {
        PsiFile module = createFile(context, null, "a.ts", TypeScriptFileType.INSTANCE, TEXT);
        assertTrue(((TypeScriptFileStub) buildStubTree(module)).isModule());

        PsiFile script = createFile(context, null, "lib.d.ts", TypeScriptFileType.INSTANCE, "declare var Date: DateConstructor;\ninterface Date {\n}\n");
        assertFalse(((TypeScriptFileStub) buildStubTree(script)).isModule());
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
