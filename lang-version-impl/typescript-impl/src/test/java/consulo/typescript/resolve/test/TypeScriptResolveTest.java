package consulo.typescript.resolve.test;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.language.file.LanguageFileType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.PsiReference;
import consulo.test.junit.impl.language.SimpleParsingTest;
import consulo.typescript.TypeScriptFileType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Local resolve of TypeScript references through the JavaScript resolve (processDeclarations + treeWalkUp).
 */
public class TypeScriptResolveTest extends SimpleParsingTest<Object> {
    private static final String CARET = "<caret>";

    public TypeScriptResolveTest() {
        super("resolve", "ts");
    }

    @Test
    public void testLocalVariable(Context context) throws Exception {
        PsiElement target = resolve(context, """
            const answer = 42;
            console.log(ans<caret>wer);
            """);
        assertDeclaration(target, JSVariable.class, "answer", "answer = 42");
    }

    @Test
    public void testLetInBlockNotVisibleBefore(Context context) throws Exception {
        PsiElement target = resolve(context, """
            function f() {
                let x = 1;
                return <caret>x;
            }
            """);
        assertDeclaration(target, JSVariable.class, "x", "x = 1");
    }

    @Test
    public void testParameter(Context context) throws Exception {
        PsiElement target = resolve(context, """
            function greet(name: string) {
                return "hi " + na<caret>me;
            }
            """);
        assertDeclaration(target, JSParameter.class, "name", "name: string");
    }

    @Test
    public void testArrowParameter(Context context) throws Exception {
        PsiElement target = resolve(context, "const twice = v => <caret>v * 2;");
        assertDeclaration(target, JSParameter.class, "v", "v");
    }

    @Test
    public void testArrowParameterList(Context context) throws Exception {
        PsiElement target = resolve(context, "const add = (a: number, b: number) => a + <caret>b;");
        assertDeclaration(target, JSParameter.class, "b", "b: number");
    }

    @Test
    public void testHoistedFunction(Context context) throws Exception {
        PsiElement target = resolve(context, """
            hel<caret>per();
            function helper() {
            }
            """);
        assertInstanceOf(JSFunction.class, target);
        assertEquals("helper", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testForOfVariable(Context context) throws Exception {
        PsiElement target = resolve(context, """
            for (const item of items) {
                use(it<caret>em);
            }
            """);
        assertDeclaration(target, JSVariable.class, "item", "item");
    }

    @Test
    public void testForVariable(Context context) throws Exception {
        PsiElement target = resolve(context, "for (let i = 0; <caret>i < 10; i++) {}");
        assertDeclaration(target, JSVariable.class, "i", "i = 0");
    }

    @Test
    public void testCatchParameter(Context context) throws Exception {
        PsiElement target = resolve(context, """
            try {
            } catch (err) {
                report(e<caret>rr);
            }
            """);
        assertDeclaration(target, JSParameter.class, "err", "err");
    }

    @Test
    public void testClassName(Context context) throws Exception {
        PsiElement target = resolve(context, """
            class Greeter {
            }
            const g = new Gree<caret>ter();
            """);
        assertInstanceOf(JSClass.class, target);
        assertEquals("Greeter", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testThisField(Context context) throws Exception {
        PsiElement target = resolve(context, """
            class Counter {
                count = 0;
                inc() {
                    this.co<caret>unt++;
                }
            }
            """);
        assertDeclaration(target, JSVariable.class, "count", "count = 0;");
    }

    @Test
    public void testThisMethod(Context context) throws Exception {
        PsiElement target = resolve(context, """
            class Counter {
                reset() {
                }
                inc() {
                    this.re<caret>set();
                }
            }
            """);
        assertInstanceOf(JSFunction.class, target);
        assertEquals("reset", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testConstructorParameterProperty(Context context) throws Exception {
        PsiElement target = resolve(context, """
            class Service {
                constructor(private http: Http) {
                }
                load() {
                    return this.ht<caret>tp;
                }
            }
            """);
        assertInstanceOf(JSParameter.class, target);
        assertEquals("http", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testStaticMember(Context context) throws Exception {
        PsiElement target = resolve(context, """
            class Config {
                static defaults = {};
            }
            const d = Config.def<caret>aults;
            """);
        assertInstanceOf(JSVariable.class, target);
        assertEquals("defaults", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testMemberOfNewInstance(Context context) throws Exception {
        PsiElement target = resolve(context, """
            class Dog {
                bark() {
                }
            }
            const dog = new Dog();
            dog.ba<caret>rk();
            """);
        assertInstanceOf(JSFunction.class, target);
        assertEquals("bark", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testMemberOfTypedParameter(Context context) throws Exception {
        PsiElement target = resolve(context, """
            class Dog {
                name = "rex";
            }
            function show(dog: Dog) {
                return dog.na<caret>me;
            }
            """);
        assertInstanceOf(JSVariable.class, target);
        assertEquals("name", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testInheritedMember(Context context) throws Exception {
        PsiElement target = resolve(context, """
            class Animal {
                move() {
                }
            }
            class Bird extends Animal {
                fly() {
                    this.mo<caret>ve();
                }
            }
            """);
        assertInstanceOf(JSFunction.class, target);
        assertEquals("move", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testMemberNotInScopeWithoutThis(Context context) throws Exception {
        PsiFile file = createFile(context, """
            class Counter {
                count = 0;
                inc() {
                    co<caret>unt;
                }
            }
            """);
        PsiReference reference = file.findReferenceAt(caretOffset);
        assertNotNull(reference);
        assertNull(resolveLocally(reference.getElement(), "count"));
    }

    @Test
    public void testExportedFunction(Context context) throws Exception {
        PsiElement target = resolve(context, """
            export function helper() {
            }
            hel<caret>per();
            """);
        assertInstanceOf(JSFunction.class, target);
        assertEquals("helper", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testExportedConst(Context context) throws Exception {
        PsiElement target = resolve(context, """
            export const LIMIT = 10;
            const twice = LIM<caret>IT * 2;
            """);
        assertDeclaration(target, JSVariable.class, "LIMIT", "LIMIT = 10");
    }

    @Test
    public void testImportBinding(Context context) throws Exception {
        // light files have no directory: the module isn't found and the reference stays on the import
        PsiElement target = resolve(context, """
            import { Component as Base } from './missing';
            class App extends Ba<caret>se {
            }
            """);
        assertEquals("Base", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testTypeAnnotationClass(Context context) throws Exception {
        PsiElement target = resolve(context, """
            class Dog {
            }
            let d: Do<caret>g;
            """);
        assertInstanceOf(JSClass.class, target);
        assertEquals("Dog", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testTypeAnnotationInterface(Context context) throws Exception {
        PsiElement target = resolve(context, """
            interface Shape {
            }
            function area(s: Sha<caret>pe) {
            }
            """);
        assertInstanceOf(JSClass.class, target);
        assertTrue(((JSClass) target).isInterface());
    }

    @Test
    public void testTypeAlias(Context context) throws Exception {
        PsiElement target = resolve(context, """
            type Id = string;
            const id: I<caret>d = "a";
            """);
        assertEquals("Id", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testClassTypeParameter(Context context) throws Exception {
        PsiElement target = resolve(context, """
            class Box<T> {
                value: <caret>T;
            }
            """);
        assertEquals("T", ((PsiNamedElement) target).getName());
        assertEquals("T", target.getText());
    }

    @Test
    public void testFunctionTypeParameter(Context context) throws Exception {
        PsiElement target = resolve(context, "function first<T>(items: T[]): <caret>T { return items[0]; }");
        assertEquals("T", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testTypeArgument(Context context) throws Exception {
        PsiElement target = resolve(context, """
            interface User {
            }
            const users: Array<Us<caret>er> = [];
            """);
        assertEquals("User", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testEnumMember(Context context) throws Exception {
        PsiElement target = resolve(context, """
            enum Color { Red, Green }
            const c = Color.Gre<caret>en;
            """);
        assertEquals("Green", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testEnumAsType(Context context) throws Exception {
        PsiElement target = resolve(context, """
            enum Color { Red, Green }
            let c: Col<caret>or;
            """);
        assertEquals("Color", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testNamespaceMember(Context context) throws Exception {
        PsiElement target = resolve(context, """
            namespace Geometry {
                export function area() {
                }
            }
            Geometry.ar<caret>ea();
            """);
        assertInstanceOf(JSFunction.class, target);
        assertEquals("area", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testQualifiedTypeInNamespace(Context context) throws Exception {
        PsiElement target = resolve(context, """
            namespace Shapes {
                export interface Circle {
                }
            }
            let c: Shapes.Cir<caret>cle;
            """);
        assertEquals("Circle", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testValueIgnoresInterface(Context context) throws Exception {
        // an interface and a const may share a name: the expression means the const
        PsiElement target = resolve(context, """
            interface Config {
            }
            const Config = {};
            const c = Con<caret>fig;
            """);
        assertInstanceOf(JSVariable.class, target);
    }

    @Test
    public void testObjectDestructuring(Context context) throws Exception {
        PsiElement target = resolve(context, """
            const { width, height: h } = size;
            const area = width * <caret>h;
            """);
        assertEquals("h", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testArrayDestructuringParameter(Context context) throws Exception {
        PsiElement target = resolve(context, "const sum = ([first, second]) => first + sec<caret>ond;");
        assertEquals("second", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testNestedDestructuringForOf(Context context) throws Exception {
        PsiElement target = resolve(context, """
            for (const { user: { name } } of rows) {
                print(na<caret>me);
            }
            """);
        assertEquals("name", ((PsiNamedElement) target).getName());
    }

    @Test
    public void testRename(Context context) throws Exception {
        PsiFile file = createFile(context, "const value = 1; const twice = val<caret>ue * 2;");
        PsiReference reference = file.findReferenceAt(caretOffset);
        assertNotNull(reference);
        assertEquals("value", reference.getCanonicalText());
        assertEquals("value", reference.getRangeInElement().substring(reference.getElement().getText()));
        assertTrue(reference.isReferenceTo(reference.resolve()));
    }

    private int caretOffset;

    private PsiFile createFile(Context context, String textWithCaret) {
        caretOffset = textWithCaret.indexOf(CARET);
        String text = textWithCaret.replace(CARET, "");
        return createFile(context, null, "a.ts", TypeScriptFileType.INSTANCE, text);
    }

    private PsiElement resolve(Context context, String textWithCaret) {
        PsiFile file = createFile(context, textWithCaret);
        PsiReference reference = file.findReferenceAt(caretOffset);
        assertNotNull(reference, "no reference at the caret");
        PsiElement target = reference.resolve();
        assertNotNull(target, "unresolved: " + reference.getCanonicalText());
        return target;
    }

    /**
     * The local part of the resolve (the global symbols need indexes, which light tests don't have).
     */
    private static PsiElement resolveLocally(PsiElement place, String name) {
        ResolveProcessor processor = new ResolveProcessor(name, place);
        JSResolveUtil.treeWalkUp(processor, place, place, place);
        return processor.getResult();
    }

    private static void assertDeclaration(PsiElement target, Class<? extends PsiElement> type, String name, String textStart) {
        assertInstanceOf(type, target);
        assertEquals(name, ((PsiNamedElement) target).getName());
        assertTrue(target.getText().startsWith(textStart), target.getText());
    }

    @Override
    protected LanguageFileType getFileType(Context context, Object o) {
        return TypeScriptFileType.INSTANCE;
    }
}
