package consulo.typescript.it;

import consulo.application.ReadAction;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * A const is visible in the block (or for statement) that declares it, not in the whole function like a var (issue #45).
 */
@ExtendWith(HeadlessProjectExtension.class)
public class JavaScriptBlockScopeTest {
    @Test
    public void testConstNotVisibleAfterItsBlock(CodeInsightTestFixture fixture) throws Exception {
        assertUnresolved(fixture, "function f() {\n  { const inner = 1; }\n  return inn<caret>er;\n}\n");
    }

    @Test
    public void testConstOfSiblingBlocks(CodeInsightTestFixture fixture) throws Exception {
        String text = "function foo(bar) {\n"
            + "  if (bar) { const qux = 'a'; console.log(qux); }\n"
            + "  else { const qux = 'b'; console.log(q<caret>ux); }\n"
            + "}\n";
        PsiElement target = PredefinedLibraryResolveTest.resolve(fixture, "js", text);
        assertEquals(text.indexOf("qux = 'b'"), ReadAction.compute(target::getTextOffset));
    }

    @Test
    public void testTopLevelConstUsedBeforeItsDeclaration(CodeInsightTestFixture fixture) throws Exception {
        String text = "function g() { return LIM<caret>IT; }\nconst LIMIT = 1;\n";
        PsiElement target = PredefinedLibraryResolveTest.resolve(fixture, "js", text);
        assertEquals(text.replace("<caret>", "").indexOf("LIMIT = 1"), ReadAction.compute(target::getTextOffset));
    }

    @Test
    public void testForOfConst(CodeInsightTestFixture fixture) throws Exception {
        PredefinedLibraryResolveTest.resolve(fixture, "js", "function f(a) {\n  for (const x of a) { console.log(<caret>x); }\n}\n");
        assertUnresolved(fixture, "function f(a) {\n  for (const x of a) { }\n  return <caret>x;\n}\n");
    }

    @Test
    public void testVarStaysFunctionScoped(CodeInsightTestFixture fixture) throws Exception {
        PredefinedLibraryResolveTest.resolve(fixture, "js", "function f() {\n  { var inner = 1; }\n  return inn<caret>er;\n}\n");
    }

    private static void assertUnresolved(CodeInsightTestFixture fixture, String text) throws Exception {
        fixture.configureByText("app" + Integer.toHexString(text.hashCode()) + ".js", text);
        PsiReference reference = fixture.getReferenceAtCaretPositionWithAssertion();
        assertNull(ReadAction.compute(reference::resolve), text);
    }
}
