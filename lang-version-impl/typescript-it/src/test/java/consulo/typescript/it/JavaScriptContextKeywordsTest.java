package consulo.typescript.it;

import consulo.application.ReadAction;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.psi.PsiErrorElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The context keywords of JavaScript (async, await, of, get, set, static, as, from) stay keywords for the JavaScript
 * parser after TypeScript was parsed: the keyword map is shared by text, and the TypeScript tokens replaced the
 * JavaScript ones in it.
 */
@ExtendWith(HeadlessProjectExtension.class)
public class JavaScriptContextKeywordsTest {
    @Test
    public void testJavaScriptAfterTypeScript(CodeInsightTestFixture fixture) throws Exception {
        PsiFile typeScript = fixture.configureByText("a.ts", "export const a = async () => { for await (const x of []) { } };\n");
        ReadAction.run(() -> typeScript.getNode().getChildren(null));

        PsiFile javaScript = fixture.configureByText("b.js",
            "import a from './a';\n" +
            "import * as ns from './a';\n" +
            "const h = async () => { await h(); };\n" +
            "const o = { async m() { await h(); }, get v() { return 1; }, set v(x) { } };\n" +
            "class C { static async s() { for (const x of [a, ns]) { await x; } } }\n"
        );
        List<String> errors = ReadAction.compute(() -> PsiTreeUtil.collectElementsOfType(javaScript, PsiErrorElement.class)
            .stream()
            .map(e -> e.getTextOffset() + ": " + e.getErrorDescription())
            .toList());
        assertEquals(List.of(), errors);
    }
}
