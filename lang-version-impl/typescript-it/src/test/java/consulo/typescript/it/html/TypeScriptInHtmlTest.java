package consulo.typescript.it.html;

import com.intellij.lang.javascript.psi.JSVariable;
import consulo.application.ReadAction;
import consulo.it.AllowLogError;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.PsiReference;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.typescript.language.impl.psi.TypeScriptEmbeddedContentImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * TypeScript in an HTML script tag is parsed and resolved as TypeScript, not by the JavaScript provider of its base
 * language.
 */
@ExtendWith(HeadlessProjectExtension.class)
// the XML plugin can't load completely headless
@AllowLogError({
    "consulo.language.impl.internal.psi.resolve.reference.ReferenceProvidersRegistryImpl",
    "consulo.xml.impl.internal.ExternalResource",
    "consulo.component.internal.NewExtensionPointImpl"
})
public class TypeScriptInHtmlTest {
    @Test
    public void testScriptTypeTypeScript(CodeInsightTestFixture fixture) throws Exception {
        PsiFile file = fixture.configureByText("page.html", """
            <html>
            <script type="application/typescript">
                const limit: number = 10;
                console.log(lim<caret>it);
            </script>
            </html>
            """);
        assertNotNull(ReadAction.compute(() -> PsiTreeUtil.findChildOfType(file, TypeScriptEmbeddedContentImpl.class)), "no TypeScript content");
        PsiReference reference = fixture.getReferenceAtCaretPositionWithAssertion();
        PsiElement target = ReadAction.compute(reference::resolve);
        assertInstanceOf(JSVariable.class, target);
        assertEquals("limit", ReadAction.compute(() -> ((PsiNamedElement) target).getName()));
    }
}
