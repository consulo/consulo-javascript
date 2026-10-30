package consulo.typescript.it;

import com.intellij.lang.javascript.patterns.JSPatterns;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import consulo.application.ReadAction;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The IntelliJ-compatible JavaScript patterns as Symfony uses them (inside() needs the injection manager, so headless).
 */
@ExtendWith(HeadlessProjectExtension.class)
public class JavaScriptApiTest {
    @Test
    public void testLiteralInsideArgument(CodeInsightTestFixture fixture) throws Exception {
        PsiFile file = fixture.configureByText("routes.ts", """
            Routing.generate('route_a', { id: 'not_first' });
            new Request('url');
            axios.create({ baseURL: 'base' });
            other('nope');
            """);
        List<String> matched = ReadAction.compute(() -> {
            List<String> values = new ArrayList<>();
            for (JSLiteralExpression literal : PsiTreeUtil.findChildrenOfType(file, JSLiteralExpression.class)) {
                if (JSPatterns.jsLiteralExpression().inside(false, JSPatterns.or(
                    JSPatterns.jsArgument(JSPatterns.jsReferenceExpression().withQualifiedName("Routing.generate"), 0),
                    JSPatterns.jsArgument(JSPatterns.jsReferenceExpression().withQualifiedName("Request"), 0)
                )).accepts(literal)) {
                    values.add(literal.getStringValue());
                }
            }
            return values;
        });
        assertEquals(List.of("route_a", "url"), matched);
    }
}
