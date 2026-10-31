package consulo.typescript.it.html;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.application.ReadAction;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.javascript.jsx.language.JSXJavaScriptVersion;
import consulo.language.lexer.Lexer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * JSX in the code of a JSX {...} in the code of a JSX {...} is highlighted as JSX (issue #20): the highlighting lexer
 * lexes the code of a {...} with itself, at every depth.
 */
@ExtendWith(HeadlessProjectExtension.class)
public class JSXHighlightingLexerTest {
    @Test
    public void testNestedEmbeddedExpressions(CodeInsightTestFixture fixture) throws Exception {
        String text = "var e = <Container>\n"
            + "{\n"
            + "  <Container className=\"data-container\">\n"
            + "    {\n"
            + "      owner.agreementEnd != null ?\n"
            + "      (\n"
            + "        <Row>\n"
            + "          <Col>Agreementend</Col>\n"
            + "        </Row>\n"
            + "      ) : ''\n"
            + "    }\n"
            + "  </Container>\n"
            + "}\n"
            + "</Container>;\n";
        List<String> tagNames = ReadAction.compute(() -> {
            Lexer lexer = JSXJavaScriptVersion.getInstance().getSyntaxHighlighter().getHighlightingLexer();
            lexer.start(text);
            List<String> result = new ArrayList<>();
            while (lexer.getTokenType() != null) {
                if (lexer.getTokenType() == JSTokenTypes.XML_TAG_NAME) {
                    result.add(text.substring(lexer.getTokenStart(), lexer.getTokenEnd()));
                }
                lexer.advance();
            }
            return result;
        });
        assertEquals(List.of("Container", "Container", "Row", "Col", "Col", "Row", "Container", "Container"), tagNames);
    }
}
