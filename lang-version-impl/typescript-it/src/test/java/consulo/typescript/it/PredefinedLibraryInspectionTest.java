package consulo.typescript.it;

import com.intellij.lang.javascript.impl.inspections.JSUnresolvedFunctionInspection;
import consulo.application.ReadAction;
import consulo.it.AllowLogError;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.editor.rawHighlight.HighlightInfo;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The function checks on the declarations of the standard library (issue #22): a TypeScript variable with a construct
 * signature ({@code declare var Date: DateConstructor}) is callable, a call fits one of the overloads.
 */
@ExtendWith(HeadlessProjectExtension.class)
// the highlighting passes of the XML plugin and the RegExp language of the JavaScript plugin aren't on the classpath, nor
// Jaxen for the language injection configuration (loaded on a roots change)
@AllowLogError({"consulo.component.internal.NewExtensionPointImpl", "consulo.language.inject.advanced.Configuration"})
public class PredefinedLibraryInspectionTest {
    // headless, the problem descriptions are the localize keys
    private static final String NOT_FUNCTION = "term.does.not.evaluate.to.function";
    private static final String PARAMETERS = "invalid.number.of.parameters";
    private static final String ARGUMENT_TYPE = "argument.type.mismatch";

    @BeforeEach
    public void enableWeb(CodeInsightTestFixture fixture) throws Exception {
        WebModules.enableWeb(fixture.getModule());
        fixture.enableInspections(JSUnresolvedFunctionInspection.class);
    }

    @Test
    public void testLibraryCalls(CodeInsightTestFixture fixture) throws Exception {
        String text = "var d = new Date();\n"
            + "var http = new XMLHttpRequest();\n"
            + "http.open('get', '/a', true);\n"
            + "http.open('get', '/a');\n"
            + "setInterval(function () {}, 1);\n";
        fixture.configureByText("calls.js", text);
        assertEquals(List.of(), problems(fixture, NOT_FUNCTION));
        assertEquals(List.of(), problems(fixture, PARAMETERS));
        assertEquals(List.of(), problems(fixture, ARGUMENT_TYPE));
    }

    /**
     * The argument types against TypeScript parameter types: the primitives are the JavaScript classes (a string literal
     * is a {@code string}), unions and other types the check can't evaluate are skipped (issue #22: XMLHttpRequest.open).
     */
    @Test
    public void testTypeScriptParameterTypes(CodeInsightTestFixture fixture) throws Exception {
        String declarations = fixture.addFileToProject("decl.d.ts",
            "declare function takesString(a: string): void;\n"
                + "declare function takesFlag(a: boolean): void;\n"
                + "declare function takesUnion(a: string | URL): void;\n").getVirtualFile().getUrl();
        WebModules.setDeclarations(fixture.getModule(), "Test", List.of(declarations));
        try {
            fixture.configureByText("args.js", "takesString('a');\ntakesFlag(true);\ntakesUnion(1);\n");
            assertEquals(List.of(), problems(fixture, ARGUMENT_TYPE));
        }
        finally {
            WebModules.setDeclarations(fixture.getModule(), "Test", List.of());
        }
    }

    /**
     * The check still runs: no overload of setInterval takes no arguments.
     */
    @Test
    public void testWrongArgumentCount(CodeInsightTestFixture fixture) throws Exception {
        fixture.configureByText("wrong.js", "setInterval();\n");
        assertEquals(1, problems(fixture, PARAMETERS).size());
    }

    /**
     * The highlighted texts of the problems.
     */
    private static List<String> problems(CodeInsightTestFixture fixture, String key) throws Exception {
        List<HighlightInfo> infos = fixture.doHighlighting();
        String text = ReadAction.compute(() -> fixture.getFile().getText());
        return infos.stream()
            .filter(info -> info.getDescription().get().contains(key))
            .map(info -> text.substring(info.getStartOffset(), info.getEndOffset()))
            .toList();
    }
}
