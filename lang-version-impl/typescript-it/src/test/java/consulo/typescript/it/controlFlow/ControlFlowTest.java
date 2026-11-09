/*
 * Copyright 2013-2026 consulo.io
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package consulo.typescript.it.controlFlow;

import consulo.annotation.access.RequiredReadAction;
import consulo.application.ReadAction;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.javascript.impl.language.controlFlow.JavaScriptControlFlow;
import consulo.javascript.impl.language.controlFlow.JavaScriptInstruction;
import consulo.language.controlFlow.Instruction;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiErrorElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * The graphs of the flow owners of every {@code flow/<name>.js} and {@code flow/<name>.ts}, in the order of the file,
 * against {@code flow/<name>.txt}: a header per owner, then an instruction per line as {@code num(successors) element},
 * unreachable ones marked with {@code !}. A missing {@code .txt} is written to {@code src/test/resources/flow} and the
 * test fails.
 *
 * @author VISTALL
 */
@ExtendWith(HeadlessProjectExtension.class)
public class ControlFlowTest {
    private static final String DIRECTORY = "flow";

    static Stream<String> sources() throws Exception {
        try (Stream<Path> list = Files.list(resource(DIRECTORY))) {
            return list.map(path -> path.getFileName().toString()).filter(name -> name.endsWith(".js") || name.endsWith(".ts")).sorted().toList().stream();
        }
    }

    @ParameterizedTest(name = "{0}")
    @MethodSource("sources")
    public void check(String fileName, CodeInsightTestFixture fixture) throws Exception {
        String source = Files.readString(resource(DIRECTORY + "/" + fileName), StandardCharsets.UTF_8);
        fixture.configureByText(fileName, source);
        PsiFile file = fixture.getFile();

        String actual = ReadAction.compute(() -> {
            assertEquals(List.of(), List.copyOf(PsiTreeUtil.findChildrenOfType(file, PsiErrorElement.class)), "parse errors");
            return dump(file);
        });

        String expectedName = fileName.substring(0, fileName.lastIndexOf('.')) + ".txt";
        URL expectedUrl = ControlFlowTest.class.getClassLoader().getResource(DIRECTORY + "/" + expectedName);
        if (expectedUrl == null) {
            Path path = Path.of("src/test/resources", DIRECTORY, expectedName);
            Files.writeString(path, actual, StandardCharsets.UTF_8);
            throw new AssertionError("written " + path.toAbsolutePath());
        }
        String expected = Files.readString(Path.of(expectedUrl.toURI()), StandardCharsets.UTF_8).replace("\r\n", "\n");
        assertEquals(expected, actual);
    }

    @RequiredReadAction
    private static String dump(PsiFile file) {
        StringBuilder builder = new StringBuilder();
        for (PsiElement owner : owners(file)) {
            JavaScriptControlFlow flow = JavaScriptControlFlow.of(owner);
            if (flow == null) {
                continue;
            }
            builder.append("# ").append(header(owner)).append('\n');
            for (Instruction instruction : flow.getInstructions()) {
                builder.append(flow.isReachable(instruction) ? "  " : "! ").append(instruction).append('\n');
            }
            builder.append('\n');
        }
        return builder.toString();
    }

    @RequiredReadAction
    static List<PsiElement> owners(PsiFile file) {
        List<PsiElement> owners = new ArrayList<>();
        owners.add(file);
        for (PsiElement element : PsiTreeUtil.findChildrenOfType(file, PsiElement.class)) {
            if (JavaScriptControlFlow.isOwner(element)) {
                owners.add(element);
            }
        }
        return owners;
    }

    @RequiredReadAction
    private static String header(PsiElement owner) {
        if (owner instanceof PsiFile file) {
            return file.getName();
        }
        String text = JavaScriptInstruction.presentation(owner);
        int brace = text.indexOf('{');
        return brace > 0 ? text.substring(0, brace).trim() : text;
    }

    private static Path resource(String path) throws Exception {
        URL url = ControlFlowTest.class.getClassLoader().getResource(path);
        if (url == null) {
            throw new AssertionError(path);
        }
        return Path.of(url.toURI());
    }
}
