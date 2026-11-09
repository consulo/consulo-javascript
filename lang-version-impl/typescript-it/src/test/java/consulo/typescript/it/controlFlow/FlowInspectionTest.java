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

import com.sixrr.inspectjs.validity.FunctionWithInconsistentReturnsJSInspection;
import com.sixrr.inspectjs.validity.TypeScriptFunctionReturnInspection;
import com.sixrr.inspectjs.validity.TypeScriptUsedBeforeAssignedInspection;
import com.sixrr.inspectjs.validity.UnreachableCodeJSInspection;
import com.sixrr.inspectjs.validity.UsedBeforeDeclarationJSInspection;
import consulo.it.AllowLogError;
import consulo.it.CodeInsightTestFixture;
import consulo.it.HeadlessProjectExtension;
import consulo.language.editor.inspection.LocalInspectionTool;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;

/**
 * The inspections on the control flow graph against the expected highlights marked in {@code inspection/<name>}, each
 * checked with tsc 6.0 ({@code allowUnreachableCode: false, strict: true}) where it has a code.
 *
 * @author VISTALL
 */
@ExtendWith(HeadlessProjectExtension.class)
// the highlighting passes of the XML plugin aren't on the classpath, nor Jaxen for the language injection configuration
@AllowLogError({"consulo.component.internal.NewExtensionPointImpl", "consulo.language.inject.advanced.Configuration"})
public class FlowInspectionTest {
    @Test
    public void testUnreachableCode(CodeInsightTestFixture fixture) throws Exception {
        check(fixture, "UnreachableCode.ts", UnreachableCodeJSInspection.class);
    }

    @Test
    public void testUnreachableCodeJavaScript(CodeInsightTestFixture fixture) throws Exception {
        check(fixture, "UnreachableCode.js", UnreachableCodeJSInspection.class);
    }

    @Test
    public void testFunctionReturn(CodeInsightTestFixture fixture) throws Exception {
        check(fixture, "FunctionReturn.ts", TypeScriptFunctionReturnInspection.class);
    }

    @Test
    public void testFunctionWithInconsistentReturns(CodeInsightTestFixture fixture) throws Exception {
        check(fixture, "FunctionWithInconsistentReturns.js", FunctionWithInconsistentReturnsJSInspection.class);
    }

    @Test
    public void testUsedBeforeDeclaration(CodeInsightTestFixture fixture) throws Exception {
        check(fixture, "UsedBeforeDeclaration.ts", UsedBeforeDeclarationJSInspection.class);
    }

    @Test
    public void testUsedBeforeDeclarationJavaScript(CodeInsightTestFixture fixture) throws Exception {
        check(fixture, "UsedBeforeDeclaration.js", UsedBeforeDeclarationJSInspection.class);
    }

    @Test
    public void testUsedBeforeAssigned(CodeInsightTestFixture fixture) throws Exception {
        check(fixture, "UsedBeforeAssigned.ts", TypeScriptUsedBeforeAssignedInspection.class);
    }

    @Test
    public void testUsedBeforeAssignedFlow(CodeInsightTestFixture fixture) throws Exception {
        check(fixture, "UsedBeforeAssignedFlow.ts", TypeScriptUsedBeforeAssignedInspection.class);
    }

    @SafeVarargs
    static void check(CodeInsightTestFixture fixture, String fileName, Class<? extends LocalInspectionTool>... inspections) throws Exception {
        String text;
        try (InputStream stream = FlowInspectionTest.class.getClassLoader().getResourceAsStream("inspection/" + fileName)) {
            if (stream == null) {
                throw new AssertionError(fileName);
            }
            text = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
        }
        fixture.enableInspections(inspections);
        fixture.configureByText(fileName, text);
        long start = System.nanoTime();
        fixture.checkHighlighting();
        System.out.println("highlighting " + fileName + ": " + (System.nanoTime() - start) / 1_000_000 + " ms");
    }
}
