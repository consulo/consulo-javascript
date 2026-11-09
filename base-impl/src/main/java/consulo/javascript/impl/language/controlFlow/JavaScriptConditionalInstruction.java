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

package consulo.javascript.impl.language.controlFlow;

import consulo.language.controlFlow.ControlFlowBuilder;
import consulo.language.controlFlow.base.ConditionalInstructionImpl;
import consulo.language.psi.PsiElement;

/**
 * The start of a branch taken when the condition is true or false - its element is the branch (the then statement,
 * a loop body, the right operand of {@code &&}) or, for the false exit of an {@code if} without an else, the statement
 *
 * @author VISTALL
 */
public class JavaScriptConditionalInstruction extends ConditionalInstructionImpl {
    public JavaScriptConditionalInstruction(ControlFlowBuilder builder, PsiElement element, PsiElement condition, boolean result) {
        super(builder, element, condition, result);
    }

    @Override
    public String getElementPresentation() {
        return JavaScriptInstruction.presentation(getElement());
    }
}
