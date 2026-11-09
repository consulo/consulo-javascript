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

import consulo.language.ast.ASTNode;
import consulo.language.controlFlow.ControlFlowBuilder;
import consulo.language.controlFlow.base.InstructionImpl;
import consulo.language.psi.PsiElement;
import org.jspecify.annotations.Nullable;

/**
 * A node of a JavaScript control flow graph - a statement, the creation of a nested function or class, or a marker
 * (entry, end of the body, exit, a join of branches, the next element of a for-in/of loop) named in the dump
 *
 * @author VISTALL
 */
public class JavaScriptInstruction extends InstructionImpl {
    public static final String ENTRY = "entry";
    public static final String END = "end";
    public static final String EXIT = "exit";
    public static final String JOIN = "join";
    // the head of a for (;;) loop without a condition
    public static final String LOOP = "loop";
    // the next element of a for-in/of loop, or its end
    public static final String NEXT = "next";
    // a copy of a finally block - one for each way out of the try
    public static final String FINALLY = "finally";

    private final @Nullable String myMarker;

    public JavaScriptInstruction(ControlFlowBuilder builder, @Nullable PsiElement element, @Nullable String marker) {
        super(builder, element);
        myMarker = marker;
    }

    public boolean is(String marker) {
        return marker.equals(myMarker);
    }

    @Override
    public String getElementPresentation() {
        PsiElement element = getElement();
        if (myMarker == null) {
            return presentation(element);
        }
        // the end of the owner - its element
        return element == null || END.equals(myMarker) ? myMarker : myMarker + " " + presentation(element);
    }

    /**
     * {@code IF_STATEMENT if (b) return 1;} - the element type and the text on one line, cut after 50 characters
     */
    public static String presentation(@Nullable PsiElement element) {
        if (element == null) {
            return "null";
        }
        ASTNode node = element.getNode();
        String text = element.getText().replaceAll("\\s+", " ").trim();
        if (text.length() > 50) {
            text = text.substring(0, 50) + "...";
        }
        return (node == null ? element.getClass().getSimpleName() : node.getElementType().toString()) + " " + text;
    }
}
