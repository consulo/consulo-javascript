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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import consulo.annotation.access.RequiredReadAction;
import consulo.application.progress.ProgressManager;
import consulo.javascript.psi.JavaScriptImportStatementBase;
import consulo.language.controlFlow.Instruction;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.util.PsiTreeUtil;

import java.util.*;

/**
 * The values assigned to a variable of the owner and never read after - live variables computed backward over the
 * graph. Not looked at: a variable never read at all (an unused variable, not an unused value), one a nested function
 * or class mentions by its name (when it runs is not known; a write there is not seen, it can only keep a value used),
 * the value of a parameter at the entry; the variables of a script (globals) and the exported ones of a module.
 *
 * @author VISTALL
 */
public final class JavaScriptUnusedValues {
    /**
     * An assignment whose value is never read - at the declaration (an initializer) or at the reference
     */
    public record Write(PsiElement element, PsiElement variable) {
    }

    private JavaScriptUnusedValues() {
    }

    @RequiredReadAction
    static List<Write> compute(JavaScriptControlFlow flow) {
        PsiElement owner = flow.getOwner();
        Instruction[] instructions = flow.getInstructions();
        // a file, a script block of an HTML file
        boolean script = JavaScriptControlFlow.getBody(owner) == owner;
        if (script && !isModule(owner)) {
            return List.of();
        }

        Set<PsiElement> read = new HashSet<>();
        for (Instruction instruction : instructions) {
            if (instruction instanceof JavaScriptReadWriteInstruction readWrite && readWrite.getAccess() == JavaScriptReadWriteInstruction.Access.READ) {
                read.add(readWrite.getVariable());
            }
        }
        if (read.isEmpty()) {
            return List.of();
        }
        Set<String> nestedNames = nestedNames(owner);
        Set<String> exported = script ? exportedNames(owner) : Set.of();

        Map<PsiElement, Integer> indexes = new HashMap<>();
        for (PsiElement variable : read) {
            String name = JavaScriptFlowVariables.name(variable);
            if (!nestedNames.contains(name) && !exported.contains(name)) {
                indexes.put(variable, indexes.size());
            }
        }
        if (indexes.isEmpty()) {
            return List.of();
        }

        BitSet[] liveOut = liveOut(flow, instructions, indexes);
        if (liveOut == null) {
            // too complex
            return List.of();
        }

        // a write is unused when none of its copies (a finally block built more than once) is read after
        Map<Write, Boolean> writes = new LinkedHashMap<>();
        for (Instruction instruction : instructions) {
            if (!(instruction instanceof JavaScriptReadWriteInstruction readWrite) || readWrite.getAccess() != JavaScriptReadWriteInstruction.Access.WRITE
                || !flow.isReachable(instruction)) {
                continue;
            }
            Integer index = indexes.get(readWrite.getVariable());
            // the value of a parameter, a catch or a for-in/of variable - not assigned by the code
            if (index == null || readWrite.getElement() == readWrite.getVariable() && !hasValue(readWrite.getVariable())) {
                continue;
            }
            boolean unused = !liveOut[instruction.num()].get(index);
            writes.merge(new Write(readWrite.getElement(), readWrite.getVariable()), unused, Boolean::logicalAnd);
        }

        List<Write> result = new ArrayList<>();
        writes.forEach((write, unused) -> {
            if (unused) {
                result.add(write);
            }
        });
        return result;
    }

    /**
     * Declared with an initializer - not a parameter (its default is not the value it has), a catch, a for-in/of variable
     */
    @RequiredReadAction
    private static boolean hasValue(PsiElement target) {
        return JavaScriptFlowVariables.kind(target) != JavaScriptFlowVariables.Kind.PARAMETER
            && PsiTreeUtil.getParentOfType(target, JSVariable.class, false) instanceof JSVariable variable
            && variable.getInitializer() != null;
    }

    /**
     * live out = the union of the live ins of the successors, live in = live out without the written variable, with the
     * read one - till nothing changes; null if it does not settle
     */
    private static BitSet[] liveOut(JavaScriptControlFlow flow, Instruction[] instructions, Map<PsiElement, Integer> indexes) {
        int length = instructions.length;
        BitSet[] liveIn = new BitSet[length];
        BitSet[] liveOut = new BitSet[length];
        for (int i = 0; i < length; i++) {
            liveIn[i] = new BitSet();
            liveOut[i] = new BitSet();
        }

        Deque<Instruction> queue = new ArrayDeque<>();
        boolean[] queued = new boolean[length];
        for (int i = length - 1; i >= 0; i--) {
            if (flow.isReachable(instructions[i])) {
                queue.add(instructions[i]);
                queued[i] = true;
            }
        }

        int limit = 20 * (length + 1) * Math.max(1, indexes.size());
        int count = 0;
        while (!queue.isEmpty()) {
            if (++count > limit) {
                return null;
            }
            if ((count & 0xFF) == 0) {
                ProgressManager.checkCanceled();
            }
            Instruction instruction = queue.poll();
            int num = instruction.num();
            queued[num] = false;

            BitSet out = new BitSet();
            for (Instruction successor : instruction.allSucc()) {
                out.or(liveIn[successor.num()]);
            }
            liveOut[num] = out;

            BitSet in = (BitSet) out.clone();
            if (instruction instanceof JavaScriptReadWriteInstruction readWrite) {
                Integer index = indexes.get(readWrite.getVariable());
                if (index != null) {
                    in.set(index, readWrite.getAccess() == JavaScriptReadWriteInstruction.Access.READ);
                }
            }
            if (!in.equals(liveIn[num])) {
                liveIn[num] = in;
                for (Instruction predecessor : instruction.allPred()) {
                    if (!queued[predecessor.num()] && flow.isReachable(predecessor)) {
                        queued[predecessor.num()] = true;
                        queue.add(predecessor);
                    }
                }
            }
        }
        return liveOut;
    }

    /**
     * The names the nested functions and classes of the owner refer to
     */
    @RequiredReadAction
    private static Set<String> nestedNames(PsiElement owner) {
        Set<String> names = new HashSet<>();
        Deque<Map.Entry<PsiElement, Boolean>> stack = new ArrayDeque<>();
        stack.push(Map.entry(owner, false));
        while (!stack.isEmpty()) {
            Map.Entry<PsiElement, Boolean> entry = stack.pop();
            for (PsiElement child = entry.getKey().getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child.getFirstChild() == null) {
                    continue;
                }
                boolean nested = entry.getValue() || JavaScriptControlFlow.isOwner(child) || child instanceof JSClass;
                if (nested && child instanceof JSReferenceExpression reference && reference.getQualifier() == null && reference.getReferencedName() != null) {
                    names.add(reference.getReferencedName());
                }
                stack.push(Map.entry(child, nested));
            }
        }
        return names;
    }

    /**
     * An ES module - with an import or an export: its variables are not globals
     */
    private static boolean isModule(PsiElement script) {
        for (PsiElement child = script.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JavaScriptImportStatementBase || isExport(child)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isExport(PsiElement statement) {
        return PsiUtilCore.getElementType(PsiTreeUtil.getDeepestFirst(statement)) == JSTokenTypes.EXPORT_KEYWORD
            || statement.getFirstChild() != null && PsiUtilCore.getElementType(PsiTreeUtil.skipWhitespacesAndCommentsForward(statement.getFirstChild())) == JSTokenTypes.EXPORT_KEYWORD;
    }

    /**
     * The identifiers in the export statements of a module - declared or listed there
     */
    private static Set<String> exportedNames(PsiElement script) {
        Set<String> names = new HashSet<>();
        for (PsiElement child = script.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (isExport(child)) {
                for (PsiElement leaf = PsiTreeUtil.firstChild(child); leaf != null && PsiTreeUtil.isAncestor(child, leaf, false); leaf = PsiTreeUtil.nextLeaf(leaf)) {
                    if (PsiUtilCore.getElementType(leaf) == JSTokenTypes.IDENTIFIER) {
                        names.add(leaf.getText());
                    }
                }
            }
        }
        return names;
    }
}
