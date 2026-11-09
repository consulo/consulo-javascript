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
import com.intellij.lang.javascript.psi.JSArrayLiteralExpression;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSForInStatement;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.lang.javascript.psi.JSLoopStatement;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSParenthesizedExpression;
import com.intellij.lang.javascript.psi.JSPostfixExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSSpreadExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import consulo.annotation.access.RequiredReadAction;
import consulo.application.progress.ProgressManager;
import consulo.language.ast.IElementType;
import consulo.language.controlFlow.Instruction;
import consulo.language.dataFlow.DFAEngine;
import consulo.language.dataFlow.DFALimitExceededException;
import consulo.language.dataFlow.DfaInstance;
import consulo.language.dataFlow.Semilattice;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import org.jspecify.annotations.Nullable;

import java.util.*;

/**
 * Definite assignment on a {@link JavaScriptControlFlow}: the set of the variables of the owner assigned on every path
 * to a node, joined by intersection; everything is assigned at an unreachable node. A read of a variable not assigned
 * there is unassigned - tsc reports it (TS2454) when the type of the variable does not accept undefined.
 * <p>
 * As tsc ({@code checkIdentifier}), a read in an arrow function or a function expression (an object literal method)
 * created here is checked where it is created when the variable is a const, or when the read is after the last
 * assignment of the variable in the text: nothing can assign it later; other reads in nested functions are not
 * checked - when they run is not known. An assignment in a nested function does not assign the variable here.
 *
 * @author VISTALL
 */
public final class JavaScriptDefiniteAssignment {
    private static final JavaScriptDefiniteAssignment EMPTY = new JavaScriptDefiniteAssignment(Map.of());

    // the references read unassigned, with their variables
    private final Map<PsiElement, PsiElement> myUnassignedReads;

    private JavaScriptDefiniteAssignment(Map<PsiElement, PsiElement> unassignedReads) {
        myUnassignedReads = unassignedReads;
    }

    /**
     * The references of variables of the owner read where they may be unassigned - also in a nested function - with
     * their variables
     */
    public Map<PsiElement, PsiElement> getUnassignedReads() {
        return myUnassignedReads;
    }

    /**
     * Assigned variables by their index - null is all of them (an unreachable node)
     */
    private record State(@Nullable BitSet assigned) {
        boolean has(int index) {
            return assigned == null || assigned.get(index);
        }
    }

    private static final State ALL = new State(null);

    @RequiredReadAction
    static JavaScriptDefiniteAssignment compute(JavaScriptControlFlow flow) {
        Instruction[] instructions = flow.getInstructions();
        Map<PsiElement, Integer> indexes = new HashMap<>();
        boolean nested = false;
        for (Instruction instruction : instructions) {
            if (instruction instanceof JavaScriptReadWriteInstruction readWrite) {
                indexes.putIfAbsent(readWrite.getVariable(), indexes.size());
            }
            else if (isNestedFunction(instruction.getElement())) {
                nested = true;
            }
        }
        if (indexes.isEmpty()) {
            return EMPTY;
        }

        List<State> states;
        try {
            states = new DFAEngine<>(instructions, new Transfer(flow, indexes), new Intersection()).performDFA();
        }
        catch (DFALimitExceededException e) {
            // too complex - nothing reported
            return EMPTY;
        }

        Map<PsiElement, PsiElement> unassigned = new LinkedHashMap<>();
        for (Instruction instruction : instructions) {
            if (instruction instanceof JavaScriptReadWriteInstruction readWrite && readWrite.getAccess() == JavaScriptReadWriteInstruction.Access.READ
                && flow.isReachable(instruction) && !states.get(instruction.num()).has(indexes.get(readWrite.getVariable()))) {
                unassigned.put(readWrite.getElement(), readWrite.getVariable());
            }
        }

        if (nested) {
            closureReads(flow, instructions, states, indexes, unassigned);
        }
        return unassigned.isEmpty() ? EMPTY : new JavaScriptDefiniteAssignment(unassigned);
    }

    /**
     * The reads in the arrow functions and function expressions created here - checked at the creation when nothing
     * can assign the variable later
     */
    @RequiredReadAction
    private static void closureReads(JavaScriptControlFlow flow, Instruction[] instructions, List<State> states, Map<PsiElement, Integer> indexes,
                                     Map<PsiElement, PsiElement> unassigned) {
        PsiElement owner = flow.getOwner();
        Map<String, List<PsiElement>> variables = JavaScriptFlowVariables.byName(owner);

        // the position of the last assignment of each variable - here and in the nested owners (tsc's lastAssignmentPos)
        Map<PsiElement, Integer> lastAssignments = new HashMap<>();
        for (Instruction instruction : instructions) {
            if (instruction instanceof JavaScriptReadWriteInstruction readWrite && readWrite.getAccess() == JavaScriptReadWriteInstruction.Access.WRITE) {
                PsiElement variable = readWrite.getVariable();
                // a declaration with a value - at its end; not a parameter, a for-in/of variable
                int position = readWrite.getElement() != variable ? assignmentPosition(readWrite.getElement(), variable)
                    : hasInitializer(variable) ? variable.getTextRange().getEndOffset() : -1;
                if (position >= 0) {
                    lastAssignments.merge(variable, position, Math::max);
                }
            }
        }
        Map<PsiElement, List<Map.Entry<PsiElement, PsiElement>>> reads = nestedReferences(owner, variables, lastAssignments);

        for (Instruction instruction : instructions) {
            List<Map.Entry<PsiElement, PsiElement>> functionReads = reads.get(instruction.getElement());
            if (functionReads == null || !flow.isReachable(instruction)) {
                continue;
            }
            State state = states.get(instruction.num());
            for (Map.Entry<PsiElement, PsiElement> read : functionReads) {
                PsiElement variable = read.getValue();
                Integer index = indexes.get(variable);
                if (index == null || state.has(index)) {
                    continue;
                }
                Integer lastAssignment = lastAssignments.get(variable);
                if (JavaScriptFlowVariables.kind(variable) == JavaScriptFlowVariables.Kind.CONST || lastAssignment == null
                    || read.getKey().getTextRange().getStartOffset() > lastAssignment) {
                    unassigned.put(read.getKey(), variable);
                }
            }
        }
    }

    /**
     * The references to the variables of the owner in its nested owners: the assignments of all of them (into the last
     * assignments), the reads of each function expression created in the owner - through the function expressions in
     * it, not through a function declaration or a class
     */
    @RequiredReadAction
    private static Map<PsiElement, List<Map.Entry<PsiElement, PsiElement>>> nestedReferences(PsiElement owner, Map<String, List<PsiElement>> variables,
                                                                                         Map<PsiElement, Integer> lastAssignments) {
        Map<PsiElement, List<Map.Entry<PsiElement, PsiElement>>> reads = new HashMap<>();
        // element, the function expression whose reads are collected (none), in a nested owner
        record Scope(PsiElement element, @Nullable PsiElement function, boolean nested) {
        }
        Deque<Scope> stack = new ArrayDeque<>();
        stack.push(new Scope(owner, null, false));
        while (!stack.isEmpty()) {
            Scope scope = stack.pop();
            for (PsiElement child = scope.element().getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child.getFirstChild() == null) {
                    continue;
                }
                ProgressManager.checkCanceled();
                boolean nestedOwner = JavaScriptControlFlow.isOwner(child) || child instanceof JSClass;
                PsiElement function = scope.function();
                if (!scope.nested() && isNestedFunction(child)) {
                    function = child;
                }
                else if (nestedOwner && !isNestedFunction(child)) {
                    function = null;
                }
                boolean nested = scope.nested() || nestedOwner;

                // the references of the owner itself are in its graph
                if (nested && child instanceof JSReferenceExpression reference && reference.getQualifier() == null) {
                    String name = reference.getReferencedName();
                    PsiElement target = name != null && variables.containsKey(name) ? reference.resolve() : null;
                    if (JavaScriptFlowVariables.isVariableOf(target, owner)) {
                        if (isAssigned(reference)) {
                            // when the nested function runs is not known
                            lastAssignments.put(target, Integer.MAX_VALUE);
                        }
                        else if (function != null) {
                            reads.computeIfAbsent(function, it -> new ArrayList<>()).add(Map.entry(reference, target));
                        }
                    }
                }
                stack.push(new Scope(child, function, nested));
            }
        }
        return reads;
    }

    /**
     * Where an assignment counts: at the end of the outermost loop it is in which does not have the declaration in it -
     * it runs again (tsc's extendAssignmentPosition)
     */
    private static int assignmentPosition(PsiElement assignment, PsiElement variable) {
        int position = assignment.getTextRange().getStartOffset();
        for (PsiElement parent = assignment.getParent(); parent != null && !PsiTreeUtil.isAncestor(parent, variable, false); parent = parent.getParent()) {
            if (parent instanceof JSLoopStatement) {
                position = parent.getTextRange().getEndOffset();
            }
        }
        return position;
    }

    @RequiredReadAction
    private static boolean hasInitializer(PsiElement target) {
        JSVariable variable = PsiTreeUtil.getParentOfType(target, JSVariable.class, false);
        return variable != null && JavaScriptFlowVariables.kind(target) != JavaScriptFlowVariables.Kind.PARAMETER && variable.getInitializer() != null;
    }

    /**
     * The target of an assignment, ++, --, a for-in/of, a destructuring
     */
    private static boolean isAssigned(PsiElement reference) {
        PsiElement element = reference;
        PsiElement parent = element.getParent();
        while (parent instanceof JSParenthesizedExpression || parent instanceof JSDefinitionExpression || parent instanceof JSSpreadExpression
            || parent instanceof JSArrayLiteralExpression || parent instanceof JSProperty || parent instanceof JSObjectLiteralExpression) {
            element = parent;
            parent = parent.getParent();
        }
        if (parent instanceof JSPostfixExpression || parent instanceof JSPrefixExpression prefix && prefix.getExpression() == element
            && JavaScriptControlFlowBuilder.isIncrement(prefix.getOperationSign())) {
            return true;
        }
        if (parent instanceof JSForInStatement forIn) {
            return forIn.getVariableExpression() == element;
        }
        IElementType operator = JavaScriptControlFlowBuilder.operator(parent);
        return operator != null && JSTokenTypes.ASSIGNMENT_OPERATIONS.contains(operator) && parent.getFirstChild() == element;
    }

    /**
     * An arrow function, a function expression, an object literal method - created at its node
     */
    private static boolean isNestedFunction(@Nullable PsiElement element) {
        return element instanceof JSFunctionExpression || JavaScriptControlFlow.isPropertyMethod(element);
    }

    private record Transfer(JavaScriptControlFlow flow, Map<PsiElement, Integer> indexes) implements DfaInstance<State> {
        @Override
        public State fun(State state, Instruction instruction) {
            if (!flow.isReachable(instruction)) {
                return ALL;
            }
            if (!(instruction instanceof JavaScriptReadWriteInstruction readWrite) || readWrite.getAccess() == JavaScriptReadWriteInstruction.Access.READ) {
                return state;
            }
            int index = indexes.get(readWrite.getVariable());
            boolean assign = readWrite.getAccess() == JavaScriptReadWriteInstruction.Access.WRITE;
            if (state.has(index) == assign) {
                return state;
            }
            BitSet assigned = state.assigned() == null ? all(indexes.size()) : (BitSet) state.assigned().clone();
            assigned.set(index, assign);
            return new State(assigned);
        }

        @Override
        public State initial() {
            return ALL;
        }

        @Override
        public boolean isForward() {
            return true;
        }

        private static BitSet all(int size) {
            BitSet bitSet = new BitSet(size);
            bitSet.set(0, size);
            return bitSet;
        }
    }

    /**
     * Assigned on every path - the entry, with no predecessor, has nothing assigned
     */
    private static final class Intersection implements Semilattice<State> {
        @Override
        public State join(ArrayList<State> ins) {
            if (ins.isEmpty()) {
                return new State(new BitSet());
            }
            BitSet result = null;
            for (State state : ins) {
                if (state.assigned() == null) {
                    continue;
                }
                if (result == null) {
                    result = (BitSet) state.assigned().clone();
                }
                else {
                    result.and(state.assigned());
                }
            }
            return result == null ? ALL : new State(result);
        }

        @Override
        public boolean eq(State first, State second) {
            return Objects.equals(first.assigned(), second.assigned());
        }
    }
}
