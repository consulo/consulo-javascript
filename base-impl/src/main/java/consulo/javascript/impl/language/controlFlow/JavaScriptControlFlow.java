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
import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.application.util.CachedValueProvider;
import consulo.javascript.psi.JSClassStaticBlock;
import consulo.javascript.psi.JavaScriptLambdaExpression;
import consulo.language.controlFlow.ControlFlow;
import consulo.language.controlFlow.Instruction;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiErrorElement;
import consulo.language.psi.PsiModificationTracker;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.util.LanguageCachedValueUtil;
import consulo.language.psi.util.PsiTreeUtil;
import org.jspecify.annotations.Nullable;

import java.util.*;

/**
 * The control flow graph of a flow owner - a function, a method, an accessor, an arrow function, a class static block,
 * a script (a file, a script block of an HTML file) - with the reachability of its nodes. Nested owners are opaque: a
 * function expression or a class is one node, a function declaration (hoisted) none.
 *
 * @author VISTALL
 */
public final class JavaScriptControlFlow implements ControlFlow {
    private final PsiElement myOwner;
    private final Instruction[] myInstructions;
    private final Instruction myEnd;
    private final Map<PsiElement, List<Instruction>> myIndex = new HashMap<>();
    private final BitSet myReachable = new BitSet();

    JavaScriptControlFlow(PsiElement owner, Instruction[] instructions, Instruction end) {
        myOwner = owner;
        myInstructions = instructions;
        myEnd = end;

        for (Instruction instruction : instructions) {
            PsiElement element = instruction.getElement();
            if (element != null) {
                myIndex.computeIfAbsent(element, it -> new ArrayList<>(1)).add(instruction);
            }
        }

        Deque<Instruction> queue = new ArrayDeque<>();
        if (instructions.length > 0) {
            myReachable.set(0);
            queue.add(instructions[0]);
        }
        while (!queue.isEmpty()) {
            for (Instruction next : queue.poll().allSucc()) {
                if (!myReachable.get(next.num())) {
                    myReachable.set(next.num());
                    queue.add(next);
                }
            }
        }
    }

    /**
     * The graph of the owner - null if it is none, has no body or does not parse
     */
    @RequiredReadAction
    public static @Nullable JavaScriptControlFlow of(PsiElement owner) {
        if (!isOwner(owner)) {
            return null;
        }
        // a completion copy, a code fragment - not cached
        if (!owner.isPhysical()) {
            return compute(owner);
        }
        return LanguageCachedValueUtil.getCachedValue(owner, () -> CachedValueProvider.Result.create(compute(owner), PsiModificationTracker.MODIFICATION_COUNT));
    }

    /**
     * The graph of the innermost owner the element is evaluated in - null if there is none
     */
    @RequiredReadAction
    public static @Nullable JavaScriptControlFlow ofElement(PsiElement element) {
        PsiElement owner = ownerOf(element);
        return owner == null ? null : of(owner);
    }

    /**
     * The innermost owner the element is evaluated in - not the element itself: a function is created in the enclosing one
     */
    @RequiredReadAction
    public static @Nullable PsiElement ownerOf(PsiElement element) {
        for (PsiElement parent = element.getParent(); parent != null; parent = parent.getParent()) {
            if (isOwner(parent)) {
                return parent;
            }
        }
        return null;
    }

    /**
     * A function with a body, a method of an object literal, a class static block, a script
     */
    @RequiredReadAction
    public static boolean isOwner(@Nullable PsiElement element) {
        if (element instanceof JSFile || element instanceof JSEmbeddedContentImpl || element instanceof JSClassStaticBlock) {
            return true;
        }
        return (element instanceof JSFunction || isPropertyMethod(element)) && getBody(element) != null;
    }

    /**
     * {@code { m() { } }} of a TypeScript object literal - a property, not a function
     */
    @RequiredReadAction
    static boolean isPropertyMethod(@Nullable PsiElement element) {
        return element instanceof JSProperty && !(element instanceof JSFunction)
            && PsiTreeUtil.getChildOfType(element, JSParameterList.class) != null
            && PsiTreeUtil.getChildOfType(element, JSBlockStatement.class) != null;
    }

    /**
     * The statements of the owner: a block, the expression of an arrow function, the owner itself for a script
     */
    @RequiredReadAction
    public static @Nullable PsiElement getBody(PsiElement owner) {
        if (owner instanceof JSFile || owner instanceof JSEmbeddedContentImpl) {
            return owner;
        }
        JSBlockStatement block = PsiTreeUtil.getChildOfType(owner, JSBlockStatement.class);
        if (block != null || !(owner instanceof JavaScriptLambdaExpression)) {
            return block;
        }
        // x => x + 1 - the expression after =>
        boolean arrow = false;
        for (PsiElement child = owner.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (PsiUtilCore.getElementType(child) == JSTokenTypes.DARROW) {
                arrow = true;
            }
            else if (arrow && child instanceof JSExpression) {
                return child;
            }
        }
        return null;
    }

    @RequiredReadAction
    private static @Nullable JavaScriptControlFlow compute(PsiElement owner) {
        PsiElement body = getBody(owner);
        if (body == null || hasErrors(owner)) {
            return null;
        }
        return JavaScriptControlFlowBuilder.build(owner, body);
    }

    /**
     * An error element in the owner - not in a nested one, it has its own graph
     */
    private static boolean hasErrors(PsiElement owner) {
        Deque<PsiElement> stack = new ArrayDeque<>();
        stack.push(owner);
        while (!stack.isEmpty()) {
            for (PsiElement child = stack.pop().getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child instanceof PsiErrorElement) {
                    return true;
                }
                if (child.getFirstChild() != null && !isOwner(child)) {
                    stack.push(child);
                }
            }
        }
        return false;
    }

    public PsiElement getOwner() {
        return myOwner;
    }

    @Override
    public Instruction[] getInstructions() {
        return myInstructions;
    }

    public List<Instruction> getInstructions(PsiElement element) {
        return myIndex.getOrDefault(element, List.of());
    }

    public boolean isReachable(Instruction instruction) {
        return myReachable.get(instruction.num());
    }

    /**
     * Any node of the element is reachable - true for an element without one (a block, an expression)
     */
    public boolean isReachable(PsiElement element) {
        List<Instruction> instructions = myIndex.get(element);
        if (instructions == null) {
            return true;
        }
        for (Instruction instruction : instructions) {
            if (isReachable(instruction)) {
                return true;
            }
        }
        return false;
    }

    /**
     * The end of the body
     */
    public Instruction getEnd() {
        return myEnd;
    }

    /**
     * The control falls off the end of the body - without a return or a throw
     */
    public boolean isEndReachable() {
        return isReachable(myEnd);
    }
}
