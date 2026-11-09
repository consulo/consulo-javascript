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
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLoopStatement;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.PsiUtilCore;
import org.jspecify.annotations.Nullable;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The variables a flow graph reads and writes: the variables, parameters and names of destructurings declared in the
 * owner, not in a nested one - a reference resolving to one of them
 *
 * @author VISTALL
 */
public final class JavaScriptFlowVariables {
    public enum Kind {
        VAR,
        LET,
        CONST,
        PARAMETER
    }

    private JavaScriptFlowVariables() {
    }

    /**
     * The variables of the owner by their names - a reference by another name is none of them, and is not resolved
     */
    @RequiredReadAction
    static Map<String, List<PsiElement>> byName(PsiElement owner) {
        Map<String, List<PsiElement>> variables = new HashMap<>();
        for (JSVariable variable : declarations(owner)) {
            for (PsiElement target : targets(variable)) {
                variables.computeIfAbsent(name(target), it -> new ArrayList<>(1)).add(target);
            }
        }
        return variables;
    }

    /**
     * The variables and parameters declared in the owner - not in a nested one
     */
    @RequiredReadAction
    public static List<JSVariable> declarations(PsiElement owner) {
        List<JSVariable> variables = new ArrayList<>();
        Deque<PsiElement> stack = new ArrayDeque<>();
        stack.push(owner);
        while (!stack.isEmpty()) {
            PsiElement element = stack.pop();
            for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child.getFirstChild() == null || JavaScriptControlFlow.isOwner(child) || child instanceof JSClass) {
                    continue;
                }
                if (child instanceof JSVariable variable) {
                    variables.add(variable);
                }
                else {
                    stack.push(child);
                }
            }
        }
        return variables;
    }

    /**
     * What a variable declares: itself, or the names of its destructuring - the elements a reference resolves to
     */
    @RequiredReadAction
    static List<PsiElement> targets(JSVariable variable) {
        List<PsiElement> targets = new ArrayList<>();
        JSExpression initializer = variable.getInitializer();
        Deque<PsiElement> stack = new ArrayDeque<>();
        stack.push(variable);
        while (!stack.isEmpty()) {
            for (PsiElement child = stack.pop().getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child.getFirstChild() == null || child == initializer || child instanceof JSExpression || child instanceof JavaScriptTypeElement) {
                    continue;
                }
                if (child instanceof JSVariable || isName(child) && child.getParent() != variable) {
                    // { q } of the JavaScript tree, [a, b] of the TypeScript one
                    targets.add(child);
                }
                else {
                    stack.push(child);
                }
            }
        }
        return targets.isEmpty() ? List.of(variable) : targets;
    }

    /**
     * A binding of a name only - a TypeScript binding pattern of an identifier
     */
    private static boolean isName(PsiElement element) {
        PsiElement child = element.getFirstChild();
        return child != null && child == element.getLastChild() && PsiUtilCore.getElementType(child) == JSTokenTypes.IDENTIFIER;
    }

    /**
     * The element is a variable of the owner - the target of a reference
     */
    @RequiredReadAction
    static boolean isVariableOf(@Nullable PsiElement target, PsiElement owner) {
        if (target == null || JavaScriptControlFlow.ownerOf(target) != owner) {
            return false;
        }
        if (target instanceof JSVariable) {
            return true;
        }
        // a name of a destructuring
        for (PsiElement parent = target.getParent(); parent != null && parent != owner; parent = parent.getParent()) {
            if (parent instanceof JSVariable) {
                return isName(target);
            }
        }
        return false;
    }

    /**
     * var, let, const or a parameter (also of a catch) - of the outermost variable declaring the target: { q } of a
     * parameter is a parameter
     */
    @RequiredReadAction
    public static Kind kind(PsiElement target) {
        PsiElement variable = null;
        for (PsiElement element = target; element != null && !JavaScriptControlFlow.isOwner(element) && !(element instanceof JSClass); element = element.getParent()) {
            if (element instanceof JSVariable) {
                variable = element;
            }
        }
        if (variable instanceof JSParameter) {
            return Kind.PARAMETER;
        }
        if (variable == null) {
            return Kind.VAR;
        }
        // the keyword before the variable or its destructuring: let a, b; for (const x of y); const { a } = b
        for (PsiElement element = variable; element != null && !JavaScriptControlFlow.isOwner(element); element = element.getParent()) {
            for (PsiElement previous = element.getPrevSibling(); previous != null; previous = previous.getPrevSibling()) {
                IElementType type = PsiUtilCore.getElementType(previous);
                if (type == JSTokenTypes.LET_KEYWORD) {
                    return Kind.LET;
                }
                if (type == JSTokenTypes.CONST_KEYWORD) {
                    return Kind.CONST;
                }
                if (type == JSTokenTypes.VAR_KEYWORD) {
                    return Kind.VAR;
                }
            }
            if (element instanceof JSVarStatement || element instanceof JSLoopStatement) {
                break;
            }
        }
        return Kind.VAR;
    }

    public static String name(PsiElement target) {
        String name = target instanceof PsiNamedElement named ? named.getName() : null;
        return name != null ? name : target.getText();
    }
}
