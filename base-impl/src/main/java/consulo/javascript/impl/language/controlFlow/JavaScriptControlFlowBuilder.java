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
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.application.progress.ProgressManager;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.javascript.psi.JSTypeOnlyDeclaration;
import consulo.javascript.psi.JavaScriptImportStatementBase;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.controlFlow.ControlFlow;
import consulo.language.controlFlow.ControlFlowBuilder;
import consulo.language.controlFlow.Instruction;
import consulo.language.psi.PsiComment;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.util.PsiTreeUtil;
import org.jspecify.annotations.Nullable;

import java.util.*;

/**
 * Builds the {@link JavaScriptControlFlow} of one flow owner on the platform {@link ControlFlowBuilder} - on the
 * JavaScript PSI interfaces, so the JavaScript and the TypeScript trees build the same graph: a node per statement (not
 * for blocks and empty statements), conditional nodes for the branches of {@code if}, loops, {@code ?:}, {@code &&},
 * {@code ||} and {@code ??}, a join where an optional chain short-circuits, jumps as pending edges (labeled break and
 * continue, switch clauses falling through), {@code finally} built once per way out of the {@code try} (normal
 * completion, each jump target, an exception), an edge from every node of a try block to its catch.
 * <p>
 * Like tsc, only the literals {@code true} and {@code false} are constant conditions: {@code while (true)} has no exit,
 * the then branch of {@code if (false)} and the right operand of {@code false && x} are unreachable; {@code (true)},
 * {@code !true}, {@code 1} are not folded. A function expression, an arrow function, a class is one node, its body
 * another owner; a function declaration (hoisted), an interface, a type alias, a {@code declare} none.
 *
 * @author VISTALL
 */
public class JavaScriptControlFlowBuilder {
    /**
     * The nodes a condition ends in when it is true, and when it is false - none: that branch is never taken
     */
    private record Exits(List<Instruction> whenTrue, List<Instruction> whenFalse) {
        private Exits swap() {
            return new Exits(whenFalse, whenTrue);
        }
    }

    /**
     * A statement a break or a continue can leave: a loop, a switch, a labeled statement
     */
    private record JumpTarget(PsiElement statement, Set<String> labels, boolean loop, boolean breakable, @Nullable PsiElement continueScope) {
    }

    /**
     * A jump out of a try with a finally, to the scope of its pending edge - null: to the exit
     */
    private record JumpKey(@Nullable PsiElement scope) {
    }

    private final ControlFlowBuilder myBuilder = new ControlFlowBuilder();
    private final Deque<JumpTarget> myTargets = new ArrayDeque<>();
    // the short-circuit exits of the optional chains being visited - of the outermost element of each
    private final Deque<List<Instruction>> myChains = new ArrayDeque<>();

    @RequiredReadAction
    static JavaScriptControlFlow build(PsiElement owner, PsiElement body) {
        return new JavaScriptControlFlowBuilder().buildImpl(owner, body);
    }

    @RequiredReadAction
    private JavaScriptControlFlow buildImpl(PsiElement owner, PsiElement body) {
        node(null, JavaScriptInstruction.ENTRY);

        JSParameterList parameterList = PsiTreeUtil.getChildOfType(owner, JSParameterList.class);
        if (parameterList != null) {
            for (PsiElement child = parameterList.getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child instanceof JSVariable parameter) {
                    variable(parameter);
                }
            }
        }

        if (body instanceof JSExpression expression) {
            // x => x + 1 - returns it
            expression(expression);
            exitTo(null);
        }
        else {
            statements(body);
        }

        JavaScriptInstruction end = node(owner, JavaScriptInstruction.END);
        node(null, JavaScriptInstruction.EXIT);

        ControlFlow flow = myBuilder.completeControlFlow();
        return new JavaScriptControlFlow(owner, flow.getInstructions(), end);
    }

    // region nodes

    private JavaScriptInstruction node(@Nullable PsiElement element) {
        return node(element, null);
    }

    private JavaScriptInstruction node(@Nullable PsiElement element, @Nullable String marker) {
        JavaScriptInstruction instruction = new JavaScriptInstruction(myBuilder, element, marker);
        myBuilder.addNodeAndCheckPending(instruction);
        return instruction;
    }

    /**
     * The flow continues after the first node out of the scope (null - the exit), and stops here
     */
    private void exitTo(@Nullable PsiElement scope) {
        myBuilder.addPendingEdge(scope, myBuilder.prevInstruction);
        myBuilder.flowAbrupted();
    }

    private List<Instruction> current() {
        Instruction instruction = myBuilder.prevInstruction;
        return instruction == null ? List.of() : List.of(instruction);
    }

    /**
     * The flow goes on from all the ends - joined in a node if more than one, stopped if none
     */
    private void join(List<Instruction> ends, PsiElement element) {
        Set<Instruction> distinct = new LinkedHashSet<>(ends);
        myBuilder.flowAbrupted();
        if (distinct.size() == 1) {
            myBuilder.prevInstruction = distinct.iterator().next();
        }
        else if (!distinct.isEmpty()) {
            edges(List.copyOf(distinct), node(element, JavaScriptInstruction.JOIN));
        }
    }

    /**
     * The start of a branch of a condition, from its exits - none if the branch is never taken
     */
    private void branch(List<Instruction> exits, PsiElement element, @Nullable PsiElement condition, boolean result) {
        myBuilder.flowAbrupted();
        if (exits.isEmpty()) {
            return;
        }
        if (condition == null) {
            join(exits, element);
            return;
        }
        JavaScriptConditionalInstruction instruction = new JavaScriptConditionalInstruction(myBuilder, element, condition, result);
        myBuilder.addNodeAndCheckPending(instruction);
        edges(exits, instruction);
    }

    private void edges(List<Instruction> from, @Nullable Instruction to) {
        for (Instruction instruction : from) {
            myBuilder.addEdge(instruction, to);
        }
    }

    /**
     * The ends of a construct in the scope: the current node and the pending edges out of a statement in the scope -
     * taken from the pending list, the flow stopped
     */
    private List<Instruction> collectEnds(PsiElement scope) {
        List<Instruction> ends = new ArrayList<>();
        if (myBuilder.prevInstruction != null) {
            ends.add(myBuilder.prevInstruction);
        }
        myBuilder.processPending((pendingScope, instruction) -> {
            if (pendingScope != null && PsiTreeUtil.isAncestor(scope, pendingScope, false)) {
                ends.add(instruction);
            }
            else {
                myBuilder.addPendingEdge(pendingScope, instruction);
            }
        });
        myBuilder.flowAbrupted();
        return ends;
    }

    private static List<Instruction> concat(List<Instruction> first, List<Instruction> second) {
        List<Instruction> result = new ArrayList<>(first);
        result.addAll(second);
        return result;
    }

    // endregion

    // region statements

    /**
     * The children of a block, a script, a clause - one after another
     */
    @RequiredReadAction
    private void statements(PsiElement parent) {
        for (PsiElement child = parent.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (isComposite(child)) {
                statement(child);
            }
        }
    }

    @RequiredReadAction
    private void statement(@Nullable PsiElement element) {
        if (element == null) {
            return;
        }
        ProgressManager.checkCanceled();
        if (element instanceof JSBlockStatement) {
            statements(element);
        }
        else if (element instanceof JSEmptyStatement) {
            // nothing
        }
        else if (element instanceof JSIfStatement statement) {
            ifStatement(statement);
        }
        else if (element instanceof JSWhileStatement statement) {
            whileStatement(statement);
        }
        else if (element instanceof JSDoWhileStatement statement) {
            doWhileStatement(statement);
        }
        else if (element instanceof JSForInStatement statement) {
            forInStatement(statement);
        }
        else if (element instanceof JSForStatement statement) {
            forStatement(statement);
        }
        else if (element instanceof JSSwitchStatement statement) {
            switchStatement(statement);
        }
        else if (element instanceof JSTryStatement statement) {
            tryStatement(statement);
        }
        else if (element instanceof JSLabeledStatement statement) {
            labeledStatement(statement);
        }
        else if (element instanceof JSBreakStatement statement) {
            breakStatement(statement);
        }
        else if (element instanceof JSContinueStatement statement) {
            continueStatement(statement);
        }
        else if (element instanceof JSReturnStatement || element instanceof JSThrowStatement) {
            node(element);
            expressions(element);
            exitTo(null);
        }
        else if (element instanceof JSVarStatement statement) {
            node(statement);
            for (PsiElement child = statement.getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child instanceof JSVariable variable) {
                    variable(variable);
                }
                else if (isComposite(child)) {
                    // a destructuring of the JavaScript tree
                    expression(child);
                }
            }
        }
        else if (element instanceof JSWithStatement statement) {
            node(statement);
            expression(statement.getExpression());
            statement(statement.getStatement());
        }
        else if (element instanceof JSExpression) {
            expression(element);
        }
        else if (element instanceof JSFunction || isTypeOnly(element)) {
            // a function declaration is hoisted; a type has no code
        }
        else if (element instanceof JSClass || hasChild(element, JSTokenTypes.ENUM_KEYWORD)) {
            // a class, a namespace, an enum: evaluated here, its members are not in this graph
            node(element);
        }
        else if (isWrapper(element)) {
            // a TypeScript statement around a declaration, an export
            for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child instanceof JSExpression) {
                    expression(child);
                }
                else if (isComposite(child) && !(child instanceof JSAttributeList)) {
                    statement(child);
                }
            }
        }
        else {
            // an expression statement, debugger, export default ...
            node(element);
            expressions(element);
        }
    }

    /**
     * An interface, a type alias, a declare, an import - no code here
     */
    @RequiredReadAction
    private static boolean isTypeOnly(PsiElement element) {
        if (element instanceof JSTypeOnlyDeclaration || element instanceof JavaScriptImportStatementBase || element instanceof JSClass jsClass && jsClass.isInterface()) {
            return true;
        }
        // declare const x: number; declare function f(): void;
        PsiElement first = PsiTreeUtil.getDeepestFirst(element);
        return !(element instanceof JSExpression) && !(element instanceof JSExpressionStatement)
            && PsiUtilCore.getElementType(first) != JSTokenTypes.IDENTIFIER && "declare".equals(first.getText());
    }

    /**
     * An element around declarations or statements, not an expression statement
     */
    private static boolean isWrapper(PsiElement element) {
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (isComposite(child) && !(child instanceof JSExpression) && !(child instanceof JSAttributeList) && !(child instanceof JavaScriptTypeElement)) {
                return true;
            }
        }
        return false;
    }

    @RequiredReadAction
    private void variable(JSVariable variable) {
        JSExpression initializer = variable.getInitializer();
        expression(initializer);
        // the defaults of a destructuring
        for (PsiElement child = variable.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child != initializer && isComposite(child)) {
                expression(child);
            }
        }
    }

    @RequiredReadAction
    private void ifStatement(JSIfStatement statement) {
        node(statement);
        JSExpression condition = statement.getCondition();
        Exits exits = condition(condition, true);

        JSStatement thenStatement = statement.getThen();
        JSStatement elseStatement = statement.getElse();

        branch(exits.whenTrue(), thenStatement != null ? thenStatement : statement, condition, true);
        statement(thenStatement);
        myBuilder.addPendingEdge(statement, myBuilder.prevInstruction);

        branch(exits.whenFalse(), elseStatement != null ? elseStatement : statement, condition, false);
        statement(elseStatement);
    }

    // endregion

    // region loops

    @RequiredReadAction
    private void whileStatement(JSWhileStatement statement) {
        Instruction head = node(statement);
        JSExpression condition = statement.getCondition();
        Exits exits = condition(condition, true);
        JSStatement body = statement.getBody();

        branch(exits.whenTrue(), body != null ? body : statement, condition, true);
        edges(loopBody(statement, body), head);
        branch(exits.whenFalse(), statement, condition, false);
    }

    @RequiredReadAction
    private void doWhileStatement(JSDoWhileStatement statement) {
        Instruction head = node(statement);
        JSStatement body = statement.getBody();
        List<Instruction> ends = loopBody(statement, body);
        JSExpression condition = statement.getCondition();
        if (condition == null) {
            return;
        }

        edges(ends, node(condition));
        Exits exits = condition(condition, true);
        branch(exits.whenTrue(), body != null ? body : statement, condition, true);
        myBuilder.addEdge(myBuilder.prevInstruction, head);
        branch(exits.whenFalse(), statement, condition, false);
    }

    @RequiredReadAction
    private void forStatement(JSForStatement statement) {
        node(statement);

        // for (initialization; condition; update) body - by the semicolons, the TypeScript tree has no var statement
        int part = 0;
        PsiElement condition = null;
        PsiElement update = null;
        JSStatement body = null;
        for (PsiElement child = statement.getFirstChild(); child != null; child = child.getNextSibling()) {
            IElementType type = PsiUtilCore.getElementType(child);
            if (type == JSTokenTypes.SEMICOLON) {
                part++;
            }
            else if (type == JSTokenTypes.RPAR) {
                part = 3;
            }
            else if (!isComposite(child)) {
                // for, (, let
            }
            else if (part == 0) {
                if (child instanceof JSVariable variable) {
                    variable(variable);
                }
                else {
                    statement(child);
                }
                // var i = 0; - the semicolon in the var statement
                if (child instanceof JSVarStatement && PsiUtilCore.getElementType(PsiTreeUtil.getDeepestLast(child)) == JSTokenTypes.SEMICOLON) {
                    part++;
                }
            }
            else if (part == 1) {
                condition = child;
            }
            else if (part == 2) {
                update = child;
            }
            else if (child instanceof JSStatement childStatement) {
                body = childStatement;
            }
        }

        Instruction head = condition != null ? node(condition) : node(statement, JavaScriptInstruction.LOOP);
        // for (;;) - true
        Exits exits = condition != null ? condition(condition, true) : new Exits(current(), List.of());

        branch(exits.whenTrue(), body != null ? body : statement, condition, true);
        List<Instruction> ends = loopBody(statement, body);
        if (update != null) {
            edges(ends, node(update));
            expression(update);
            ends = current();
        }
        edges(ends, head);
        branch(exits.whenFalse(), statement, condition, false);
    }

    /**
     * The collection is evaluated, then each time the next element is assigned to the target and the body runs
     */
    @RequiredReadAction
    private void forInStatement(JSForInStatement statement) {
        node(statement);
        expression(statement.getCollectionExpression());

        Instruction head = node(statement, JavaScriptInstruction.NEXT);
        // no more elements
        myBuilder.addPendingEdge(statement, head);

        JSVarStatement declaration = statement.getDeclarationStatement();
        JSExpression target = statement.getVariableExpression();
        if (declaration != null) {
            for (JSVariable variable : declaration.getVariables()) {
                variable(variable);
            }
        }
        else if (target != null) {
            expression(target);
        }
        else {
            // for (const x of a) of the TypeScript tree - the variable is a child
            for (PsiElement child = statement.getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child instanceof JSVariable variable) {
                    variable(variable);
                }
            }
        }

        edges(loopBody(statement, statement.getBody()), head);
    }

    /**
     * The body with the break and continue targets - its ends, the continues included
     */
    @RequiredReadAction
    private List<Instruction> loopBody(JSStatement loop, @Nullable JSStatement body) {
        PsiElement scope = body != null ? body : loop;
        myTargets.push(new JumpTarget(loop, labels(loop), true, true, scope));
        try {
            statement(body);
        }
        finally {
            myTargets.pop();
        }
        return collectEnds(scope);
    }

    /**
     * The labels of a statement - a: b: while (...)
     */
    @RequiredReadAction
    private static Set<String> labels(PsiElement statement) {
        Set<String> labels = new HashSet<>();
        for (PsiElement parent = statement.getParent(); parent instanceof JSLabeledStatement labeled; parent = parent.getParent()) {
            String label = labeled.getLabel();
            if (label != null) {
                labels.add(label);
            }
        }
        return labels;
    }

    // endregion

    // region switch

    /**
     * The case expressions are evaluated one after another until one matches, then the statements of its clause and
     * of the next ones (falling through) until a break; the default clause is taken when none matches
     */
    @RequiredReadAction
    private void switchStatement(JSSwitchStatement statement) {
        node(statement);
        expression(statement.getSwitchExpression());

        List<JSCaseClause> clauses = new ArrayList<>();
        for (PsiElement child = statement.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSCaseClause clause) {
                clauses.add(clause);
            }
        }

        Map<JSCaseClause, Instruction> entries = new HashMap<>();
        JSCaseClause defaultClause = null;
        Instruction test = myBuilder.prevInstruction;
        for (JSCaseClause clause : clauses) {
            if (clause.isDefault()) {
                defaultClause = clause;
                continue;
            }
            myBuilder.prevInstruction = test;
            expression(clause.getCaseExpression());
            test = myBuilder.prevInstruction;
            entries.put(clause, test);
        }
        if (defaultClause != null) {
            entries.put(defaultClause, test);
        }

        myTargets.push(new JumpTarget(statement, labels(statement), false, true, null));
        try {
            myBuilder.flowAbrupted();
            for (JSCaseClause clause : clauses) {
                // falls through from the clause before
                myBuilder.addEdge(entries.get(clause), node(clause));
                boolean statements = false;
                for (PsiElement child = clause.getFirstChild(); child != null; child = child.getNextSibling()) {
                    if (PsiUtilCore.getElementType(child) == JSTokenTypes.COLON) {
                        statements = true;
                    }
                    else if (statements && isComposite(child)) {
                        statement(child);
                    }
                }
            }
        }
        finally {
            myTargets.pop();
        }

        if (defaultClause == null) {
            myBuilder.addPendingEdge(statement, test);
        }
    }

    // endregion

    // region jumps

    @RequiredReadAction
    private void labeledStatement(JSLabeledStatement statement) {
        node(statement);
        myTargets.push(new JumpTarget(statement, labels(statement.getStatement()), false, false, null));
        try {
            statement(statement.getStatement());
        }
        finally {
            myTargets.pop();
        }
    }

    @RequiredReadAction
    private void breakStatement(JSBreakStatement statement) {
        node(statement);
        String label = jumpLabel(statement);
        for (JumpTarget target : myTargets) {
            if (label == null ? target.breakable() : target.labels().contains(label)) {
                exitTo(target.statement());
                return;
            }
        }
        // out of nothing (a syntax error) - the flow goes on
    }

    @RequiredReadAction
    private void continueStatement(JSContinueStatement statement) {
        node(statement);
        String label = jumpLabel(statement);
        for (JumpTarget target : myTargets) {
            if (target.loop() && (label == null || target.labels().contains(label))) {
                exitTo(target.continueScope());
                return;
            }
        }
    }

    @RequiredReadAction
    private static @Nullable String jumpLabel(PsiElement statement) {
        ASTNode identifier = statement.getNode().findChildByType(JSTokenTypes.IDENTIFIER);
        return identifier == null ? null : identifier.getText();
    }

    // endregion

    // region try

    @RequiredReadAction
    private void tryStatement(JSTryStatement statement) {
        Instruction tryNode = node(statement);

        // try { } catch (e) { } finally { } - by the children: the getters differ in the JavaScript and TypeScript trees
        JSBlockStatement tryBlock = null;
        JSBlockStatement finallyBlock = null;
        List<JSCatchBlock> catches = new ArrayList<>();
        boolean afterFinally = false;
        for (PsiElement child = statement.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (PsiUtilCore.getElementType(child) == JSTokenTypes.FINALLY_KEYWORD) {
                afterFinally = true;
            }
            else if (child instanceof JSCatchBlock catchBlock) {
                catches.add(catchBlock);
            }
            else if (child instanceof JSBlockStatement block) {
                if (afterFinally) {
                    finallyBlock = block;
                }
                else if (tryBlock == null) {
                    tryBlock = block;
                }
            }
        }

        List<Instruction> normalEnds = new ArrayList<>();
        if (tryBlock != null) {
            statements(tryBlock);
            normalEnds.addAll(collectEnds(tryBlock));
        }
        // an exception from any node of the try block - or before its first one
        List<Instruction> throwing = new ArrayList<>(myBuilder.instructions.subList(tryNode.num(), myBuilder.instructions.size()));

        for (JSCatchBlock catchBlock : catches) {
            myBuilder.flowAbrupted();
            edges(throwing, node(catchBlock));
            for (PsiElement child = catchBlock.getFirstChild(); child != null; child = child.getNextSibling()) {
                if (child instanceof JSVariable parameter) {
                    variable(parameter);
                }
                else if (child instanceof JSBlockStatement block) {
                    statements(block);
                }
            }
            normalEnds.addAll(collectEnds(catchBlock));
        }

        if (finallyBlock == null) {
            for (Instruction end : normalEnds) {
                myBuilder.addPendingEdge(statement, end);
            }
            return;
        }

        // the jumps out of the try and the catches - each through its own copy of the finally block
        Map<JumpKey, List<Instruction>> jumps = new LinkedHashMap<>();
        int tryNumber = tryNode.num();
        myBuilder.processPending((pendingScope, instruction) -> {
            if (instruction.num() > tryNumber) {
                jumps.computeIfAbsent(new JumpKey(pendingScope), it -> new ArrayList<>()).add(instruction);
            }
            else {
                myBuilder.addPendingEdge(pendingScope, instruction);
            }
        });

        // an exception from the try block or a catch - the finally block, then up to the exit
        List<Instruction> failing = new ArrayList<>(myBuilder.instructions.subList(tryNumber, myBuilder.instructions.size()));
        for (Instruction end : finallyCopy(finallyBlock, failing)) {
            myBuilder.addPendingEdge(null, end);
        }

        if (!normalEnds.isEmpty()) {
            for (Instruction end : finallyCopy(finallyBlock, normalEnds)) {
                myBuilder.addPendingEdge(statement, end);
            }
        }

        for (Map.Entry<JumpKey, List<Instruction>> jump : jumps.entrySet()) {
            for (Instruction end : finallyCopy(finallyBlock, jump.getValue())) {
                myBuilder.addPendingEdge(jump.getKey().scope(), end);
            }
        }
        myBuilder.flowAbrupted();
    }

    @RequiredReadAction
    private List<Instruction> finallyCopy(JSBlockStatement finallyBlock, List<Instruction> from) {
        myBuilder.flowAbrupted();
        edges(from, node(finallyBlock, JavaScriptInstruction.FINALLY));
        statements(finallyBlock);
        return collectEnds(finallyBlock);
    }

    // endregion

    // region expressions

    /**
     * The composite children of an element as expressions, one after another
     */
    @RequiredReadAction
    private void expressions(PsiElement element) {
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (isComposite(child)) {
                expression(child);
            }
        }
    }

    @RequiredReadAction
    private void expression(@Nullable PsiElement element) {
        if (element == null || element instanceof JavaScriptTypeElement) {
            return;
        }
        ProgressManager.checkCanceled();
        if (element instanceof JSFunction || element instanceof JSClass || element instanceof JSClassExpression
            || JavaScriptControlFlow.isPropertyMethod(element)) {
            // created here, its body is another owner
            node(element);
            return;
        }
        if (isLogical(element)) {
            join(allExits(condition(element, true)), element);
            return;
        }
        if (element instanceof JSConditionalExpression conditional) {
            Exits exits = condition(conditional.getCondition(), true);
            join(exits.whenTrue(), conditional);
            expression(conditional.getThen());
            List<Instruction> thenEnd = current();
            join(exits.whenFalse(), conditional);
            expression(conditional.getElse());
            join(concat(thenEnd, current()), conditional);
            return;
        }
        IElementType operator = operator(element);
        if (operator == JSTokenTypes.OR_OR_EQ || operator == JSTokenTypes.AND_AND_EQ || operator == JSTokenTypes.QUEST_QUEST_EQ) {
            // x ||= y - assigned only when x is falsy
            PsiElement[] operands = operands(element);
            expression(operands[0]);
            List<Instruction> shortCircuit = current();
            expression(operands[1]);
            join(concat(shortCircuit, current()), element);
            return;
        }
        if (isChainElement(element)) {
            chain(element);
            return;
        }
        expressions(element);
    }

    private static List<Instruction> allExits(Exits exits) {
        return concat(exits.whenTrue(), exits.whenFalse());
    }

    /**
     * Visits a condition: {@code &&}, {@code ||}, {@code !} of them branch, {@code ??} goes on with its right operand
     * when the left one is null; the literals true and false take one exit only (fold) - not the right operand of
     * {@code ??}, a parenthesized or a negated literal, as tsc
     */
    @RequiredReadAction
    private Exits condition(@Nullable PsiElement expression, boolean fold) {
        if (expression == null) {
            return new Exits(current(), current());
        }

        if (expression instanceof JSParenthesizedExpression parenthesized && isLogical(parenthesized.getInnerExpression())) {
            return condition(parenthesized.getInnerExpression(), fold);
        }
        if (expression instanceof JSPrefixExpression prefix && prefix.getOperationSign() == JSTokenTypes.EXCL && isLogical(prefix.getExpression())) {
            return condition(prefix.getExpression(), fold).swap();
        }

        IElementType operator = operator(expression);
        if (operator == JSTokenTypes.ANDAND || operator == JSTokenTypes.OROR) {
            PsiElement[] operands = operands(expression);
            Exits left = condition(operands[0], fold);
            join(operator == JSTokenTypes.ANDAND ? left.whenTrue() : left.whenFalse(), expression);
            Exits right = condition(operands[1], fold);
            return operator == JSTokenTypes.ANDAND
                ? new Exits(right.whenTrue(), concat(left.whenFalse(), right.whenFalse()))
                : new Exits(concat(left.whenTrue(), right.whenTrue()), right.whenFalse());
        }
        if (operator == JSTokenTypes.QUEST_QUEST) {
            PsiElement[] operands = operands(expression);
            expression(operands[0]);
            List<Instruction> notNull = current();
            Exits right = condition(operands[1], false);
            return new Exits(concat(notNull, right.whenTrue()), concat(notNull, right.whenFalse()));
        }

        expression(expression);
        IElementType literal = fold ? literal(expression) : null;
        if (literal == JSTokenTypes.TRUE_KEYWORD) {
            return new Exits(current(), List.of());
        }
        if (literal == JSTokenTypes.FALSE_KEYWORD) {
            return new Exits(List.of(), current());
        }
        return new Exits(current(), current());
    }

    /**
     * {@code &&}, {@code ||}, {@code ??}, and those in parentheses or negated (tsc's isLogicalExpression)
     */
    @RequiredReadAction
    private static boolean isLogical(@Nullable PsiElement expression) {
        while (true) {
            if (expression instanceof JSParenthesizedExpression parenthesized) {
                expression = parenthesized.getInnerExpression();
            }
            else if (expression instanceof JSPrefixExpression prefix && prefix.getOperationSign() == JSTokenTypes.EXCL) {
                expression = prefix.getExpression();
            }
            else {
                IElementType operator = operator(expression);
                return operator == JSTokenTypes.ANDAND || operator == JSTokenTypes.OROR || operator == JSTokenTypes.QUEST_QUEST;
            }
        }
    }

    /**
     * The operator token between the two operands of a binary expression - of the JavaScript tree, or of the TypeScript
     * one (logical expressions are not JSBinaryExpression there)
     */
    @RequiredReadAction
    private static @Nullable IElementType operator(@Nullable PsiElement expression) {
        if (!(expression instanceof JSExpression)) {
            return null;
        }
        boolean left = false;
        for (PsiElement child = expression.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression) {
                if (left) {
                    return null;
                }
                left = true;
            }
            else if (left && !(child instanceof PsiWhiteSpace) && !(child instanceof PsiComment)) {
                IElementType type = PsiUtilCore.getElementType(child);
                return JSTokenTypes.OPERATIONS.contains(type) ? type : null;
            }
        }
        return null;
    }

    /**
     * The left and the right operand - either can be null
     */
    private static PsiElement[] operands(PsiElement expression) {
        PsiElement[] operands = new PsiElement[2];
        int index = 0;
        for (PsiElement child = expression.getFirstChild(); child != null && index < 2; child = child.getNextSibling()) {
            if (child instanceof JSExpression) {
                operands[index++] = child;
            }
        }
        return operands;
    }

    /**
     * true or false - a literal expression of the keyword only
     */
    private static @Nullable IElementType literal(PsiElement expression) {
        PsiElement child = expression.getFirstChild();
        if (!(expression instanceof JSExpression) || child == null || child != expression.getLastChild()) {
            return null;
        }
        IElementType type = PsiUtilCore.getElementType(child);
        return type == JSTokenTypes.TRUE_KEYWORD || type == JSTokenTypes.FALSE_KEYWORD ? type : null;
    }

    // endregion

    // region optional chains

    private static boolean isChainElement(@Nullable PsiElement element) {
        return element instanceof JSReferenceExpression || element instanceof JSCallExpression || element instanceof JSIndexedPropertyAccessExpression;
    }

    /**
     * a.b, a(), a[i] - a
     */
    private static @Nullable PsiElement qualifier(PsiElement element) {
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression) {
                return child;
            }
            if (child instanceof JSArgumentList) {
                return null;
            }
        }
        return null;
    }

    private static boolean hasChild(PsiElement element, IElementType type) {
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (PsiUtilCore.getElementType(child) == type) {
                return true;
            }
        }
        return false;
    }

    /**
     * a?.b.c(d) - when a is null or undefined the rest of the chain is skipped: an exit after a, joined at the end of
     * the outermost element of the chain
     */
    @RequiredReadAction
    private void chain(PsiElement element) {
        PsiElement parent = element.getParent();
        boolean outermost = !(isChainElement(parent) && qualifier(parent) == element);
        boolean optional = false;
        if (outermost) {
            for (PsiElement link = element; isChainElement(link); link = qualifier(link)) {
                if (hasChild(link, JSTokenTypes.QUEST_DOT)) {
                    optional = true;
                    break;
                }
            }
        }
        if (optional) {
            myChains.push(new ArrayList<>());
        }

        PsiElement qualifier = qualifier(element);
        expression(qualifier);
        if (hasChild(element, JSTokenTypes.QUEST_DOT) && !myChains.isEmpty()) {
            myChains.peek().addAll(current());
        }
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child != qualifier && isComposite(child)) {
                expression(child);
            }
        }

        if (optional) {
            join(concat(current(), myChains.pop()), element);
        }
    }

    // endregion

    /**
     * Not a token, a whitespace or a comment
     */
    private static boolean isComposite(PsiElement element) {
        return element.getFirstChild() != null && !(element instanceof PsiComment);
    }
}
