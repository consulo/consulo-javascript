/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.intention.switchtoif;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiManager;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.*;
import org.jetbrains.annotations.NonNls;

import java.util.*;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSReplaceSwitchWithIfIntention",
    categories = {"JavaScript", "Control Flow"},
    fileExtensions = "js"
)
public class JSReplaceSwitchWithIfIntention extends JSIntention {
    @NonNls
    private static final String IF_PREFIX = "if (";
    @NonNls
    private static final String IF_SUFFIX = ") {";
    @NonNls
    private static final String ELSE = "} else {";
    @NonNls
    private static final String ELSE_KEYWORD = "else ";
    @NonNls
    private static final String VAR_PREFIX = "var ";
    @NonNls
    private static final String BREAK_KEYWORD = "break ";
    @NonNls
    private static final String DEFAULT_LABEL_NAME = "Label";

    @Override
    @Nonnull
    public String getText() {
        return JSIntentionLocalize.switchtoifReplaceSwitchWithIf().get();
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new SwitchPredicate();
    }

    @Override
    @RequiredReadAction
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        final JSSwitchStatement switchStatement = (JSSwitchStatement)element.getParent();

        assert (switchStatement != null);

        final PsiManager mgr = switchStatement.getManager();
        final JSExpression switchExpression = switchStatement.getSwitchExpression();
        final CodeStyleManager codeStyleMgr = CodeStyleManager.getInstance(element.getProject());
        final boolean hadSideEffects = SideEffectChecker.mayHaveSideEffects(switchExpression);
        final String declarationString;
        final String expressionText;

        if (hadSideEffects) {
            final String variableName =
                "i"; // TODO JavaCodeStyleManager.getInstance(switchExpression.getProject()).suggestUniqueVariableName("i", switchExpression, true);

            expressionText = variableName;
            declarationString = VAR_PREFIX + variableName + " = " + switchExpression.getText() + ';';
        }
        else {
            declarationString = null;
            expressionText = switchExpression.getText();
        }

        boolean renameBreaks = false;

        for (JSCaseClause caseClause : switchStatement.getCaseClauses()) {
            if (CaseUtil.containsHiddenBreak(caseClause)) {
                renameBreaks = true;
                break;
            }
        }

        final StringBuilder ifStatementBuffer = new StringBuilder(1024);
        String breakLabel = null;

        if (renameBreaks) {
            breakLabel = CaseUtil.findUniqueLabel(switchStatement, DEFAULT_LABEL_NAME);
            ifStatementBuffer.append(breakLabel).append(':');
        }

        final List<SwitchStatementBranch> openBranches = new ArrayList<>();
        final Set<JSVariable> declaredVars = new HashSet<>(5);
        final List<SwitchStatementBranch> allBranches = new ArrayList<>();
        SwitchStatementBranch currentBranch = null;

        for (JSCaseClause caseClause : switchStatement.getCaseClauses()) {
            final PsiElement[] caseClauseChildren = caseClause.getChildren();

            for (PsiElement child : caseClauseChildren) {
                if (child == caseClauseChildren[0]) {
                    if (currentBranch == null) {
                        openBranches.clear();
                        currentBranch = new SwitchStatementBranch();
                        currentBranch.addPendingVariableDeclarations(declaredVars);
                        allBranches.add(currentBranch);
                        openBranches.add(currentBranch);
                    }
                    else if (currentBranch.hasStatements()) {
                        currentBranch = new SwitchStatementBranch();
                        allBranches.add(currentBranch);
                        openBranches.add(currentBranch);
                    }

                    if (caseClause.isDefault()) {
                        currentBranch.setDefault();
                    }
                }

                if (child instanceof JSExpression) {
                    // Processes case clause expression
                    final JSExpression value = ParenthesesUtils.stripParentheses(caseClause.getCaseExpression());

                    assert (currentBranch != null);
                    currentBranch.addLabel(value.getText());
                }
                else if (child instanceof JSStatement) {
                    // Processes case clause statements
                    final JSStatement statement = (JSStatement)child;

                    if (statement instanceof JSVarStatement) {
                        Collections.addAll(declaredVars, ((JSVarStatement)statement).getVariables());
                    }
                    else if (statement instanceof JSBlockStatement) {
                        for (PsiElement blockElement : statement.getChildren()) {
                            final boolean isJsElement = (blockElement instanceof JSElement);
                            final boolean isWhiteSpace = (blockElement instanceof PsiWhiteSpace);

                            for (SwitchStatementBranch branch : openBranches) {
                                if (isJsElement) {
                                    branch.addStatement((JSElement)blockElement);
                                }
                                else if (isWhiteSpace) {
                                    branch.addWhiteSpace(blockElement);
                                }
                                else {
                                    branch.addComment(blockElement);
                                }
                            }
                        }
                    }
                    else {
                        for (SwitchStatementBranch branch : openBranches) {
                            branch.addStatement(statement);
                        }
                    }

                    if (!ControlFlowUtils.statementMayCompleteNormally(statement)) {
                        currentBranch = null;
                    }
                }
                else {
                    final boolean isWhiteSpace = (child instanceof PsiWhiteSpace);

                    for (SwitchStatementBranch openBranch : openBranches) {
                        if (isWhiteSpace) {
                            openBranch.addWhiteSpace(child);
                        }
                        else {
                            openBranch.addComment(child);
                        }
                    }
                }
            }
        }

        boolean firstBranch = true;
        SwitchStatementBranch defaultBranch = null;

        for (SwitchStatementBranch branch : allBranches) {
            if (branch.isDefault()) {
                defaultBranch = branch;
            }
            else {
                final List<String> labels = branch.getLabels();
                final List<PsiElement> bodyElements = branch.getBodyElements();
                final Set<JSVariable> pendingVariableDeclarations = branch.getPendingVariableDeclarations();

                dumpBranch(
                    ifStatementBuffer,
                    expressionText,
                    labels,
                    bodyElements,
                    firstBranch,
                    renameBreaks && CaseUtil.containsHiddenBreak(bodyElements),
                    breakLabel,
                    pendingVariableDeclarations
                );
                firstBranch = false;
            }
        }
        if (defaultBranch != null) {
            final List<PsiElement> bodyElements = defaultBranch.getBodyElements();
            final Set<JSVariable> pendingVariableDeclarations = defaultBranch.getPendingVariableDeclarations();

            dumpDefaultBranch(
                ifStatementBuffer,
                bodyElements,
                firstBranch,
                renameBreaks,
                breakLabel,
                pendingVariableDeclarations
            );
        }

        if (hadSideEffects) {
            final String ifStatementString = ifStatementBuffer.toString();
            final JSStatement declarationStatement =
                (JSStatement)JSChangeUtil.createStatementFromText(element.getProject(), declarationString).getPsi();
            final JSStatement ifStatement =
                (JSStatement)JSChangeUtil.createStatementFromText(element.getProject(), ifStatementString).getPsi();

            codeStyleMgr.reformat(declarationStatement);
            codeStyleMgr.reformat(ifStatement);
            JSElementFactory.replaceStatement(switchStatement, declarationStatement.getText() + '\n' + ifStatement.getText());
        }
        else {
            final String ifStatementString = ifStatementBuffer.toString();
            final JSStatement newStatement =
                (JSStatement)JSChangeUtil.createStatementFromText(element.getProject(), ifStatementString).getPsi();

            codeStyleMgr.reformat(newStatement);
            JSElementFactory.replaceStatement(switchStatement, newStatement.getText());
        }
    }

    @RequiredReadAction
    private static void dumpBranch(
        StringBuilder ifStatementString,
        String expressionText,
        List<String> labels,
        List<PsiElement> bodyStatements,
        boolean firstBranch,
        boolean renameBreaks,
        String breakLabel,
        Set<JSVariable> variableDecls
    ) {
        if (!firstBranch) {
            ifStatementString.append(ELSE_KEYWORD);
        }
        dumpLabels(ifStatementString, expressionText, labels);
        dumpBody(ifStatementString, bodyStatements, renameBreaks, breakLabel, variableDecls);
    }

    @RequiredReadAction
    private static void dumpDefaultBranch(
        StringBuilder ifStatementString,
        List<PsiElement> bodyStatements,
        boolean firstBranch,
        boolean renameBreaks,
        String breakLabel,
        Set<JSVariable> variableDecls
    ) {
        if (!firstBranch) {
            ifStatementString.append(ELSE_KEYWORD);
        }
        dumpBody(ifStatementString, bodyStatements, renameBreaks, breakLabel, variableDecls);
    }

    private static void dumpLabels(StringBuilder ifStatementString, String expressionText, List<String> labels) {
        boolean firstLabel = true;

        ifStatementString.append(IF_PREFIX);
        for (String label : labels) {
            if (!firstLabel) {
                ifStatementString.append("||");
            }
            firstLabel = false;

            ifStatementString.append(expressionText).append("==").append(label);
        }
        ifStatementString.append(')');
    }

    @RequiredReadAction
    private static void dumpBody(
        StringBuilder ifStatementString,
        List<PsiElement> bodyStatements,
        boolean renameBreaks,
        String breakLabel,
        Set<JSVariable> variableDecls
    ) {
        ifStatementString.append('{');
        for (final JSVariable var : variableDecls) {
            if (CaseUtil.isUsedByStatementList(var, bodyStatements)) {
                ifStatementString.append(VAR_PREFIX)
                    .append(var.getName())
                    .append(';');
            }
        }

        for (final PsiElement bodyStatement : bodyStatements) {
            if (!(bodyStatement instanceof JSBreakStatement)) {
                appendElement(ifStatementString, bodyStatement, renameBreaks,
                    breakLabel
                );
            }
        }
        ifStatementString.append('}');
    }

    @RequiredReadAction
    private static void appendElement(
        StringBuilder ifStatementString,
        PsiElement element,
        boolean renameBreakElements,
        String breakLabelString
    ) {
        if (!renameBreakElements) {
            final String text = element.getText();

            ifStatementString.append(text);
        }
        else if (element instanceof JSBreakStatement breakStatement) {
            final String identifier = breakStatement.getLabel();

            if (identifier == null || identifier.isEmpty()) {
                ifStatementString.append(BREAK_KEYWORD).append(breakLabelString).append(';');
            }
            else {
                ifStatementString.append(element.getText());
            }
        }
        else if (element instanceof JSBlockStatement) {
            for (final PsiElement child : element.getChildren()) {
                appendElement(ifStatementString, child, renameBreakElements, breakLabelString);
            }
        }
        else if (element instanceof JSIfStatement ifStatement) {
            JSStatement elseBranch = ifStatement.getElse();

            ifStatementString.append(IF_PREFIX)
                .append(ifStatement.getCondition().getText())
                .append(IF_SUFFIX);
            appendElement(ifStatementString, ifStatement.getThen(), renameBreakElements, breakLabelString);
            if (elseBranch != null) {
                ifStatementString.append(ELSE);
                appendElement(ifStatementString, elseBranch, renameBreakElements, breakLabelString);
            }
            ifStatementString.append('}');
        }
        else {
            ifStatementString.append(element.getText());
        }
    }

    private static class SwitchPredicate implements JSElementPredicate {
        @Override
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            final PsiElement parent = element.getParent();

            if (parent instanceof JSSwitchStatement switchStatement) {
                if (ErrorUtil.containsError(switchStatement)) {
                    return false;
                }

                final JSExpression expression = switchStatement.getSwitchExpression();

                return expression != null && expression.isValid();
            }
            else {
                return false;
            }
        }
    }

    private static class SwitchStatementBranch {
        private final Set<JSVariable> pendingVariableDeclarations = new HashSet<>(5);
        private final List<String> labels = new ArrayList<>(2);
        private final List<PsiElement> bodyElements = new ArrayList<>(5);
        private final List<PsiElement> pendingWhiteSpace = new ArrayList<>(2);
        private boolean isDefault;
        private boolean hasStatements;

        public void addLabel(String labelString) {
            this.labels.add(labelString);
        }

        public void addStatement(JSElement statement) {
            this.hasStatements = true;
            this.addElement(statement);
        }

        public void addComment(PsiElement comment) {
            this.addElement(comment);
        }

        private void addElement(PsiElement element) {
            this.bodyElements.addAll(this.pendingWhiteSpace);
            this.pendingWhiteSpace.clear();
            this.bodyElements.add(element);
        }

        public void addWhiteSpace(PsiElement whiteSpace) {
            if (this.bodyElements.size() > 0) {
                this.pendingWhiteSpace.add(whiteSpace);
            }
        }

        public List<String> getLabels() {
            return Collections.unmodifiableList(this.labels);
        }

        public List<PsiElement> getBodyElements() {
            return Collections.unmodifiableList(this.bodyElements);
        }

        public boolean isDefault() {
            return this.isDefault;
        }

        public void setDefault() {
            this.isDefault = true;
        }

        public boolean hasStatements() {
            return this.hasStatements;
        }

        public void addPendingVariableDeclarations(Set<JSVariable> vars) {
            this.pendingVariableDeclarations.addAll(vars);
        }

        public Set<JSVariable> getPendingVariableDeclarations() {
            return Collections.unmodifiableSet(this.pendingVariableDeclarations);
        }
    }
}
