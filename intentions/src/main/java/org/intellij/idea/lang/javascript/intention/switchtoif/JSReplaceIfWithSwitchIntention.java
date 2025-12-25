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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.ast.IElementType;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.*;
import org.jetbrains.annotations.NonNls;

import java.util.*;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSReplaceIfWithSwitchIntention",
    categories = {"JavaScript", "Control Flow"},
    fileExtensions = "js"
)
public class JSReplaceIfWithSwitchIntention extends JSIntention {
    @NonNls
    private static final String IF_KEYWORD = "if";
    @NonNls
    private static final String DEFAULT_LABEL_NAME = "Label";
    @NonNls
    private static final String DEFAULT_CASE_CLAUSE_EXPRESSION = "default: ";
    @NonNls
    private static final String LABELED_BREAK_STATEMENT_PREFIX = "break ";
    @NonNls
    private static final String BREAK_STATEMENT = "\nbreak;";

    @Override
    @Nonnull
    public LocalizeValue getText() {
        return JSIntentionLocalize.switchtoifReplaceIfWithSwitch();
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new IfToSwitchPredicate();
    }

    @Override
    @RequiredReadAction
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        JSIfStatement ifStatement = (JSIfStatement)element.getParent();

        assert (ifStatement != null);

        boolean breaksNeedRelabeled = false;
        JSStatement breakTarget = null;
        String labelString = "";

        if (ControlFlowUtils.statementContainsExitingBreak(ifStatement)) {
            JSElement ancestor = (JSElement)ifStatement.getParent();
            while (ancestor != null) {
                if (ancestor instanceof JSForStatement
                    || ancestor instanceof JSForInStatement
                    || ancestor instanceof JSDoWhileStatement
                    || ancestor instanceof JSWhileStatement
                    || ancestor instanceof JSSwitchStatement) {
                    breakTarget = (JSStatement)ancestor;
                    break;
                }
                ancestor = (JSElement)ancestor.getParent();
            }

            if (breakTarget != null) {
                labelString = CaseUtil.findUniqueLabel(ifStatement, DEFAULT_LABEL_NAME);
                breaksNeedRelabeled = true;
            }
        }
        JSIfStatement statementToReplace = ifStatement;
        JSExpression caseExpression = CaseUtil.getCaseExpression(ifStatement);

        assert (caseExpression != null);

        StringBuilder switchStatementBuffer = new StringBuilder(1024);

        switchStatementBuffer.append("switch(").append(caseExpression.getText()).append(')')
            .append('{');

        List<IfStatementBranch> branches = new ArrayList<>(20);

        while (true) {
            Set<String> topLevelVariables = new HashSet<>(5);
            Set<String> innerVariables = new HashSet<>(5);
            JSExpression condition = ifStatement.getCondition();
            JSExpression[] labels = getValuesFromCondition(condition, caseExpression);
            JSStatement thenBranch = ifStatement.getThen();

            DeclarationUtils.calculateVariablesDeclared(thenBranch, topLevelVariables, innerVariables, true);

            IfStatementBranch ifBranch = new IfStatementBranch();

            ifBranch.setInnerVariables(innerVariables);
            ifBranch.setTopLevelVariables(topLevelVariables);
            ifBranch.setStatement(thenBranch);
            for (JSExpression label : labels) {
                ifBranch.addCondition(label.getText());
            }
            branches.add(ifBranch);

            JSStatement elseBranch = ifStatement.getElse();

            if (elseBranch instanceof JSIfStatement elseIfStatement) {
                ifStatement = elseIfStatement;
            }
            else if (elseBranch == null) {
                break;
            }
            else {
                Set<String> elseTopLevelVariables = new HashSet<>(5);
                Set<String> elseInnerVariables = new HashSet<>(5);

                DeclarationUtils.calculateVariablesDeclared(elseBranch, elseTopLevelVariables, elseInnerVariables, true);

                IfStatementBranch elseIfBranch = new IfStatementBranch();

                elseIfBranch.setInnerVariables(elseInnerVariables);
                elseIfBranch.setTopLevelVariables(elseTopLevelVariables);
                elseIfBranch.setElse();
                elseIfBranch.setStatement(elseBranch);
                branches.add(elseIfBranch);
                break;
            }
        }

        for (IfStatementBranch branch : branches) {
            boolean hasConflicts = false;
            for (IfStatementBranch testBranch : branches) {
                if (branch.topLevelDeclarationsConfictWith(testBranch)) {
                    hasConflicts = true;
                }
            }

            JSStatement branchStatement = branch.getStatement();

            if (branch.isElse()) {
                dumpDefaultBranch(switchStatementBuffer, branchStatement, hasConflicts, breaksNeedRelabeled, labelString);
            }
            else {
                List<String> conditions = branch.getConditions();

                dumpBranch(switchStatementBuffer, conditions, branchStatement, hasConflicts, breaksNeedRelabeled, labelString);
            }
        }

        switchStatementBuffer.append('}');

        String switchStatementString = switchStatementBuffer.toString();

        if (breaksNeedRelabeled) {
            int length = switchStatementBuffer.length();
            StringBuilder out = new StringBuilder(length);

            out.append(labelString).append(':');
            termReplace(out, breakTarget, statementToReplace, switchStatementString);
            JSElementFactory.replaceStatement(breakTarget, out.toString());
        }
        else {
            JSElementFactory.replaceStatement(statementToReplace, switchStatementString);
        }
    }

    @RequiredReadAction
    private static void termReplace(StringBuilder out, JSElement target, JSElement replace, String stringToReplaceWith) {
        if (target.equals(replace)) {
            out.append(stringToReplaceWith);
        }
        else if (target.getChildren().length != 0) {
            JSElement[] children = (JSElement[])target.getChildren();
            for (JSElement child : children) {
                termReplace(out, child, replace, stringToReplaceWith);
            }
        }
        else {
            String text = target.getText();
            out.append(text);
        }
    }

    @RequiredReadAction
    private static JSExpression[] getValuesFromCondition(JSExpression condition, JSExpression caseExpression) {
        List<JSExpression> values = new ArrayList<>(10);

        getValuesFromExpression(condition, caseExpression, values);
        return values.toArray(new JSExpression[values.size()]);
    }

    @RequiredReadAction
    private static void getValuesFromExpression(JSExpression expression, JSExpression caseExpression, List<JSExpression> values) {
        if (expression instanceof JSBinaryExpression binaryExpression) {
            JSExpression lhs = binaryExpression.getLOperand();
            JSExpression rhs = binaryExpression.getROperand();
            IElementType tokenType = binaryExpression.getOperationSign();

            if (JSTokenTypes.OROR.equals(tokenType)) {
                getValuesFromExpression(lhs, caseExpression, values);
                getValuesFromExpression(rhs, caseExpression, values);
            }
            else if (EquivalenceChecker.expressionsAreEquivalent(caseExpression, rhs)) {
                values.add(lhs);
            }
            else {
                values.add(rhs);
            }
        }
        else if (expression instanceof JSParenthesizedExpression parenExpression) {
            JSExpression contents = parenExpression.getInnerExpression();

            getValuesFromExpression(contents, caseExpression, values);
        }
    }

    @RequiredReadAction
    private static void dumpBranch(
        StringBuilder switchStatementString,
        List<String> labels,
        JSStatement body,
        boolean wrap,
        boolean renameBreaks,
        String breakLabelName
    ) {
        dumpLabels(switchStatementString, labels);
        dumpBody(switchStatementString, body, wrap, renameBreaks, breakLabelName);
    }

    @RequiredReadAction
    private static void dumpDefaultBranch(
        StringBuilder switchStatementString,
        JSStatement body,
        boolean wrap,
        boolean renameBreaks,
        String breakLabelName
    ) {
        switchStatementString.append(DEFAULT_CASE_CLAUSE_EXPRESSION);
        dumpBody(switchStatementString, body, wrap, renameBreaks, breakLabelName);
    }

    private static void dumpLabels(StringBuilder switchStatementString, List<String> labels) {
        for (String label : labels) {
            switchStatementString.append("\ncase ").append(label).append(": ");
        }
    }

    @RequiredReadAction
    private static void dumpBody(
        StringBuilder switchStatementString,
        JSStatement bodyStatement,
        boolean wrap,
        boolean renameBreaks,
        String breakLabelName
    ) {
        if (bodyStatement instanceof JSBlockStatement) {
            if (wrap) {
                appendElement(switchStatementString, bodyStatement, renameBreaks, breakLabelName);
            }
            else {
                // Skip the first and last characters to unwrap the block
                String bodyText = bodyStatement.getText();
                switchStatementString.append(bodyText.substring(1, bodyText.length() - 1));
            }
        }
        else {
            if (wrap) {
                switchStatementString.append('{');
                appendElement(switchStatementString, bodyStatement, renameBreaks, breakLabelName);
                switchStatementString.append('}');
            }
            else {
                appendElement(switchStatementString, bodyStatement, renameBreaks, breakLabelName);
            }
        }
        if (ControlFlowUtils.statementMayCompleteNormally(bodyStatement)) {
            switchStatementString.append(BREAK_STATEMENT);
        }
        switchStatementString.append('\n');
    }

    @RequiredReadAction
    private static void appendElement(
        StringBuilder switchStatementString,
        JSElement element,
        boolean renameBreakElements,
        String breakLabelString
    ) {
        String text = element.getText();

        if (!renameBreakElements) {
            switchStatementString.append(text);
        }
        else if (element instanceof JSBreakStatement breakStatement) {
            String identifier = breakStatement.getLabel();
            if (identifier == null || identifier.isEmpty()) {
                switchStatementString.append(LABELED_BREAK_STATEMENT_PREFIX);
                switchStatementString.append(breakLabelString);
                switchStatementString.append(';');
            }
            else {
                switchStatementString.append(text);
            }
        }
        else if (element instanceof JSBlockStatement || element instanceof JSIfStatement) {
            JSElement[] children = (JSElement[])element.getChildren();
            for (JSElement child : children) {
                appendElement(switchStatementString, child, renameBreakElements, breakLabelString);
            }
        }
        else {
            switchStatementString.append(text);
        }
    }

    private static class IfToSwitchPredicate implements JSElementPredicate {
        @Override
        @RequiredReadAction
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            return element.getParent() instanceof JSIfStatement statement
                && IF_KEYWORD.equals(element.getText())
                && !ErrorUtil.containsError(statement)
                && CaseUtil.getCaseExpression(statement) != null;
        }
    }

    private static class IfStatementBranch {
        private final List<String> conditions = new ArrayList<>(3);
        private Set<String> topLevelVariables = new HashSet<>(3);
        private Set<String> innerVariables = new HashSet<>(3);
        private JSStatement statement;
        private boolean isElse;

        public void addCondition(String conditionString) {
            this.conditions.add(conditionString);
        }

        public void setStatement(JSStatement statement) {
            this.statement = statement;
        }

        public JSStatement getStatement() {
            return this.statement;
        }

        public List<String> getConditions() {
            return Collections.unmodifiableList(this.conditions);
        }

        public boolean isElse() {
            return this.isElse;
        }

        public void setElse() {
            this.isElse = true;
        }

        public void setTopLevelVariables(Set<String> topLevelVariables) {
            this.topLevelVariables = new HashSet<>(topLevelVariables);
        }

        public void setInnerVariables(Set<String> innerVariables) {
            this.innerVariables = new HashSet<>(innerVariables);
        }

        private Set<String> getTopLevelVariables() {
            return Collections.unmodifiableSet(this.topLevelVariables);
        }

        private Set<String> getInnerVariables() {
            return Collections.unmodifiableSet(this.innerVariables);
        }

        public boolean topLevelDeclarationsConfictWith(IfStatementBranch testBranch) {
            Set<String> innerVariables = testBranch.getInnerVariables();
            Set<String> topLevel = testBranch.getTopLevelVariables();

            return hasNonEmptyIntersection(this.topLevelVariables, topLevel)
                || hasNonEmptyIntersection(this.topLevelVariables, innerVariables);
        }

        private static boolean hasNonEmptyIntersection(Set<String> set1, Set<String> set2) {
            for (String set1Element : set1) {
                if (set2.contains(set1Element)) {
                    return true;
                }
            }
            return false;
        }
    }
}