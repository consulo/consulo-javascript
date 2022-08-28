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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.Nonnull;

import consulo.language.ast.IElementType;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ControlFlowUtils;
import org.intellij.idea.lang.javascript.psiutil.DeclarationUtils;
import org.intellij.idea.lang.javascript.psiutil.EquivalenceChecker;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NonNls;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;

public class JSReplaceIfWithSwitchIntention extends JSIntention {
    @NonNls private static final String IF_KEYWORD                     = "if";
    @NonNls private static final String DEFAULT_LABEL_NAME             = "Label";
    @NonNls private static final String SWITCH_STATEMENT_PREFIX        = "switch(";
    @NonNls private static final String DEFAULT_CASE_CLAUSE_EXPRESSION = "default: ";
    @NonNls private static final String CASE_EXPRESSION_PREFIX         = "\ncase ";
    @NonNls private static final String LABELED_BREAK_STATEMENT_PREFIX = "break ";
    @NonNls private static final String BREAK_STATEMENT                = "\nbreak;";

    @Override
	@Nonnull
    public JSElementPredicate getElementPredicate() {
        return new IfToSwitchPredicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element)
            throws IncorrectOperationException {
        JSIfStatement ifStatement = (JSIfStatement) element.getParent();

        assert (ifStatement != null);

        boolean     breaksNeedRelabeled = false;
        JSStatement breakTarget         = null;
        String      labelString         = "";

        if (ControlFlowUtils.statementContainsExitingBreak(ifStatement)) {
            JSElement ancestor = (JSElement) ifStatement.getParent();
            while (ancestor != null) {
                if (ancestor instanceof JSForStatement     ||
                    ancestor instanceof JSForInStatement   ||
                    ancestor instanceof JSDoWhileStatement ||
                    ancestor instanceof JSWhileStatement   ||
                    ancestor instanceof JSSwitchStatement) {
                    breakTarget = (JSStatement) ancestor;
                    break;
                }
                ancestor = (JSElement) ancestor.getParent();
            }
            if (breakTarget != null) {
                labelString         = CaseUtil.findUniqueLabel(ifStatement, DEFAULT_LABEL_NAME);
                breaksNeedRelabeled = true;
            }
        }
        final JSIfStatement statementToReplace = ifStatement;
        final JSExpression  caseExpression     = CaseUtil.getCaseExpression(ifStatement);

        assert (caseExpression != null);

        final StringBuilder switchStatementBuffer = new StringBuilder(1024);

        switchStatementBuffer.append(SWITCH_STATEMENT_PREFIX)
                             .append(caseExpression.getText())
                             .append(')')
                             .append('{');

        final List<IfStatementBranch> branches = new ArrayList<IfStatementBranch>(20);

        while (true) {
            final Set<String>    topLevelVariables = new HashSet<String>(5);
            final Set<String>    innerVariables    = new HashSet<String>(5);
            final JSExpression   condition         = ifStatement.getCondition();
            final JSExpression[] labels            = getValuesFromCondition(condition, caseExpression);
            final JSStatement    thenBranch        = ifStatement.getThen();

            DeclarationUtils.calculateVariablesDeclared(thenBranch,
                                                        topLevelVariables,
                                                        innerVariables,
                                                        true);

            final IfStatementBranch ifBranch = new IfStatementBranch();

            ifBranch.setInnerVariables(innerVariables);
            ifBranch.setTopLevelVariables(topLevelVariables);
            ifBranch.setStatement(thenBranch);
            for (final JSExpression label : labels) {
                ifBranch.addCondition(label.getText());
            }
            branches.add(ifBranch);

            final JSStatement elseBranch = ifStatement.getElse();

            if (elseBranch instanceof JSIfStatement) {
                ifStatement = (JSIfStatement) elseBranch;
            } else if (elseBranch == null) {
                break;
            } else {
                final Set<String> elseTopLevelVariables = new HashSet<String>(5);
                final Set<String> elseInnerVariables    = new HashSet<String>(5);

                DeclarationUtils.calculateVariablesDeclared(elseBranch, elseTopLevelVariables,
                                                            elseInnerVariables, true);

                final IfStatementBranch elseIfBranch = new IfStatementBranch();

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

            final JSStatement branchStatement = (JSStatement) branch.getStatement();

            if (branch.isElse()) {
                dumpDefaultBranch(switchStatementBuffer, branchStatement,
                                  hasConflicts, breaksNeedRelabeled, labelString);
            } else {
                final List<String> conditions = branch.getConditions();

                dumpBranch(switchStatementBuffer, conditions, branchStatement,
                           hasConflicts, breaksNeedRelabeled, labelString);
            }
        }

        switchStatementBuffer.append('}');

        final String switchStatementString = switchStatementBuffer.toString();

        if (breaksNeedRelabeled) {
            final int           length = switchStatementBuffer.length();
            final StringBuilder out    = new StringBuilder(length);

            out.append(labelString)
               .append(':');
            termReplace(out, breakTarget, statementToReplace, switchStatementString);
            JSElementFactory.replaceStatement(breakTarget, out.toString());
        } else {
            JSElementFactory.replaceStatement(statementToReplace, switchStatementString);
        }
    }

    private static void termReplace(StringBuilder out,
                                    JSElement     target,
                                    JSElement     replace,
                                    String        stringToReplaceWith) {
        if (target.equals(replace)) {
            out.append(stringToReplaceWith);
        } else if (target.getChildren().length != 0) {
            final JSElement[] children = (JSElement[]) target.getChildren();
            for (final JSElement child : children) {
                termReplace(out, child, replace, stringToReplaceWith);
            }
        } else {
            final String text = target.getText();
            out.append(text);
        }
    }

    private static JSExpression[] getValuesFromCondition(JSExpression condition,
                                                         JSExpression caseExpression) {
        final List<JSExpression> values = new ArrayList<JSExpression>(10);

        getValuesFromExpression(condition, caseExpression, values);
        return values.toArray(new JSExpression[values.size()]);
    }

    private static void getValuesFromExpression(JSExpression       expression,
                                                JSExpression       caseExpression,
                                                List<JSExpression> values) {
        if (expression instanceof JSBinaryExpression) {
            final JSBinaryExpression binaryExpression =
                    (JSBinaryExpression) expression;
            final JSExpression       lhs              = binaryExpression.getLOperand();
            final JSExpression       rhs              = binaryExpression.getROperand();
            final IElementType tokenType        = binaryExpression.getOperationSign();

            if (JSTokenTypes.OROR.equals(tokenType)) {
                getValuesFromExpression(lhs, caseExpression, values);
                getValuesFromExpression(rhs, caseExpression, values);
            } else {
                if (EquivalenceChecker.expressionsAreEquivalent(caseExpression, rhs)) {
                    values.add(lhs);
                } else {
                    values.add(rhs);
                }
            }
        } else if (expression instanceof JSParenthesizedExpression) {
            final JSParenthesizedExpression parenExpression = (JSParenthesizedExpression) expression;
            final JSExpression              contents        = parenExpression.getInnerExpression();

            getValuesFromExpression(contents, caseExpression, values);
        }
    }

    private static void dumpBranch(StringBuilder switchStatementString,
                                   List<String>  labels,
                                   JSStatement   body,
                                   boolean       wrap,
                                   boolean       renameBreaks,
                                   String        breakLabelName) {
        dumpLabels(switchStatementString, labels);
        dumpBody  (switchStatementString, body, wrap, renameBreaks, breakLabelName);
    }

    private static void dumpDefaultBranch(StringBuilder switchStatementString,
                                          JSStatement   body,
                                          boolean       wrap,
                                          boolean       renameBreaks,
                                          String        breakLabelName) {
        switchStatementString.append(DEFAULT_CASE_CLAUSE_EXPRESSION);
        dumpBody(switchStatementString, body, wrap, renameBreaks, breakLabelName);
    }

    private static void dumpLabels(StringBuilder switchStatementString,
                                   List<String>  labels) {
        for (String label : labels) {
            switchStatementString.append(CASE_EXPRESSION_PREFIX);
            switchStatementString.append(label);
            switchStatementString.append(": ");
        }
    }

    private static void dumpBody(StringBuilder switchStatementString,
                                 JSStatement   bodyStatement,
                                 boolean       wrap,
                                 boolean       renameBreaks,
                                 String        breakLabelName) {
        if (bodyStatement instanceof JSBlockStatement) {
            if (wrap) {
                appendElement(switchStatementString, bodyStatement,
                              renameBreaks, breakLabelName);
            } else {
                // Skip the first and last characters to unwrap the block
                final String bodyText = bodyStatement.getText();
                switchStatementString.append(bodyText.substring(1, bodyText.length() - 1));
            }
        } else {
            if (wrap) {
                switchStatementString.append('{');
                appendElement(switchStatementString, bodyStatement,
                              renameBreaks, breakLabelName);
                switchStatementString.append('}');
            } else {
                appendElement(switchStatementString, bodyStatement,
                              renameBreaks, breakLabelName);
            }
        }
        if (ControlFlowUtils.statementMayCompleteNormally(bodyStatement)) {
            switchStatementString.append(BREAK_STATEMENT);
        }
        switchStatementString.append('\n');
    }

    private static void appendElement(StringBuilder switchStatementString,
                                      JSElement     element,
                                      boolean       renameBreakElements,
                                      String        breakLabelString) {
        final String text = element.getText();

        if (!renameBreakElements) {
            switchStatementString.append(text);
        } else if (element instanceof JSBreakStatement) {
            final String identifier = ((JSBreakStatement) element).getLabel();
            if (identifier == null || identifier.length() == 0) {
                switchStatementString.append(LABELED_BREAK_STATEMENT_PREFIX);
                switchStatementString.append(breakLabelString);
                switchStatementString.append(';');
            } else {
                switchStatementString.append(text);
            }
        } else if (element instanceof JSBlockStatement ||
                   element instanceof JSIfStatement) {
            final JSElement[] children = (JSElement[]) element.getChildren();
            for (final JSElement child : children) {
                appendElement(switchStatementString, child, renameBreakElements,
                              breakLabelString);
            }
        } else {
            switchStatementString.append(text);
        }
    }

    private static class IfToSwitchPredicate implements JSElementPredicate {

        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            final PsiElement parent = element.getParent();
            if (!(parent instanceof JSIfStatement)) {
                return false;
            }
            final String text = element.getText();
            if (!IF_KEYWORD.equals(text)) {
                return false;
            }
            final JSIfStatement statement = (JSIfStatement)parent;
            if (ErrorUtil.containsError(statement)) {
                return false;
            }
            return (CaseUtil.getCaseExpression(statement) != null);
        }
    }

    private static class IfStatementBranch {
        private final List<String> conditions        = new ArrayList<String>(3);
        private       Set<String>  topLevelVariables = new HashSet<String>(3);
        private       Set<String>  innerVariables    = new HashSet<String>(3);
        private       JSStatement statement;
        private       boolean      isElse;

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

        public void setTopLevelVariables(Set<String> topLevelVariables){
            this.topLevelVariables = new HashSet<String>(topLevelVariables);
        }

        public void setInnerVariables(Set<String> innerVariables){
            this.innerVariables = new HashSet<String>(innerVariables);
        }

        private Set<String> getTopLevelVariables(){
            return Collections.unmodifiableSet(this.topLevelVariables);
        }

        private Set<String> getInnerVariables(){
            return Collections.unmodifiableSet(this.innerVariables);
        }

        public boolean topLevelDeclarationsConfictWith(IfStatementBranch testBranch){
            final Set<String> innerVariables = testBranch.getInnerVariables();
            final Set<String> topLevel       = testBranch.getTopLevelVariables();

            return (hasNonEmptyIntersection(this.topLevelVariables, topLevel) ||
                    hasNonEmptyIntersection(this.topLevelVariables, innerVariables));
        }

        private static boolean hasNonEmptyIntersection(Set<String> set1,
                                                       Set<String> set2) {
            for (final String set1Element : set1) {
                if (set2.contains(set1Element)) {
                    return true;
                }
            }
            return false;
        }
    }
}