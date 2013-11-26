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
package org.intellij.idea.lang.javascript.psiutil;

import com.intellij.lang.javascript.psi.*;

import java.util.HashSet;
import java.util.Set;

public class DeclarationUtils {
    private DeclarationUtils() {}

    public static void calculateVariablesDeclared(JSStatement statement,
                                                  Set<String> variablesDeclaredAtTopLevel,
                                                  Set<String> variablesDeclaredAtLowerLevels,
                                                  boolean     isTopLevel) {
        if (statement == null) {
            return;
        }

        if (statement instanceof JSBreakStatement      ||
            statement instanceof JSExpressionStatement ||
            statement instanceof JSContinueStatement   ||
            statement instanceof JSThrowStatement      ||
            statement instanceof JSReturnStatement) {
            // Nothing to do.
        } else if (statement instanceof JSVarStatement) {
            calculateVariablesDeclared((JSVarStatement) statement, variablesDeclaredAtTopLevel,
                                       variablesDeclaredAtLowerLevels, isTopLevel);
        } else if (statement instanceof JSForStatement) {
            final JSForStatement loopStatement = (JSForStatement) statement;

            calculateVariablesDeclared(loopStatement.getVarDeclaration(), variablesDeclaredAtTopLevel,
                                       variablesDeclaredAtLowerLevels, false);
            calculateVariablesDeclared(loopStatement.getBody(),           variablesDeclaredAtTopLevel,
                                       variablesDeclaredAtLowerLevels, false);
        } else if (statement instanceof JSWhileStatement) {
            final JSWhileStatement loopStatement = (JSWhileStatement) statement;

            calculateVariablesDeclared(loopStatement.getBody(), variablesDeclaredAtTopLevel,
                                       variablesDeclaredAtLowerLevels, false);
        } else if (statement instanceof JSDoWhileStatement) {
            final JSDoWhileStatement loopStatement = (JSDoWhileStatement) statement;

            calculateVariablesDeclared(loopStatement.getBody(), variablesDeclaredAtTopLevel,
                                       variablesDeclaredAtLowerLevels, false);
        } else if (statement instanceof JSBlockStatement) {
            final JSBlockStatement block = (JSBlockStatement) statement;

            calculateVariablesDeclared(block.getStatements(), variablesDeclaredAtTopLevel,
                                       variablesDeclaredAtLowerLevels, false);
        } else if (statement instanceof JSLabeledStatement) {
            final JSLabeledStatement labeledStatement = (JSLabeledStatement) statement;

            calculateVariablesDeclared(labeledStatement.getStatement(), variablesDeclaredAtTopLevel,
                                       variablesDeclaredAtLowerLevels, false);
        } else if (statement instanceof JSIfStatement) {
            final JSIfStatement ifStatement = (JSIfStatement) statement;
            final JSStatement   thenBranch  = ifStatement.getThen();
            final JSStatement   elseBranch  = ifStatement.getElse();

            calculateVariablesDeclared(thenBranch, variablesDeclaredAtTopLevel,
                                       variablesDeclaredAtLowerLevels, false);
            calculateVariablesDeclared(elseBranch, variablesDeclaredAtTopLevel,
                                       variablesDeclaredAtLowerLevels, false);
        } else if (statement instanceof JSTryStatement) {
            final JSTryStatement tryStatement = (JSTryStatement) statement;

            calculateVariablesDeclared(tryStatement.getStatement(),
                                       variablesDeclaredAtTopLevel,
                                       variablesDeclaredAtLowerLevels,
                                       false);
            calculateVariablesDeclared(tryStatement.getFinallyStatement(),
                                       variablesDeclaredAtTopLevel,
                                       variablesDeclaredAtLowerLevels,
                                       false);

            final JSCatchBlock catchBlock = tryStatement.getCatchBlock();

            if (catchBlock != null) {
                calculateVariablesDeclared(catchBlock.getStatement(),
                                           variablesDeclaredAtTopLevel,
                                           variablesDeclaredAtLowerLevels,
                                           false);
            }
        } else if (statement instanceof JSSwitchStatement) {
            final JSSwitchStatement switchStatement = (JSSwitchStatement) statement;

            for (JSCaseClause caseClause : switchStatement.getCaseClauses()) {
                calculateVariablesDeclared(caseClause.getStatements(), variablesDeclaredAtTopLevel,
                                           variablesDeclaredAtLowerLevels, false);
            }
        }
    }

    private static void calculateVariablesDeclared(JSVarStatement statement,
                                                   Set<String>    variablesDeclaredAtTopLevel,
                                                   Set<String>    variablesDeclaredAtLowerLevels,
                                                   boolean        isTopLevel) {
        for (JSVariable variable : statement.getVariables()) {
            final String variableName = variable.getName();

            if (isTopLevel) {
                variablesDeclaredAtTopLevel.add(variableName);
            } else {
                variablesDeclaredAtLowerLevels.add(variableName);
            }
        }
    }

    private static void calculateVariablesDeclared(JSStatement[] statements,
                                                   Set<String>   variablesDeclaredAtTopLevel,
                                                   Set<String>   variablesDeclaredAtLowerLevels,
                                                   boolean       isTopLevel) {
        if (statements != null) {
            for (JSStatement statement : statements) {
                calculateVariablesDeclared(statement,
                                           variablesDeclaredAtTopLevel,
                                           variablesDeclaredAtLowerLevels,
                                           isTopLevel);
            }
        }
    }

    public static class DeclarationConflictVisitor extends JSRecursiveElementVisitor {
        private final Set<String> declarations;
        private       boolean     hasConflict;

        public DeclarationConflictVisitor(Set<String> declarations) {
            this.declarations = new HashSet<String>(declarations);
        }

        @Override public void visitJSVariable(JSVariable variable) {
            super.visitJSVariable(variable);

            final String name = variable.getName();

            for (String declaration : this.declarations) {
                if (declaration.equals(name)) {
                    this.hasConflict = true;
                    break;
                }
            }
        }

        public boolean hasConflict() {
            return this.hasConflict;
        }
    }
}
