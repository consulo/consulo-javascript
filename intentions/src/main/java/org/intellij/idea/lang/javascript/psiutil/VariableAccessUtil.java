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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.ArrayListSet;
import org.jetbrains.annotations.NotNull;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class VariableAccessUtil {

    private VariableAccessUtil() {}

    public static boolean variableIsAssignedFrom(JSVariable variable,
                                                 JSElement  context) {
        final VariableAssignedFromVisitor visitor = new VariableAssignedFromVisitor(variable);
        context.accept(visitor);
        return visitor.isAssignedFrom();
    }

//    public static boolean variableIsPassedAsMethodArgument(
//            JSVariable variable, JSElement context) {
//        final VariablePassedAsArgumentVisitor visitor =
//                new VariablePassedAsArgumentVisitor(variable);
//        context.accept(visitor);
//        return visitor.isPassed();
//    }

//    public static boolean variableIsUsedInArrayInitializer(
//            JSVariable variable, JSElement context) {
//        final VariableUsedInArrayInitializerVisitor visitor =
//                new VariableUsedInArrayInitializerVisitor(variable);
//        context.accept(visitor);
//        return visitor.isPassed();
//    }

    public static boolean variableIsAssigned(
            JSVariable variable, JSElement context) {
        //noinspection unchecked
        return variableIsAssigned(variable, context, Collections.EMPTY_SET, Collections.EMPTY_SET);
    }

    public static boolean variableIsAssigned(
            JSVariable      variable,
            JSElement       context,
            Set<JSVariable> notUpdatedSymbols) {
        //noinspection unchecked
        return variableIsAssigned(variable, context, notUpdatedSymbols, Collections.EMPTY_SET);
    }

    private static boolean variableIsAssigned(
            JSVariable      variable,
            JSElement       context,
            Set<JSVariable> notUpdatedSymbols,
            Set<JSVariable> candidateSymbols) {
        final VariableAssignedVisitor visitor = new VariableAssignedVisitor(variable, context, notUpdatedSymbols,
                                                                            candidateSymbols);
        context.accept(visitor);
        return visitor.isAssigned();
    }

    public static boolean variableIsReturned(
            JSVariable variable, JSElement context) {
        final VariableReturnedVisitor visitor = new VariableReturnedVisitor(variable);
        context.accept(visitor);
        return visitor.isReturned();
    }

    public static boolean arrayContentsAreAccessed(
            JSVariable variable, JSElement context) {
        final ArrayContentsAccessedVisitor visitor = new ArrayContentsAccessedVisitor(variable);
        context.accept(visitor);
        return visitor.isAccessed();
    }

    public static boolean arrayContentsAreAssigned(
            JSVariable variable, JSElement context) {
        final ArrayContentsAssignedVisitor visitor = new ArrayContentsAssignedVisitor(variable);
        context.accept(visitor);
        return visitor.isAssigned();
    }

    public static boolean mayEvaluateToVariable(JSExpression expression,
                                                JSVariable variable) {
        final String variableName = variable.getName();
        JSExpression expr         = expression;

        if (variableName == null) {
            return false;
        }

        while (true) {
            if (expr == null) {
                return false;
            } else if (expr instanceof JSParenthesizedExpression) {
                expr = ((JSParenthesizedExpression) expr).getInnerExpression();
            } else if (expr instanceof JSConditionalExpression) {
                final JSConditionalExpression conditional = (JSConditionalExpression) expr;

                return (mayEvaluateToVariable(conditional.getThen(), variable) ||
                        mayEvaluateToVariable(conditional.getElse(), variable));
            } else if (expr instanceof JSIndexedPropertyAccessExpression) {
                expr = ((JSIndexedPropertyAccessExpression) expr).getQualifier();
            } else if (expr instanceof JSDefinitionExpression) {
                expr = ((JSDefinitionExpression) expr).getExpression();
            } else if (expr instanceof JSReferenceExpression) {
                final PsiElement referent = ((JSReferenceExpression) expr).resolve();

                return (referent != null && referent.equals(variable));
            } else {
                return false;
            }
        }
    }

    public static boolean variableIsUsed(JSVariable variable,
                                         PsiElement context) {
        return (variable.getName() != null &&
                FindReferenceUtil.getReferences(variable, context).iterator().hasNext());
    }

    public static Set<JSVariable> getUsedVariables(PsiElement context) {
        final UsedVariableVisitor visitor = new UsedVariableVisitor();
        context.accept(visitor);
        return visitor.getVariables();
    }

    private static class VariableAssignedVisitor extends JSRecursiveElementVisitor {
        private final JSVariable      variable;
        private final JSElement       context;
        private       Set<JSVariable> notUpdatedSymbols;
        private final Set<JSVariable> candidateSymbols;
        private       boolean         assigned;

        public VariableAssignedVisitor(@NotNull JSVariable      variable,
                                       @NotNull JSElement       context,
                                       @NotNull Set<JSVariable> notUpdatedSymbols) {
            this.variable          = variable;
            this.context           = context;
            this.notUpdatedSymbols = notUpdatedSymbols;
            this.candidateSymbols  = new ArrayListSet<JSVariable>();

            this.candidateSymbols.add(variable);
        }

        private VariableAssignedVisitor(@NotNull JSVariable      variable,
                                        @NotNull JSElement       context,
                                        @NotNull Set<JSVariable> notUpdatedSymbols,
                                        @NotNull Set<JSVariable> candidateSymbols) {
            this.variable          = variable;
            this.context           = context;
            this.notUpdatedSymbols = notUpdatedSymbols;
            this.candidateSymbols  = candidateSymbols;
        }

        @Override public void visitJSElement(JSElement element) {
            if (!this.assigned) {
                super.visitJSElement(element);
            }
        }

        @Override public void visitJSAssignmentExpression(JSAssignmentExpression assignment) {
            if (!this.assigned) {
                super.visitJSAssignmentExpression(assignment);

                final JSExpression arg = assignment.getLOperand();

                if (VariableAccessUtil.mayEvaluateToVariable(arg, this.variable)) {
                    final Set<JSVariable> usedVariables   = getUsedVariables(assignment.getROperand());
                    final Set<JSVariable> newCandidateSet = new ArrayListSet<JSVariable>();
                    boolean               assigned        = false;

                    newCandidateSet.addAll(this.candidateSymbols);
                    newCandidateSet.add(this.variable);
                    for (JSVariable variable : usedVariables) {
                        if (newCandidateSet.contains(variable) ||
                            variableIsAssigned(variable, this.context, this.notUpdatedSymbols, newCandidateSet)) {
                            assigned = true;
                        } else {
                            if (this.notUpdatedSymbols.isEmpty()) {
                                this.notUpdatedSymbols = new ArrayListSet<JSVariable>();
                            }
                            this.notUpdatedSymbols.add(variable);
                        }
                    }
                    this.assigned = assigned;
                } else {
                    this.assigned = false;
                }
            }
        }

        @Override public void visitJSPrefixExpression(JSPrefixExpression expression) {
            if (!this.assigned) {
                super.visitJSPrefixExpression(expression);

                final IElementType tokenType = expression.getOperationSign();

                if (!(tokenType.equals(JSTokenTypes.PLUSPLUS) ||
                      tokenType.equals(JSTokenTypes.MINUSMINUS))) {
                    return;
                }

                final JSExpression operand = expression.getExpression();

                this.assigned = (VariableAccessUtil.mayEvaluateToVariable(operand, this.variable));
            }
        }

        @Override public void visitJSPostfixExpression(JSPostfixExpression postfixExpression) {
            if (!this.assigned) {
                super.visitJSPostfixExpression(postfixExpression);

                final IElementType tokenType = postfixExpression.getOperationSign();

                if (!(tokenType.equals(JSTokenTypes.PLUSPLUS) ||
                      tokenType.equals(JSTokenTypes.MINUSMINUS))) {
                    return;
                }

                final JSExpression operand = postfixExpression.getExpression();

                this.assigned = VariableAccessUtil.mayEvaluateToVariable(operand, this.variable);
            }
        }

        public boolean isAssigned() {
            return this.assigned;
        }
    }

    private static class VariableAssignedFromVisitor extends JSRecursiveElementVisitor {
        private final JSVariable variable;
        private       boolean    assignedFrom;

        public VariableAssignedFromVisitor(@NotNull JSVariable variable) {
            this.variable = variable;
        }

        @Override public void visitJSElement(@NotNull JSElement element) {
            if (!this.assignedFrom) {
                super.visitJSElement(element);
            }
        }

        @Override public void visitJSAssignmentExpression(@NotNull JSAssignmentExpression assignment) {
            if (!this.assignedFrom) {
                super.visitJSAssignmentExpression(assignment);

                this.assignedFrom = VariableAccessUtil.mayEvaluateToVariable(assignment.getROperand(), this.variable);
            }
        }

        @Override public void visitJSVarStatement(@NotNull JSVarStatement statement) {
            if (!this.assignedFrom) {
                super.visitJSVarStatement(statement);

                for (JSVariable declaredVariable : statement.getVariables()) {
                    final JSExpression initializer = declaredVariable.getInitializer();

                    if (initializer != null) {
                        this.assignedFrom = VariableAccessUtil.mayEvaluateToVariable(initializer, this.variable);
                    }
                }
            }
        }

        @Override public void visitJSVariable(JSVariable var) {
            if (!this.assignedFrom) {
                super.visitJSVariable(var);
                this.assignedFrom = (VariableAccessUtil.mayEvaluateToVariable(var.getInitializer(), this.variable));
            }
        }

        public boolean isAssignedFrom() {
            return this.assignedFrom;
        }
    }

    private static class VariableReturnedVisitor extends JSRecursiveElementVisitor {
        @NotNull private final JSVariable variable;
        private                boolean    returned;

        public VariableReturnedVisitor(@NotNull JSVariable variable) {
            this.variable = variable;
        }

        @Override public void visitJSReturnStatement(@NotNull JSReturnStatement returnStatement){
            if (!this.returned) {
                super.visitJSReturnStatement(returnStatement);
                this.returned = VariableAccessUtil.mayEvaluateToVariable(returnStatement.getExpression(), this.variable);
            }
        }

        public boolean isReturned() {
            return this.returned;
        }
    }

    private static class ArrayContentsAccessedVisitor extends JSRecursiveElementVisitor {
        private final JSVariable variable;
        private       boolean    accessed;

        public ArrayContentsAccessedVisitor(@NotNull JSVariable variable) {
            this.variable = variable;
        }

        @Override public void visitJSForInStatement(@NotNull JSForInStatement statement) {
            if (!this.accessed) {
                super.visitJSForInStatement(statement);
                this.checkQualifier(statement.getCollectionExpression());
            }
        }

        @Override public void visitJSIndexedPropertyAccessExpression(@NotNull JSIndexedPropertyAccessExpression accessExpression) {
            if (!this.accessed) {
                super.visitJSIndexedPropertyAccessExpression(accessExpression);

                final PsiElement parent = accessExpression.getParent();

                if (!(parent instanceof JSAssignmentExpression ||
                      ((JSAssignmentExpression) parent).getLOperand().equals(accessExpression))) {
                    this.checkQualifier(accessExpression.getQualifier());
                }
            }
        }

        private void checkQualifier(JSExpression qualifier) {
            if (qualifier instanceof JSReferenceExpression) {
                final PsiElement referent = ((JSReferenceExpression) qualifier).resolve();

                this.accessed = (referent != null && referent.equals(this.variable));
            }
        }

        public boolean isAccessed() {
            return this.accessed;
        }
    }

    private static class ArrayContentsAssignedVisitor extends JSRecursiveElementVisitor {
        private final JSVariable  variable;
        private       boolean     assigned;

        public ArrayContentsAssignedVisitor(@NotNull JSVariable variable) {
            this.variable = variable;
        }

        @Override public void visitJSAssignmentExpression(@NotNull JSAssignmentExpression assignment) {
            if (!this.assigned) {
                super.visitJSAssignmentExpression(assignment);
                this.checkExpression(null, assignment.getLOperand());
            }
        }

        @Override public void visitJSPrefixExpression(@NotNull JSPrefixExpression expression) {
            if (!this.assigned) {
                super.visitJSPrefixExpression(expression);
                this.checkExpression(expression.getOperationSign(), expression.getExpression());
            }
        }

        @Override public void visitJSPostfixExpression(@NotNull JSPostfixExpression expression) {
            if (!this.assigned) {
                super.visitJSPostfixExpression(expression);
                this.checkExpression(expression.getOperationSign(), expression.getExpression());
            }
        }

        private void checkExpression(IElementType tokenType, JSExpression expression) {
            if (!(tokenType == null ||
                  tokenType.equals(JSTokenTypes.PLUSPLUS)   ||
                  tokenType.equals(JSTokenTypes.MINUSMINUS))) {
                return;
            }
            if (!(expression instanceof JSIndexedPropertyAccessExpression)) {
                return;
            }

            JSExpression arrayExpression = ((JSIndexedPropertyAccessExpression) expression).getQualifier();

            while (arrayExpression instanceof JSIndexedPropertyAccessExpression) {
                final JSIndexedPropertyAccessExpression accessExpression = (JSIndexedPropertyAccessExpression) arrayExpression;

                arrayExpression = accessExpression.getQualifier();
            }

            if (arrayExpression instanceof JSReferenceExpression) {
                final String referencedName = ((JSReferenceExpression) arrayExpression).getReferencedName();
              

              // TODO maybe it's better to check ((JSReferenceExpression) arrayExpression).isReferenceTo(variable) ?
              this.assigned = referencedName != null && referencedName.equals(this.variable.getName());
            }
        }

        public boolean isAssigned() {
            return this.assigned;
        }
    }

    private static class UsedVariableVisitor extends JSRecursiveElementVisitor {

        @NotNull private final Set<JSVariable> variables;

        public UsedVariableVisitor() {
            this.variables = new HashSet<JSVariable>();
        }

        @Override public void visitJSReferenceExpression(@NotNull JSReferenceExpression ref) {
            super.visitJSReferenceExpression(ref);

            final PsiElement referent = ref.resolve();

            if (referent != null && referent instanceof JSVariable) {
                this.variables.add((JSVariable) referent);
            }
        }

        public Set<JSVariable> getVariables() {
            return this.variables;
        }
    }
}
