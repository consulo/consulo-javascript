/*
 * Copyright 2000-2005 JetBrains s.r.o.
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

package com.intellij.lang.javascript.index;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nullable;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author maxim.mossienko, yole
 */
public class JSSymbolUtil {
    private static final String PROTOTYPE_FIELD_NAME = "prototype";
    private static final String J_QUERY_VAR_NAME = "jQuery";
    private static final String FN_FUN_NAME = "fn";

    private static JSElement findNameComponent(JSElement expr) {
        if (expr instanceof JSReferenceExpression) {
            return expr;
        }
        JSElement current = expr;

        while (expr != null) {
            if (expr instanceof JSReferenceExpression) {
                return expr;
            }
            else if (expr instanceof JSAssignmentExpression assignment) {
                JSExpression _lOperand = assignment.getLOperand();
                if (!(_lOperand instanceof JSDefinitionExpression)) {
                    break;
                }
                JSExpression lOperand = ((JSDefinitionExpression)_lOperand).getExpression();

                if (lOperand instanceof JSReferenceExpression refExpr) {
                    expr = refExpr;
                    continue;
                }
                else {
                    break;
                }
            }
            else if (expr instanceof JSVariable variable) {
                return variable;
            }
            else if (expr instanceof JSCallExpression call) {
                if (call.getMethodExpression() instanceof JSReferenceExpression methodRefExpr) {
                    return methodRefExpr;
                }
            }
            else {
                current = expr;
            }

            if (current != null) {
                PsiElement parent = current.getParent();
                if (!(parent instanceof JSElement)) {
                    break;
                }
                if (parent instanceof JSStatement) {
                    break;
                }
                expr = (JSElement)parent;
            }
        }

        return null;
    }

    public static String[] buildNameIndexArray(JSElement _expr) {
        final List<String> nameComponents = new ArrayList<>();

        JSElement nameComponent = findNameComponent(_expr);
        JSReferenceExpression expr = null;

        if (nameComponent instanceof JSVariable variable) {
            String varName = variable.getName();
            if (varName != null) {
                nameComponents.add(varName);
            }
        }
        else if (nameComponent instanceof JSReferenceExpression nameRefExpr) {
            expr = nameRefExpr;
        }

        if (expr != null) {
            final JSReferenceExpression expr1 = expr;
            visitReferenceExpressionComponentsInRootFirstOrder(
                expr,
                new ReferenceExpressionProcessor() {
                    @Override
                    public void processExpression(JSReferenceExpression expr) {
                        nameComponents.add(expr.getReferencedName());
                    }

                    @Override
                    public void processUnresolvedThis() {
                        nameComponents.add("");
                    }

                    @Override
                    public boolean isTopLevel(JSReferenceExpression expression) {
                        return expr1 == expression;
                    }
                }
            );
        }

        return nameComponents.toArray(new String[nameComponents.size()]);
    }

    interface ReferenceExpressionProcessor {
        void processExpression(JSReferenceExpression expr);

        void processUnresolvedThis();

        boolean isTopLevel(JSReferenceExpression expression);
    }

    private static void visitReferenceExpressionComponentsInRootFirstOrder(
        JSReferenceExpression expr,
        ReferenceExpressionProcessor processor
    ) {
        JSExpression qualifier = expr.getQualifier();

        if (qualifier instanceof JSCallExpression callExpression) {
            qualifier = callExpression.getMethodExpression();
        }

        if (qualifier instanceof JSIndexedPropertyAccessExpression indexedPropertyAccessExpression) {
            qualifier = indexedPropertyAccessExpression.getQualifier();
        }

        if (qualifier instanceof JSReferenceExpression referenceExpression) {
            visitReferenceExpressionComponentsInRootFirstOrder(referenceExpression, processor);
        }

        if (qualifier instanceof JSThisExpression) {
            processor.processUnresolvedThis();
        }

        String refName = expr.getReferencedName();

        if (refName != null && (!refName.equals(PROTOTYPE_FIELD_NAME) || processor.isTopLevel(expr))) {
            processor.processExpression(expr);
        }
    }

    private static JSReferenceExpression evaluateInitializedPrototype(JSExpression initializer) {
        JSReferenceExpression initializedPrototype = null;

        if (initializer instanceof JSReferenceExpression initializerRefExpr
            && PROTOTYPE_FIELD_NAME.equals(initializerRefExpr.getReferencedName())
            && initializerRefExpr.getQualifier() instanceof JSReferenceExpression qualifierRefExpr) {
            initializedPrototype = qualifierRefExpr;
        }
        else if (initializer instanceof JSAssignmentExpression initializerAssignmentExpr
            && initializerAssignmentExpr.getLOperand() instanceof JSDefinitionExpression lOperandDefExpr) {
            initializedPrototype = evaluateInitializedPrototype(lOperandDefExpr.getExpression());
        }
        return initializedPrototype;
    }

    public static JSReferenceExpression findReferenceExpressionUsedForClassExtending(JSReferenceExpression lOperand) {
        return findReferenceExpressionUsedForClassExtending(lOperand, null);
    }

    private static JSReferenceExpression findReferenceExpressionUsedForClassExtending(
        JSReferenceExpression lOperand,
        @Nullable Set<String> visited
    ) {
        JSReferenceExpression originalExpr = lOperand;
        ResolveProcessor processor = new ResolveProcessor(lOperand.getText(), true);
        processor.setLocalResolve(true);
        PsiElement parent = lOperand.getParent();
        JSResolveUtil.treeWalkUp(processor, lOperand, parent, lOperand);

        PsiElement jsElement = processor.getResult();
        if (jsElement == null && parent instanceof JSDefinitionExpression parentDefExpr) {
            jsElement = parentDefExpr;
        }

        if (jsElement instanceof JSVariable variable) {
            JSExpression initialization = variable.getInitializer();
            JSReferenceExpression expression = initialization != null ? evaluateInitializedPrototype(initialization) : null;

            if (expression != null) {
                lOperand = expression;
            }
            else if (initialization instanceof JSReferenceExpression initializationRefExpr) {
                lOperand = initializationRefExpr;
            }
        }
        else {
            PsiElement parentJsElement = jsElement != null ? jsElement.getParent() : null;

            // new expression also could mean something extension !
            if (jsElement instanceof JSDefinitionExpression
                && jsElement.getParent() instanceof JSAssignmentExpression parentAssignExpr) {
                JSExpression rOperand = parentAssignExpr.getROperand();

                if (rOperand instanceof JSCallExpression callExpression && !(rOperand instanceof JSNewExpression)) {
                    JSArgumentList list = callExpression.getArgumentList();

                    if (list != null) {
                        JSExpression[] jsExpressions = list.getArguments();

                        if (jsExpressions.length >= 2
                            && jsExpressions[0] instanceof JSReferenceExpression
                            && jsExpressions[1] instanceof JSReferenceExpression) {
                            lOperand = (JSReferenceExpression)jsExpressions[0];
                        }
                    }
                }
                else if (rOperand instanceof JSReferenceExpression rOperandRefExpr) {
                    JSReferenceExpression expression = evaluateInitializedPrototype(rOperand);
                    lOperand = expression != null ? expression : rOperandRefExpr;
                }
            }
        }
        return lOperand != originalExpr ? replaceLocalVars(lOperand, visited) : lOperand;
    }

    private static JSReferenceExpression replaceLocalVars(JSReferenceExpression expression, @Nullable Set<String> visited) {
        JSReferenceExpression expr = expression;
        JSExpression qualifier = expr.getQualifier();

        JSFunction func = PsiTreeUtil.getParentOfType(expression, JSFunction.class);
        if (func == null) {
            return expression;
        }

        while (qualifier instanceof JSReferenceExpression qualifierRefExpr) {
            expr = qualifierRefExpr;
            qualifier = expr.getQualifier();
        }

        if (qualifier == null) {
            PsiElement ref = JSResolveUtil.getLocalVariableRef(func, expr);

            if (ref instanceof JSVariable variable && !(ref instanceof JSParameter)) {
                JSExpression initializer = variable.getInitializer();

                if (initializer instanceof JSReferenceExpression initializerRefExpr) {
                    return replaceExpression(expression, expr, initializerRefExpr);
                }
                else if (expr != expression) {
                    if (visited == null) {
                        visited = new HashSet<>();
                    }
                    String replaced = expr.getText();

                    if (!visited.contains(replaced)) {
                        visited.add(replaced);
                        return replaceExpression(expression, expr, findReferenceExpressionUsedForClassExtending(expr, visited));
                    }
                }
                else {
                    return findReferenceExpressionUsedForClassExtending(expr, visited);
                }
            }
        }
        return expression;
    }

    private static JSReferenceExpression replaceExpression(
        JSReferenceExpression expression,
        JSReferenceExpression what,
        JSReferenceExpression by
    ) {
        if (expression == what) {
            return by;
        }
        int offsetOfExprInExpression = what.getTextOffset() - expression.getTextOffset();
        JSReferenceExpression copyOfExpr = (JSReferenceExpression)expression.copy();
        JSReferenceExpression expressionToReplace = PsiTreeUtil.getParentOfType(
            copyOfExpr.findElementAt(offsetOfExprInExpression),
            JSReferenceExpression.class
        );
        expressionToReplace.replace(by);
        return copyOfExpr;
    }

    public static JSElement findQualifyingExpressionFromArgumentList(JSArgumentList parent) {
        PsiElement firstParent = parent.getParent();
        PsiElement grandParent = firstParent.getParent();

        if (grandParent instanceof JSVariable variable) {
            return variable;
        }

        if (grandParent instanceof JSAssignmentExpression assignExpr) {
            JSExpression jsExpression = assignExpr.getLOperand();
            JSExpression assignedTo = jsExpression instanceof JSDefinitionExpression defExpr ? defExpr.getExpression() : null;
            if (assignedTo instanceof JSReferenceExpression) {
                return assignedTo;
            }
        }
        if (grandParent instanceof JSExpressionStatement
            && firstParent instanceof JSCallExpression call
            && call.getMethodExpression() instanceof JSReferenceExpression methodRefExpr) {

            String methodName = methodRefExpr.getReferencedName();

            if ("each".equals(methodName) || "extend".equals(methodName)) {
                JSExpression expression = methodRefExpr.getQualifier();

                if (expression instanceof JSReferenceExpression qualifierRefExpr
                    && FN_FUN_NAME.equals(qualifierRefExpr.getReferencedName())) {
                    expression = qualifierRefExpr.getQualifier();
                }

                if (expression != null && J_QUERY_VAR_NAME.equals(expression.getText())) {
                    return expression;
                }
            }
            else if ("implement".equals(methodName)) {
                if (methodRefExpr.getQualifier() instanceof JSReferenceExpression qualifierRefExpr && parent.getArguments().length == 1) {
                    return qualifierRefExpr;
                }
            }
        }

        JSExpression[] jsExpressions = parent.getArguments();
        for (int i = 0; i < jsExpressions.length; ++i) {
            JSExpression expr = jsExpressions[i];

            if (expr instanceof JSReferenceExpression
                || (expr instanceof JSLiteralExpression && !expr.textContains(' ') && expr.getTextLength() < 100)) {
                return expr;
            }
            else if (expr instanceof JSCallExpression call) {
                JSArgumentList argumentList = call.getArgumentList();
                if (argumentList != null) {
                    jsExpressions = argumentList.getArguments();
                    i = -1;
                }
            }
            else if (expr instanceof JSArrayLiteralExpression arrayLiteral) {
                jsExpressions = arrayLiteral.getExpressions();
                i = -1;
            }
        }
        return null;
    }
}
