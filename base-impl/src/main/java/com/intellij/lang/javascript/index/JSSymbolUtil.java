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
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @by maxim.mossienko, yole
 */
public class JSSymbolUtil
{
	@NonNls
	private static final String PROTOTYPE_FIELD_NAME = "prototype";
	@NonNls
	private static final String J_QUERY_VAR_NAME = "jQuery";
	@NonNls
	private static final String FN_FUN_NAME = "fn";


	private static JSElement findNameComponent(JSElement expr)
	{
		if(expr instanceof JSReferenceExpression)
		{
			return expr;
		}
		JSElement current = expr;

		while(expr != null)
		{
			if(expr instanceof JSReferenceExpression)
			{
				return expr;
			}
			else if(expr instanceof JSAssignmentExpression)
			{
				final JSExpression _lOperand = ((JSAssignmentExpression) expr).getLOperand();
				if(!(_lOperand instanceof JSDefinitionExpression))
				{
					break;
				}
				final JSExpression lOperand = ((JSDefinitionExpression) _lOperand).getExpression();

				if(lOperand instanceof JSReferenceExpression)
				{
					expr = lOperand;
					continue;
				}
				else
				{
					break;
				}
			}
			else if(expr instanceof JSVariable)
			{
				return expr;
			}
			else if(expr instanceof JSCallExpression)
			{
				final JSExpression method = ((JSCallExpression) expr).getMethodExpression();
				if(method instanceof JSReferenceExpression)
				{
					return method;
				}
			}
			else
			{
				current = expr;
			}

			if(current != null)
			{
				final PsiElement parent = current.getParent();
				if(!(parent instanceof JSElement))
				{
					break;
				}
				if(parent instanceof JSStatement)
				{
					break;
				}
				expr = (JSElement) parent;
			}
		}

		return null;
	}

	public static String[] buildNameIndexArray(final JSElement _expr)
	{
		final List<String> nameComponents = new ArrayList<String>();

		JSElement nameComponent = findNameComponent(_expr);
		JSReferenceExpression expr = null;

		if(nameComponent instanceof JSVariable)
		{
			String varName = nameComponent.getName();
			if(varName != null)
			{
				nameComponents.add(varName);
			}
		}
		else if(nameComponent instanceof JSReferenceExpression)
		{
			expr = (JSReferenceExpression) nameComponent;
		}

		if(expr != null)
		{
			final JSReferenceExpression expr1 = expr;
			visitReferenceExpressionComponentsInRootFirstOrder(expr, new ReferenceExpressionProcessor()
			{

				@Override
				public void processExpression(JSReferenceExpression expr)
				{
					nameComponents.add(expr.getReferencedName());
				}

				@Override
				public void processUnresolvedThis()
				{
					nameComponents.add("");
				}

				@Override
				public boolean isTopLevel(final JSReferenceExpression expression)
				{
					return expr1 == expression;
				}
			});
		}

		return nameComponents.toArray(new String[nameComponents.size()]);
	}

	interface ReferenceExpressionProcessor
	{
		void processExpression(JSReferenceExpression expr);

		void processUnresolvedThis();

		boolean isTopLevel(final JSReferenceExpression expression);
	}


	private static void visitReferenceExpressionComponentsInRootFirstOrder(JSReferenceExpression expr, ReferenceExpressionProcessor processor)
	{
		JSExpression qualifier = expr.getQualifier();

		if(qualifier instanceof JSCallExpression)
		{
			qualifier = ((JSCallExpression) qualifier).getMethodExpression();
		}

		if(qualifier instanceof JSIndexedPropertyAccessExpression)
		{
			qualifier = ((JSIndexedPropertyAccessExpression) qualifier).getQualifier();
		}

		if(qualifier instanceof JSReferenceExpression)
		{
			visitReferenceExpressionComponentsInRootFirstOrder((JSReferenceExpression) qualifier, processor);
		}

		if(qualifier instanceof JSThisExpression)
		{
			processor.processUnresolvedThis();
		}

		final String refName = expr.getReferencedName();

		if(refName != null && (!refName.equals(PROTOTYPE_FIELD_NAME) || processor.isTopLevel(expr)))
		{
			processor.processExpression(expr);
		}
	}

	private static JSReferenceExpression evaluateInitializedPrototype(final JSExpression initializer)
	{
		JSReferenceExpression initializedPrototype = null;

		if(initializer instanceof JSReferenceExpression && PROTOTYPE_FIELD_NAME.equals(((JSReferenceExpression) initializer).getReferencedName()))
		{
			final JSExpression qualifier = ((JSReferenceExpression) initializer).getQualifier();
			if(qualifier instanceof JSReferenceExpression)
			{
				initializedPrototype = (JSReferenceExpression) qualifier;
			}
		}
		else if(initializer instanceof JSAssignmentExpression)
		{
			JSExpression initializerlOperand = ((JSAssignmentExpression) initializer).getLOperand();
			if(initializerlOperand instanceof JSDefinitionExpression)
			{
				JSExpression lOperand = ((JSDefinitionExpression) initializerlOperand).getExpression();
				initializedPrototype = evaluateInitializedPrototype(lOperand);
			}
		}
		return initializedPrototype;
	}

	public static JSReferenceExpression findReferenceExpressionUsedForClassExtending(JSReferenceExpression lOperand)
	{
		return findReferenceExpressionUsedForClassExtending(lOperand, null);
	}

	private static JSReferenceExpression findReferenceExpressionUsedForClassExtending(JSReferenceExpression lOperand, @Nullable Set<String> visited)
	{
		JSReferenceExpression originalExpr = lOperand;
		final ResolveProcessor processor = new ResolveProcessor(lOperand.getText(), true);
		processor.setLocalResolve(true);
		final PsiElement parent = lOperand.getParent();
		JSResolveUtil.treeWalkUp(processor, lOperand, parent, lOperand);

		PsiElement jsElement = processor.getResult();
		if(jsElement == null && parent instanceof JSDefinitionExpression)
		{
			jsElement = (JSElement) parent;
		}

		if(jsElement instanceof JSVariable)
		{
			final JSExpression initialization = ((JSVariable) jsElement).getInitializer();
			final JSReferenceExpression expression = initialization != null ? evaluateInitializedPrototype(initialization) : null;

			if(expression != null)
			{
				lOperand = expression;
			}
			else if(initialization instanceof JSReferenceExpression)
			{
				lOperand = (JSReferenceExpression) initialization;
			}
		}
		else
		{
			final PsiElement parentJsElement = jsElement != null ? jsElement.getParent() : null;

			if(jsElement instanceof JSDefinitionExpression && parentJsElement instanceof JSAssignmentExpression)
			{ // new expression also could mean something extension !
				JSExpression rOperand = ((JSAssignmentExpression) parentJsElement).getROperand();

				if(rOperand instanceof JSCallExpression && !(rOperand instanceof JSNewExpression))
				{
					final JSArgumentList list = ((JSCallExpression) rOperand).getArgumentList();

					if(list != null)
					{
						final JSExpression[] jsExpressions = list.getArguments();

						if(jsExpressions.length >= 2 &&
								jsExpressions[0] instanceof JSReferenceExpression &&
								jsExpressions[1] instanceof JSReferenceExpression)
						{
							lOperand = (JSReferenceExpression) jsExpressions[0];
						}
					}
				}
				else if(rOperand instanceof JSReferenceExpression)
				{
					final JSReferenceExpression expression = evaluateInitializedPrototype(rOperand);

					if(expression != null)
					{
						lOperand = expression;
					}
					else
					{
						lOperand = (JSReferenceExpression) rOperand;
					}
				}
			}
		}
		return lOperand != originalExpr ? replaceLocalVars(lOperand, visited) : lOperand;
	}

	private static JSReferenceExpression replaceLocalVars(final JSReferenceExpression expression, @Nullable Set<String> visited)
	{
		JSReferenceExpression expr = expression;
		JSExpression qualifier = expr.getQualifier();

		JSFunction func = PsiTreeUtil.getParentOfType(expression, JSFunction.class);
		if(func == null)
		{
			return expression;
		}

		while(qualifier instanceof JSReferenceExpression)
		{
			expr = (JSReferenceExpression) qualifier;
			qualifier = expr.getQualifier();
		}

		if(qualifier == null)
		{
			final PsiElement ref = JSResolveUtil.getLocalVariableRef(func, expr);

			if(ref instanceof JSVariable && !(ref instanceof JSParameter))
			{
				final JSExpression initializer = ((JSVariable) ref).getInitializer();

				if(initializer instanceof JSReferenceExpression)
				{
					return replaceExpression(expression, expr, (JSReferenceExpression) initializer);
				}
				else if(expr != expression)
				{
					if(visited == null)
					{
						visited = new HashSet<String>();
					}
					final String replaced = expr.getText();

					if(!visited.contains(replaced))
					{
						visited.add(replaced);
						return replaceExpression(expression, expr, findReferenceExpressionUsedForClassExtending(expr, visited));
					}
				}
				else
				{
					return findReferenceExpressionUsedForClassExtending(expr, visited);
				}
			}
		}
		return expression;
	}

	private static JSReferenceExpression replaceExpression(final JSReferenceExpression expression, final JSReferenceExpression what,
			final JSReferenceExpression by)
	{
		if(expression == what)
		{
			return by;
		}
		int offsetOfExprInExpression = what.getTextOffset() - expression.getTextOffset();
		final JSReferenceExpression copyOfExpr = (JSReferenceExpression) expression.copy();
		final JSReferenceExpression expressionToReplace = PsiTreeUtil.getParentOfType(copyOfExpr.findElementAt(offsetOfExprInExpression),
				JSReferenceExpression.class);
		expressionToReplace.replace(by);
		return copyOfExpr;
	}

	public static JSElement findQualifyingExpressionFromArgumentList(final JSArgumentList parent)
	{
		PsiElement firstParent = parent.getParent();
		final PsiElement grandParent = firstParent.getParent();

		if(grandParent instanceof JSVariable)
		{
			return (JSElement) grandParent;
		}

		if(grandParent instanceof JSAssignmentExpression)
		{
			JSExpression jsExpression = ((JSAssignmentExpression) grandParent).getLOperand();
			JSExpression assignedTo = jsExpression instanceof JSDefinitionExpression ? ((JSDefinitionExpression) jsExpression).getExpression() : null;
			if(assignedTo instanceof JSReferenceExpression)
			{
				return assignedTo;
			}
		}
		if(grandParent instanceof JSExpressionStatement && firstParent instanceof JSCallExpression)
		{
			JSExpression methodExpression = ((JSCallExpression) firstParent).getMethodExpression();
			String methodName = methodExpression instanceof JSReferenceExpression ? ((JSReferenceExpression) methodExpression).getReferencedName() : null;

			if("each".equals(methodName) || "extend".equals(methodName))
			{
				JSExpression expression = ((JSReferenceExpression) methodExpression).getQualifier();

				if(expression instanceof JSReferenceExpression)
				{
					JSReferenceExpression qualifierExpr = (JSReferenceExpression) expression;

					if(FN_FUN_NAME.equals(qualifierExpr.getReferencedName()))
					{
						expression = qualifierExpr.getQualifier();
					}
				}
				if(expression != null && J_QUERY_VAR_NAME.equals(expression.getText()))
				{
					return expression;
				}
			}
			else if("implement".equals(methodName))
			{
				JSExpression qualifier = ((JSReferenceExpression) methodExpression).getQualifier();
				if(qualifier instanceof JSReferenceExpression && parent.getArguments().length == 1)
				{
					return qualifier;
				}
			}
		}

		JSExpression[] jsExpressions = parent.getArguments();
		for(int i = 0; i < jsExpressions.length; ++i)
		{
			final JSExpression expr = jsExpressions[i];

			if(expr instanceof JSReferenceExpression || (expr instanceof JSLiteralExpression && !expr.textContains(' ') && expr.getTextLength() < 100))
			{
				return expr;
			}
			else if(expr instanceof JSCallExpression)
			{
				final JSArgumentList argumentList = ((JSCallExpression) expr).getArgumentList();
				if(argumentList != null)
				{
					jsExpressions = argumentList.getArguments();
					i = -1;
				}
			}
			else if(expr instanceof JSArrayLiteralExpression)
			{
				jsExpressions = ((JSArrayLiteralExpression) expr).getExpressions();
				i = -1;
			}
		}
		return null;
	}
}
