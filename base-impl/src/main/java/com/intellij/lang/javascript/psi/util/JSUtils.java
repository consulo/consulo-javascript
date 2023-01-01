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

package com.intellij.lang.javascript.psi.util;

import com.intellij.lang.javascript.psi.*;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.template.TemplateLanguageFileViewProvider;
import consulo.xml.psi.xml.XmlAttributeValue;
import consulo.xml.psi.xml.XmlFile;
import consulo.xml.psi.xml.XmlTag;
import consulo.xml.psi.xml.XmlTagChild;

import static com.intellij.lang.javascript.JSElementTypes.*;
import static com.intellij.lang.javascript.JSTokenTypes.*;

/**
 * @author max
 */
public class JSUtils
{
	public static boolean isLHSExpression(JSExpression expr)
	{
		if(expr instanceof JSDefinitionExpression)
		{
			expr = ((JSDefinitionExpression) expr).getExpression();
		}

		if(expr instanceof JSReferenceExpression)
		{
			return true;
		}

		if(expr instanceof JSParenthesizedExpression)
		{
			return isLHSExpression(((JSParenthesizedExpression) expr).getInnerExpression());
		}

		if(expr instanceof JSIndexedPropertyAccessExpression)
		{
			return true;
		}

		if(expr instanceof JSCallExpression)
		{
			return true;
		}

		if(expr instanceof JSNewExpression)
		{
			return true;
		}

		return false;
	}

	public static boolean isNeedParenthesis(JSExpression oldExpr, JSExpression newExpr)
	{
		int priority = getExpressionPrecedence(newExpr);
		final PsiElement parent = oldExpr.getParent();
		if(!(parent instanceof JSExpression))
		{
			return false;
		}
		int parentPriority = getExpressionPrecedence((JSExpression) parent);
		if(priority < parentPriority)
		{
			return true;
		}
		if(priority == parentPriority && parent instanceof JSBinaryExpression)
		{
			final IElementType operationSign = ((JSBinaryExpression) parent).getOperationSign();
			if(oldExpr != ((JSBinaryExpression) parent).getROperand())
			{
				return false;
			}
			if(!ASSOC_OPERATIONS.contains(operationSign))
			{
				return true;
			}

			return (((JSBinaryExpression) newExpr).getOperationSign() != operationSign);
		}

		return false;
	}

	private static int getExpressionPrecedence(JSExpression expr)
	{
		IElementType i = expr.getNode().getElementType();
		if(i == ASSIGNMENT_EXPRESSION)
		{
			return 0;
		}
		else if(i == CONDITIONAL_EXPRESSION)
		{
			return 1;
		}
		else if(i == BINARY_EXPRESSION)
		{
			{
				IElementType opType = ((JSBinaryExpression) expr).getOperationSign();
				if(opType == OROR)
				{
					return 2;
				}
				else if(opType == ANDAND)
				{
					return 3;
				}
				else if(opType == OR)
				{
					return 4;
				}
				else if(opType == XOR)
				{
					return 5;
				}
				else if(opType == AND)
				{
					return 6;
				}
				else if(EQUALITY_OPERATIONS.contains(opType))
				{
					return 7;
				}
				else if(RELATIONAL_OPERATIONS.contains(opType))
				{
					return 8;
				}
				else if(SHIFT_OPERATIONS.contains(opType))
				{
					return 9;
				}
				else if(ADDITIVE_OPERATIONS.contains(opType))
				{
					return 10;
				}
				else if(MULTIPLICATIVE_OPERATIONS.contains(opType))
				{
					return 11;
				}
			}

			return 8;
		}
		else if(i == PREFIX_EXPRESSION)
		{
			return 12;
		}
		else if(i == POSTFIX_EXPRESSION)
		{
			return 13;
		}

		return 14;
	}

	public static PsiElement findStatementAnchor(final JSReferenceExpression referenceExpression, final PsiFile file)
	{
		PsiElement anchor = PsiTreeUtil.getParentOfType(referenceExpression, JSStatement.class);

		if(file instanceof XmlFile)
		{
			final XmlAttributeValue attributeValue = PsiTreeUtil.getParentOfType(referenceExpression, XmlAttributeValue.class);

			if(attributeValue != null)
			{
				XmlFile root = ((XmlFile) file);
				if(root.getViewProvider() instanceof TemplateLanguageFileViewProvider)
				{
					final TemplateLanguageFileViewProvider viewProvider = (TemplateLanguageFileViewProvider) root.getViewProvider();
					final PsiFile psi = viewProvider.getPsi(viewProvider.getTemplateDataLanguage());
					if(psi instanceof XmlFile)
					{
						root = (XmlFile) psi;
					}
				}

				final XmlTag tag = root.getDocument().getRootTag();

				if(tag != null)
				{
					final XmlTag headTag = tag.findFirstSubTag("head");

					if(headTag != null)
					{
						final XmlTag scriptTag = headTag.findFirstSubTag("script");

						if(scriptTag != null)
						{
							PsiElement statementInScript = PsiTreeUtil.getChildOfType(scriptTag, JSStatement.class);
							if(statementInScript != null)
							{
								anchor = statementInScript;
							}
							else
							{
								final XmlTagChild tagChild = PsiTreeUtil.getChildOfType(scriptTag, XmlTagChild.class);
								if(tagChild != null)
								{
									anchor = tagChild;
								}
							}
						}
					}
				}
			}
		}
		return anchor;
	}
}
