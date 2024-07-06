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

package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.javascript.JSDocTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.util.JSLookupUtil;
import consulo.document.util.TextRange;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.ASTNode;
import consulo.language.editor.util.PsiUtilBase;
import consulo.language.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.util.collection.ArrayUtil;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

public class JSDocTagImpl extends JSElementImpl implements JSDocTag
{
	private volatile PsiReference[] myRefs;
	private volatile long myModificationCount = -1;

	public JSDocTagImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public String getName()
	{
		final PsiElement element = findChildByType(JSDocTokenTypes.DOC_TAG_NAME);
		return element != null ? element.getText().substring(1) : null;
	}

	@Override
	public PsiElement setName(@NonNls @Nonnull final String name) throws IncorrectOperationException
	{
		throw new IncorrectOperationException();
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSDocTag(this);
	}

	@Override
	public JSDocTagValue getValue()
	{
		return findChildByClass(JSDocTagValue.class);
	}

	@Nonnull
	@Override
	public PsiReference[] getReferences()
	{
		final long count = getManager().getModificationTracker().getModificationCount();

		if(count != myModificationCount)
		{
			final @NonNls String name = getName();
			PsiReference[] result = PsiReference.EMPTY_ARRAY;

			if("param".equals(name))
			{
				final PsiElement data = findChildByType(JSDocTokenTypes.DOC_COMMENT_DATA);
				if(data != null)
				{
					result = new PsiReference[]{new ParamReference(this)};
				}
			}

			myRefs = result;
			myModificationCount = count;
		}

		return myRefs;
	}

	private static class ParamReference implements PsiReference, EmptyResolveMessageProvider
	{
		private PsiElement myJsDocTagValue;
		private TextRange myRange;

		public ParamReference(final JSDocTagImpl elt)
		{
			reset(elt);
		}

		private void reset(final JSDocTagImpl elt)
		{
			myJsDocTagValue = elt.findChildByType(JSDocTokenTypes.DOC_COMMENT_DATA);
			int offsetInParent = myJsDocTagValue.getStartOffsetInParent();
			int textLength;

			if(myJsDocTagValue.textContains('['))
			{
				final String text = myJsDocTagValue.getText();
				final int at = text.indexOf('[');
				offsetInParent += at + 1;

				// @param [name] | //@param[name="something"] | [obj.prop2(='somestring')?]
				int rBracketIndex = text.indexOf(']');
				int eqIndex = text.indexOf('=');
				int dotIndex = text.indexOf('.');
				int combinedIndex = text.length();

				if(rBracketIndex != -1)
				{
					combinedIndex = rBracketIndex;
				}
				if(eqIndex != -1)
				{
					combinedIndex = eqIndex;
				}
				if(dotIndex != -1 && (eqIndex == -1 || dotIndex < eqIndex))
				{
					combinedIndex = dotIndex;
				}
				textLength = combinedIndex - at - 1;

			}
			else if(myJsDocTagValue.textContains('='))
			{
				textLength = myJsDocTagValue.getText().indexOf('='); // @param name=""
			}
			else
			{
				if(myJsDocTagValue.textContains('.'))
				{ //@param userInfo.email
					textLength = myJsDocTagValue.getText().indexOf('.');
				}
				else
				{
					textLength = myJsDocTagValue.getTextLength();
				}
			}
			myRange = new TextRange(offsetInParent, offsetInParent + textLength);
		}

		@Override
		public PsiElement getElement()
		{
			return myJsDocTagValue.getParent();
		}

		@Override
		public TextRange getRangeInElement()
		{
			return myRange;
		}

		@Override
		public String getCanonicalText()
		{
			final int offsetInText = myRange.getStartOffset() - myJsDocTagValue.getStartOffsetInParent();
			return myJsDocTagValue.getText().substring(offsetInText, offsetInText + myRange.getLength());
		}

		@Override
		public PsiElement handleElementRename(final String newElementName) throws IncorrectOperationException
		{
			JSDocTag jsDocTag = (JSDocTag) myJsDocTagValue.getParent();
			final ElementManipulator<JSDocTag> manipulator = ElementManipulators.getManipulator(jsDocTag);
			jsDocTag = manipulator.handleContentChange(jsDocTag, myRange, newElementName);
			reset((JSDocTagImpl) jsDocTag);
			return myJsDocTagValue;
		}

		@Override
		public PsiElement bindToElement(@Nonnull final PsiElement element) throws IncorrectOperationException
		{
			return null;
		}

		@Override
		public boolean isReferenceTo(final PsiElement element)
		{
			return element.isEquivalentTo(resolve());
		}

		private static JSParameterList findParameterList(PsiElement elt)
		{
			if(elt == null)
			{
				return null;
			}
			PsiComment psiComment = PsiTreeUtil.getParentOfType(elt, PsiComment.class);
			if(psiComment == null)
			{
				return null;
			}
			final PsiElement parent = psiComment.getParent();
			if(parent instanceof PsiComment)
			{
				psiComment = (PsiComment) parent;
			}

			PsiElement next = psiComment.getNextSibling();
			if(next instanceof PsiWhiteSpace)
			{
				next = next.getNextSibling();
			}
			if(next instanceof PsiComment)
			{
				next = next.getNextSibling();
				if(next instanceof PsiWhiteSpace)
				{
					next = next.getNextSibling();
				}
			}

			if(next instanceof JSExpressionStatement)
			{
				final JSExpression expression = ((JSExpressionStatement) next).getExpression();

				if(expression instanceof JSAssignmentExpression)
				{
					JSExpression roperand = ((JSAssignmentExpression) expression).getROperand();
					if(roperand instanceof JSNewExpression)
					{
						roperand = ((JSNewExpression) roperand).getMethodExpression();
					}

					if(roperand instanceof JSFunctionExpression)
					{
						next = roperand;
					}
				}
			}
			else if(next instanceof JSProperty)
			{
				next = ((JSProperty) next).getValue();
			}
			else if(next instanceof JSVarStatement)
			{
				JSVariable[] variables = ((JSVarStatement) next).getVariables();
				if(variables.length > 0)
				{
					JSExpression initializer = variables[0].getInitializer();
					if(initializer instanceof JSFunctionExpression)
					{
						next = initializer;
					}
				}
			}
			else if(next != null)
			{
				if(next instanceof JSVariable)
				{
					JSExpression expression = ((JSVariable) next).getInitializer();
					if(expression instanceof JSFunctionExpression)
					{
						next = expression;
					}
				}
				PsiElement nextParent = next.getParent();
				if(nextParent instanceof JSFunction)
				{
					next = nextParent;
				}
			}
			if(next instanceof JSFunction)
			{
				return ((JSFunction) next).getParameterList();
			}

			return null;
		}

		@Override
		public PsiElement resolve()
		{
			final JSParameterList parameterList = findParameterList(getElement());

			if(parameterList != null)
			{
				final String name = getCanonicalText();
				for(JSParameter param : parameterList.getParameters())
				{
					if(name.equals(param.getName()))
					{
						return param;
					}
				}
			}

			return null;
		}

		@Override
		public Object[] getVariants()
		{
			final PsiElement elt = getElement();
			final JSParameterList parameterList = findParameterList(PsiUtilBase.getOriginalElement(elt, elt.getClass()));

			if(parameterList != null)
			{
				final JSParameter[] parameters = parameterList.getParameters();
				final Object[] result = new Object[parameters.length];

				for(int i = 0; i < parameters.length; ++i)
				{
					result[i] = JSLookupUtil.createLookupItem(parameters[i], parameters[i].getName(), JSLookupUtil.LookupPriority.HIGHEST);
				}
				return result;
			}

			return ArrayUtil.EMPTY_OBJECT_ARRAY;
		}

		@Override
		public boolean isSoft()
		{
			return false;
		}

		@Override
		public String getUnresolvedMessagePattern()
		{
			return JavaScriptLocalize.javascriptValidationMessageIncorrectParameterName().get();
		}
	}
}