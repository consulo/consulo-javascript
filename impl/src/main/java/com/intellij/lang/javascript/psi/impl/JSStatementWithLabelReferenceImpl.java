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

import java.util.ArrayList;
import java.util.List;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.PsiElementProcessor;
import com.intellij.util.ArrayUtil;
import com.intellij.util.IncorrectOperationException;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 9:52:04 PM
 * To change this template use File | Settings | File Templates.
 */
class JSStatementWithLabelReferenceImpl extends JSStatementImpl
{
	private PsiReference[] myReferences;
	private String myReferencesText;

	protected JSStatementWithLabelReferenceImpl(final ASTNode node)
	{
		super(node);
	}

	public String getLabel()
	{
		final ASTNode label = getNode().findChildByType(JSTokenTypes.IDENTIFIER);
		return label != null ? label.getText() : null;
	}

	@Override
	@NotNull
	public PsiReference[] getReferences()
	{
		final String text = getText();

		if(!text.equals(myReferencesText) || myReferences == null)
		{
			final ASTNode label = getNode().findChildByType(JSTokenTypes.IDENTIFIER);
			if(label != null)
			{
				myReferences = new PsiReference[]{new JSStatementWithLabelReferenceImpl.LabelReference(label.getPsi())};
			}
			else
			{
				myReferences = PsiReference.EMPTY_ARRAY;
			}
			myReferencesText = text;
		}
		return myReferences;
	}

	private class LabelReference implements PsiReference
	{
		private PsiElement labelNode;

		LabelReference(PsiElement _labelNode)
		{
			labelNode = _labelNode;
		}

		@Override
		public PsiElement getElement()
		{
			return JSStatementWithLabelReferenceImpl.this;
		}

		@Override
		public TextRange getRangeInElement()
		{
			final int startOffsetInParent = labelNode.getStartOffsetInParent();
			return new TextRange(startOffsetInParent, startOffsetInParent + labelNode.getTextLength());
		}

		@Override
		@Nullable
		public PsiElement resolve()
		{
			final PsiElement[] result = new PsiElement[1];

			processElements(new PsiElementProcessor<JSLabeledStatement>()
			{
				private final String label = getCanonicalText();

				@Override
				public boolean execute(final JSLabeledStatement element)
				{
					if(label.equals(element.getLabel()))
					{
						result[0] = element;
						return false;
					}
					return true;
				}
			});

			return result[0];
		}

		@Override
		public String getCanonicalText()
		{
			return labelNode.getText();
		}

		@Override
		public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException
		{
			JSChangeUtil.doIdentifierReplacement(getElement(), labelNode, newElementName);
			return getElement();
		}

		@Override
		public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException
		{
			return null;
		}

		@Override
		public boolean isReferenceTo(PsiElement element)
		{
			return getManager().areElementsEquivalent(resolve(), element);
		}

		@Override
		public Object[] getVariants()
		{
			final List<String> labels = new ArrayList<String>(1);
			processElements(new PsiElementProcessor<JSLabeledStatement>()
			{
				@Override
				public boolean execute(final JSLabeledStatement element)
				{
					labels.add(element.getLabel());
					return true;
				}
			});
			return ArrayUtil.toStringArray(labels);
		}

		private void processElements(PsiElementProcessor<JSLabeledStatement> processor)
		{
			PsiElement run = getParent();
			while(run != null)
			{
				if(run instanceof JSLabeledStatement)
				{
					if(!processor.execute((JSLabeledStatement) run))
					{
						return;
					}
				}

				if(run instanceof JSFunction)
				{
					break;
				}
				run = run.getParent();
			}
		}

		@Override
		public boolean isSoft()
		{
			return false;
		}
	}
}
