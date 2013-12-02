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

		public PsiElement getElement()
		{
			return JSStatementWithLabelReferenceImpl.this;
		}

		public TextRange getRangeInElement()
		{
			final int startOffsetInParent = labelNode.getStartOffsetInParent();
			return new TextRange(startOffsetInParent, startOffsetInParent + labelNode.getTextLength());
		}

		@Nullable
		public PsiElement resolve()
		{
			final PsiElement[] result = new PsiElement[1];

			processElements(new PsiElementProcessor<JSLabeledStatement>()
			{
				private final String label = getCanonicalText();

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

		public String getCanonicalText()
		{
			return labelNode.getText();
		}

		public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException
		{
			JSChangeUtil.doIdentifierReplacement(getElement(), labelNode, newElementName);
			return getElement();
		}

		public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException
		{
			return null;
		}

		public boolean isReferenceTo(PsiElement element)
		{
			return getManager().areElementsEquivalent(resolve(), element);
		}

		public Object[] getVariants()
		{
			final List<String> labels = new ArrayList<String>(1);
			processElements(new PsiElementProcessor<JSLabeledStatement>()
			{
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

		public boolean isSoft()
		{
			return false;
		}
	}
}
