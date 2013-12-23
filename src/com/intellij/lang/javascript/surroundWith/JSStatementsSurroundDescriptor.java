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
package com.intellij.lang.javascript.surroundWith;

import java.util.ArrayList;
import java.util.List;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.lang.surroundWith.SurroundDescriptor;
import com.intellij.lang.surroundWith.Surrounder;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;

/**
 * Created by IntelliJ IDEA.
 * User: yole
 * Date: 12.07.2005
 * Time: 14:14:55
 */
public class JSStatementsSurroundDescriptor implements SurroundDescriptor
{
	private static final Surrounder[] SURROUNDERS = {
			new JSWithBlockSurrounder(),
			new JSWithIfSurrounder(),
			new JSWithIfElseSurrounder(),
			new JSWithWhileSurrounder(),
			new JSWithDoWhileSurrounder(),
			new JSWithForSurrounder(),
			new JSWithTryCatchSurrounder(),
			new JSWithTryFinallySurrounder(),
			new JSWithTryCatchFinallySurrounder(),
			new JSWithWithSurrounder(),
			new JSWithFunctionSurrounder(),
			new JSWithFunctionExpressionSurrounder(),
	};

	@Override
	@NotNull
	public PsiElement[] getElementsToSurround(PsiFile file, int startOffset, int endOffset)
	{
		final PsiElement[] statements = findStatementsInRange(file, startOffset, endOffset);
		if(statements == null)
		{
			return PsiElement.EMPTY_ARRAY;
		}
		return statements;
	}

	@Override
	@NotNull
	public Surrounder[] getSurrounders()
	{
		return SURROUNDERS;
	}

	@Override
	public boolean isExclusive()
	{
		return false;
	}

	private PsiElement[] findStatementsInRange(PsiFile file, int startOffset, int endOffset)
	{
		PsiElement element1 = file.findElementAt(startOffset);
		PsiElement element2 = file.findElementAt(endOffset - 1);
		if(element1 instanceof PsiWhiteSpace)
		{
			startOffset = element1.getTextRange().getEndOffset();
			element1 = file.findElementAt(startOffset);
		}
		if(element2 instanceof PsiWhiteSpace)
		{
			endOffset = element2.getTextRange().getStartOffset();
			element2 = file.findElementAt(endOffset - 1);
		}
		if(element1 == null || element2 == null)
		{
			return null;
		}

		final JSStatement statement = PsiTreeUtil.getParentOfType(element1, JSStatement.class);
		final JSStatement statement2 = PsiTreeUtil.getParentOfType(element2, JSStatement.class);

		PsiElement parent = PsiTreeUtil.findCommonParent(element1, element2);
		while(true)
		{
			if(parent instanceof JSBlockStatement || ((parent instanceof JSEmbeddedContentImpl || parent instanceof JSFile) && (statement != null &&
					statement2 != null && PsiTreeUtil.isAncestor(parent, statement, false) && PsiTreeUtil.isAncestor(parent, statement2, false))))
			{
				break;
			}
			if(parent instanceof JSStatement)
			{
				parent = parent.getParent();
				break;
			}
			if(parent instanceof PsiFile)
			{
				return null;
			}
			parent = parent.getParent();
		}


		while(!element1.getParent().equals(parent))
		{
			element1 = element1.getParent();
		}
		if(startOffset != element1.getTextRange().getStartOffset())
		{
			return null;
		}

		while(!element2.getParent().equals(parent))
		{
			element2 = element2.getParent();
		}
		if(endOffset != element2.getTextRange().getEndOffset())
		{
			return null;
		}

		final ASTNode[] astNodes = parent.getNode().getChildren(null);
		List<PsiElement> children = new ArrayList<PsiElement>(astNodes.length);
		for(ASTNode node : astNodes)
		{
			children.add(node.getPsi());
		}

		ArrayList<PsiElement> array = new ArrayList<PsiElement>();
		boolean flag = false;
		for(PsiElement child : children)
		{
			if(child.equals(element1))
			{
				flag = true;
			}
			if(flag /*&& !(child instanceof PsiWhiteSpace)*/)
			{
				array.add(child);
			}
			if(child.equals(element2))
			{
				break;
			}
		}

		for(PsiElement element : array)
		{
			if(!(element instanceof JSStatement || element instanceof PsiWhiteSpace || element instanceof PsiComment))
			{
				return null;
			}
		}

		return array.toArray(new PsiElement[array.size()]);
	}
}
