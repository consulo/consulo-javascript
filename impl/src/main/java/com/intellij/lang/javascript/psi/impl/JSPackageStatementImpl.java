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

import java.io.IOException;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSSourceElement;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.stubs.JSPackageStatementStub;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.ResolveState;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.util.IncorrectOperationException;

/**
 * @by Maxim.Mossienko
 */
public class JSPackageStatementImpl extends JSStubbedStatementImpl<JSPackageStatementStub> implements JSPackageStatement
{
	public JSPackageStatementImpl(final ASTNode node)
	{
		super(node);
	}

	public JSPackageStatementImpl(final JSPackageStatementStub stub)
	{
		super(stub, JSElementTypes.PACKAGE_STATEMENT);
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSPackageStatement(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	@Override
	public String getName()
	{
		final JSPackageStatementStub stub = getStub();
		if(stub != null)
		{
			return stub.getName();
		}
		final PsiElement node = getNameIdentifier();
		if(node != null)
		{
			return ((JSReferenceExpression) node).getReferencedName();
		}
		return null;
	}

	@Override
	public String getQualifiedName()
	{
		final JSPackageStatementStub stub = getStub();
		if(stub != null)
		{
			return stub.getQualifiedName();
		}

		final PsiElement node = getNameIdentifier();
		if(node != null)
		{
			return node.getText();
		}
		return null;
	}

	@Override
	public JSSourceElement[] getStatements()
	{
		return getStubOrPsiChildren(JSElementTypes.SOURCE_ELEMENTS, JSSourceElement.EMPTY_ARRAY);
	}

	@Override
	public void setQualifiedName(final String expectedPackageNameFromFile)
	{
		doChangeName(getProject(), this, expectedPackageNameFromFile);
	}

	@Override
	public PsiElement setName(@NonNls @NotNull String name) throws IncorrectOperationException
	{
		VirtualFile virtualFile = getContainingFile().getVirtualFile();
		String expectedPackageNameFromFile = JSResolveUtil.getExpectedPackageNameFromFile(virtualFile, getProject(), false);
		if(expectedPackageNameFromFile != null && expectedPackageNameFromFile.equals(getQualifiedName()))
		{
			try
			{
				JSPsiImplUtils.doRenameParentDirectoryIfNeeded(virtualFile, name, this);
			}
			catch(IOException ex)
			{
				throw new IncorrectOperationException("", ex);
			}
		}

		PsiElement child = getNameIdentifier();
		if(child instanceof JSReferenceExpression)
		{
			JSReferenceExpression expr = (JSReferenceExpression) child;
			PsiElement element = expr.getReferenceNameElement();
			if(element != null)
			{
				JSChangeUtil.doIdentifierReplacement(expr, element, name);
			}
		}

		return this;
	}

	@Override
	public boolean processDeclarations(@NotNull final PsiScopeProcessor processor, @NotNull final ResolveState substitutor,
			final PsiElement lastParent, @NotNull final PsiElement place)
	{
		if(lastParent != null && lastParent.getParent() == this)
		{
			return JSImportHandlingUtil.tryResolveImports(processor, this, place);
		}
		else
		{
			return true;
		}
	}

	@Override
	public PsiElement getNameIdentifier()
	{
		return findChildByType(JSElementTypes.REFERENCE_EXPRESSION);
	}

	@Override
	public PsiElement addBefore(@NotNull PsiElement element, PsiElement anchor) throws IncorrectOperationException
	{
		if(JSChangeUtil.isStatementOrComment(element))
		{
			final PsiElement insertedElement = JSChangeUtil.doAddBefore(this, element, anchor);
			CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(getNode(), insertedElement.getNode());
			return insertedElement;
		}
		return super.addBefore(element, anchor);
	}

	@Override
	public PsiElement addAfter(@NotNull PsiElement element, PsiElement anchor) throws IncorrectOperationException
	{
		if(JSChangeUtil.isStatementOrComment(element))
		{
			final PsiElement insertedElement = JSChangeUtil.doAddAfter(this, element, anchor);
			CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(getNode(), insertedElement.getNode());
			return insertedElement;
		}
		return super.addAfter(element, anchor);
	}

	public static void doChangeName(final Project project, final JSPackageStatement packageStatement, final String expected)
	{
		if(expected == null)
		{
			return;
		}
		final PsiElement node = packageStatement.getNameIdentifier();
		final ASTNode parent = packageStatement.getNode();

		if(expected.length() == 0)
		{
			if(node != null)
			{
				final ASTNode treeNext = node.getNode().getTreeNext();
				parent.removeChild(node.getNode());
				if(treeNext.getPsi() instanceof PsiWhiteSpace)
				{
					parent.removeChild(treeNext);
				}
			}
		}
		else
		{
			final ASTNode child = JSChangeUtil.createExpressionFromText(project, expected).getNode();
			if(node != null)
			{
				parent.replaceChild(node.getNode(), child);
			}
			else
			{
				parent.addChild(child, parent.findChildByType(JSTokenTypes.LBRACE));
			}
		}
	}
}
