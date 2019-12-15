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

package com.intellij.lang.javascript.structureView;

import com.intellij.ide.structureView.StructureViewTreeElement;
import com.intellij.lang.injection.InjectedLanguageManager;
import com.intellij.lang.javascript.index.JSSymbolUtil;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.util.TextRange;
import com.intellij.pom.Navigatable;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import consulo.javascript.psi.JavaScriptImportStatementBase;
import gnu.trove.THashMap;
import gnu.trove.THashSet;
import gnu.trove.TIntObjectHashMap;
import gnu.trove.TIntObjectProcedure;

import javax.annotation.Nullable;
import java.util.*;

/**
 * @author max
 */
public class JSStructureViewElement implements StructureViewTreeElement
{
	protected PsiElement myElement;
	private boolean myInherited;

	public JSStructureViewElement(final PsiElement element)
	{
		myElement = element;
	}

	@Override
	public PsiElement getValue()
	{
		return myElement;
	}

	PsiElement getRealElement()
	{
		return myElement;
	}

	@Override
	public void navigate(boolean requestFocus)
	{
		((Navigatable) myElement).navigate(requestFocus);
	}

	@Override
	public boolean canNavigate()
	{
		return ((Navigatable) myElement).canNavigate();
	}

	@Override
	public boolean canNavigateToSource()
	{
		return canNavigate();
	}

	@Override
	public StructureViewTreeElement[] getChildren()
	{
		final PsiElement element = getUpToDateElement();
		// since we use proxies for StructureViewElement then real element may invalidate due to structural change
		if(element == null)
		{
			return EMPTY_ARRAY;
		}

		final Set<String> referencedNamedIds = new HashSet<String>();


		List<StructureViewTreeElement> children = collectMyElements(referencedNamedIds);

		ArrayList<StructureViewTreeElement> elementsFromSupers = null;

		if(element instanceof JSClass)
		{
			for(JSClass clazz : ((JSClass) element).getSuperClasses())
			{
				final StructureViewTreeElement[] structureViewTreeElements = createStructureViewElement(clazz).getChildren();

				if(elementsFromSupers == null)
				{
					elementsFromSupers = new ArrayList<StructureViewTreeElement>(structureViewTreeElements.length);
				}
				else
				{
					elementsFromSupers.ensureCapacity(elementsFromSupers.size() + structureViewTreeElements.length);
				}

				for(StructureViewTreeElement e : structureViewTreeElements)
				{
					if(!isVisible((PsiElement) e.getValue(), element))
					{
						continue;
					}
					// TODO CSS styles can also be inherited, so we better show them
					// This can't be done until 'inherited' property is elevated up to StructureViewTreeElement interface
					if(!(e instanceof JSStructureViewElement))
					{
						continue;
					}
					((JSStructureViewElement) e).setInherited(true);
					elementsFromSupers.add(e);
				}
			}
		}


		Collections.sort(children, new Comparator<StructureViewTreeElement>()
		{
			@Override
			public int compare(final StructureViewTreeElement _o1, final StructureViewTreeElement _o2)
			{
				PsiElement e = getPsiElement(_o1);
				PsiElement e2 = getPsiElement(_o2);

				TextRange t1 = InjectedLanguageManager.getInstance(e.getProject()).injectedToHost(e, e.getTextRange());
				TextRange t2 = InjectedLanguageManager.getInstance(e.getProject()).injectedToHost(e, e.getTextRange());
				final int offset = e.getTextOffset() + t1.getStartOffset();
				final int offset2 = e2.getTextOffset() + t2.getStartOffset();

				return offset - offset2;
			}
		});

		if(elementsFromSupers != null)
		{
			Map<String, JSFunction> functionsByNames = new THashMap<String, JSFunction>();
			for(StructureViewTreeElement child : children)
			{
				PsiElement el = getPsiElementResolveProxy(child);
				if(el instanceof JSFunction)
				{
					JSFunction function = (JSFunction) el;
					functionsByNames.put(function.getName(), function);
				}
			}

			for(StructureViewTreeElement superTreeElement : elementsFromSupers)
			{
				PsiElement superElement = getPsiElementResolveProxy(superTreeElement);
				boolean addSuperElement = true;
				if(superElement instanceof JSFunction)
				{
					JSFunction superFunction = (JSFunction) superElement;
					JSFunction function = functionsByNames.get(superFunction.getName());
					if(function != null)
					{
						// TODO check signature
						addSuperElement = false;
					}
				}

				if(addSuperElement)
				{
					children.add(superTreeElement);
				}
			}
		}
		return children.toArray(new StructureViewTreeElement[children.size()]);
	}

	protected JSStructureViewElement createStructureViewElement(PsiElement element)
	{
		return new JSStructureViewElement(element);
	}

	private static PsiElement getPsiElement(StructureViewTreeElement element)
	{
		PsiElement e;
		if(element instanceof JSStructureViewElement)
		{
			final JSStructureViewElement o1 = (JSStructureViewElement) element;
			e = o1.myElement;
		}
		else
		{
			e = (PsiElement) element.getValue();
		}
		return e;
	}

	public static PsiElement getPsiElementResolveProxy(StructureViewTreeElement element)
	{
		return getPsiElement(element);
	}


	protected List<StructureViewTreeElement> collectMyElements(final Set<String> referencedNamedIds)
	{
		final TIntObjectHashMap<PsiElement> offset2Element = new TIntObjectHashMap<PsiElement>();

		collectChildrenFromElement(myElement, referencedNamedIds, offset2Element);


		final List<StructureViewTreeElement> children = new ArrayList<StructureViewTreeElement>(offset2Element.size());
		offset2Element.forEachEntry(new TIntObjectProcedure<PsiElement>()
		{
			@Override
			public boolean execute(int textOffset, PsiElement element)
			{
				children.add(createStructureViewElement(element));
				return true;
			}
		});
		return children;
	}

	private static boolean isVisible(PsiElement namedElement, final PsiElement element)
	{
		if(namedElement instanceof JSAttributeListOwner)
		{
			final JSAttributeListOwner attributeListOwner = (JSAttributeListOwner) namedElement;
			final JSAttributeList attributeList = attributeListOwner.getAttributeList();

			if(attributeList != null)
			{
				final JSAttributeList.AccessType type = attributeList.getAccessType();

				if(type == JSAttributeList.AccessType.PACKAGE_LOCAL)
				{
					final JSPackageStatement packageStatement = PsiTreeUtil.getParentOfType(namedElement, JSPackageStatement.class);
					final JSPackageStatement packageStatement2 = PsiTreeUtil.getParentOfType(element, JSPackageStatement.class);
					String packageQName;

					return packageStatement == packageStatement2 || (packageStatement != null &&
							packageStatement2 != null &&
							(packageQName = packageStatement.getQualifiedName()) != null &&
							packageQName.equals(packageStatement2.getQualifiedName()));
				}
				return type != JSAttributeList.AccessType.PRIVATE;
			}
		}
		return true;
	}

	private static void collectChildrenFromElement(final PsiElement element, final Set<String> referencedNamedIds, final TIntObjectHashMap<PsiElement> offset2Element)
	{
		element.acceptChildren(new JSElementVisitor()
		{
			Set<PsiFile> visited;
			PsiElement context = element;

			@Override
			public void visitElement(PsiElement element)
			{
				if(element instanceof JSNamedElement &&
						((JSNamedElement) element).getName() != null &&
						!(element instanceof JSDefinitionExpression) &&
						!(element instanceof JSLabeledStatement) &&
						!(element instanceof JSPackageStatement) &&
						!(element instanceof JavaScriptImportStatementBase))
				{
					if(!(element instanceof JSFunction) || !(element.getParent() instanceof JSProperty))
					{
						addElement(element);
					}
					else
					{
						element.acceptChildren(this);
					}
				}
				else
				{
					element.acceptChildren(this);
				}
			}

			@Override
			public void visitJSParameter(final JSParameter node)
			{
				// Do not add parameters to structure view
			}

			@Override
			public void visitJSIncludeDirective(final JSIncludeDirective includeDirective)
			{
				final PsiFile file = includeDirective.resolveFile();
				if(file instanceof JSFile)
				{
					if(visited != null && visited.contains(file))
					{
						return;
					}
					if(visited == null)
					{
						visited = new THashSet<PsiFile>();
					}
					visited.add(file);

					final PsiElement prevContext = context;
					context = file;
					context.putUserData(JSResolveUtil.contextKey, element);
					file.acceptChildren(this);
					context = prevContext;
				}
			}

			@Override
			public void visitJSObjectLiteralExpression(final JSObjectLiteralExpression node)
			{
				final PsiElement parent = node.getParent();
				if(parent instanceof JSVariable ||
						parent instanceof JSProperty ||
						parent instanceof JSFile ||
						parent instanceof JSReturnStatement ||
						parent instanceof JSAssignmentExpression)
				{
					node.acceptChildren(this);
					return;
				}
				if(parent instanceof JSArgumentList)
				{
					final JSElement expression = JSSymbolUtil.findQualifyingExpressionFromArgumentList((JSArgumentList) parent);
					if(expression != null)
					{
						return;
					}
				}
				if(node.getProperties().length > 0)
				{
					addElement(node);
				}
			}

			@Override
			public void visitJSVariable(final JSVariable node)
			{
				if(element instanceof JSFunction)
				{
					return;
				}
				super.visitJSVariable(node);
			}

			@Override
			public void visitJSAssignmentExpression(final JSAssignmentExpression node)
			{
				JSExpression rOperand = node.getROperand();
				final JSExpression lOperand = node.getLOperand();

				final boolean outsideFunction = PsiTreeUtil.getParentOfType(node, JSFunction.class) == null;
				if(!outsideFunction)
				{
					return;
				}

				if(rOperand instanceof JSCallExpression)
				{
					rOperand = ((JSCallExpression) rOperand).getMethodExpression();
				}

				if(rOperand instanceof JSFunction)
				{
					JSExpression qualifier = null;
					final JSExpression operand = ((JSDefinitionExpression) lOperand).getExpression();

					if(operand instanceof JSReferenceExpression)
					{
						qualifier = ((JSReferenceExpression) operand).getQualifier();
					}
					if((qualifier == null || qualifier instanceof JSThisExpression))
					{
						addElement(rOperand);
					}
				}
				else if(lOperand != null)
				{
					final JSExpression operand = ((JSDefinitionExpression) lOperand).getExpression();
					if(operand instanceof JSReferenceExpression && ((JSReferenceExpression) operand).getQualifier() instanceof JSThisExpression)
					{
						final PsiElement resolved = ((JSReferenceExpression) operand).resolve();
						if(resolved == lOperand)
						{
							addElement(lOperand);
						}
					}
					//super.visitJSAssignmentExpression(node);
				}
			}

			private void addElement(final PsiElement lOperand)
			{
				if(lOperand instanceof JSNamedElement)
				{
					final String namedId = ((JSNamedElement) lOperand).getName();
					if(referencedNamedIds.contains(namedId))
					{
						return;
					}
					referencedNamedIds.add(namedId);
				}
				offset2Element.put(lOperand.getTextOffset(), lOperand);
			}
		});
	}

	@Override
	public ItemPresentation getPresentation()
	{
		return new JSStructureItemPresentation(this);
	}

	public boolean isInherited()
	{
		return myInherited;
	}

	private void setInherited(boolean b)
	{
		myInherited = b;
	}

	public
	@Nullable
	PsiElement getUpToDateElement()
	{
		boolean isValid = myElement.isValid();

		if(!isValid)
		{
			return null;
		}

		return myElement;
	}

	static abstract class JSStructureItemPresentationBase implements ItemPresentation
	{
		final protected JSStructureViewElement element;

		JSStructureItemPresentationBase(JSStructureViewElement _element)
		{
			element = _element;
		}

		@Override
		public String getLocationString()
		{
			return null;
		}
	}
}
