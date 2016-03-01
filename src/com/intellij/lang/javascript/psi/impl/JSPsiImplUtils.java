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

import gnu.trove.TObjectHashingStrategy;

import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.javascript.lang.psi.JavaScriptTypeElement;
import org.mustbe.consulo.javascript.lang.psi.impl.elementType.BaseJavaScriptElementType;
import org.mustbe.consulo.javascript.lang.psi.stubs.JavaScriptIndexKeys;
import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.openapi.project.DumbService;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.OrderEntry;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiDirectoryContainer;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.StubBasedPsiElement;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlFile;
import com.intellij.util.Consumer;
import com.intellij.util.IncorrectOperationException;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 8, 2008
 *         Time: 3:23:37 PM
 */
public class JSPsiImplUtils
{
	@NonNls
	private static final String ARRAY_TYPE_NAME = "Array";
	@NonNls
	private static final String ARRAY_ELEMENT_TYPE_ANNOTATION_NAME = "ArrayElementType";

	@Nullable
	@RequiredReadAction
	public static JavaScriptTypeElement findTypeElement(@NotNull PsiElement element)
	{
		if(element instanceof StubBasedPsiElement)
		{
			StubElement<?> stub = ((StubBasedPsiElement) element).getStub();
			if(stub != null)
			{
				List<StubElement> childrenStubs = stub.getChildrenStubs();
				for(StubElement childrenStub : childrenStubs)
				{
					IStubElementType stubType = childrenStub.getStubType();
					if(stubType instanceof BaseJavaScriptElementType)
					{
						return (JavaScriptTypeElement) stub.getPsi();
					}
				}
			}
		}

		return findChildrenByClass(element, JavaScriptTypeElement.class);
	}

	@Nullable
	@RequiredReadAction
	@SuppressWarnings("unchecked")
	private static <T> T findChildrenByClass(@NotNull PsiElement element, Class<T> aClass)
	{
		for(PsiElement cur = element.getFirstChild(); cur != null; cur = cur.getNextSibling())
		{
			if(aClass.isInstance(cur))
			{
				return (T) cur;
			}
		}
		return null;
	}

	@Deprecated
	public static ASTNode getTypeExpressionFromDeclaration(JSNamedElement element)
	{
		final ASTNode myNode = element.getNode();
		final ASTNode node = myNode != null ? myNode.findChildByType(JSTokenTypes.COLON) : null;
		String s = null;

		if(node != null)
		{
			return myNode.findChildByType(JSDocumentationUtils.ourTypeFilter, node);
		}

		return null;
	}

	@Deprecated
	public static String getTypeFromDeclaration(JSNamedElement element)
	{
		ASTNode typeExpr = getTypeExpressionFromDeclaration(element);

		String s = null;
		if(typeExpr != null)
		{
			s = typeExpr.getText();
		}
		else if(element instanceof JSParameter && ((JSParameter) element).isRest())
		{
			s = ARRAY_TYPE_NAME;
		}

		if(ARRAY_TYPE_NAME.equals(s))
		{
			final PsiComment psiComment = typeExpr != null ? PsiTreeUtil.getPrevSiblingOfType(typeExpr.getPsi(), PsiComment.class) : null;

			if(psiComment != null)
			{
				final String elementType = JSDocumentationUtils.unwrapCommentDelimiters(psiComment.getText()).trim();

				if(elementType.length() > 0)
				{
					return s + "[" + elementType;
				}
			}

			if(element instanceof JSAttributeListOwner)
			{
				final JSAttributeListOwner attributeListOwner = (JSAttributeListOwner) element;
				final JSAttributeList attributeList = attributeListOwner.getAttributeList();

				if(attributeList != null)
				{
					String type = getArrayElementTypeFromAnnotation(attributeList);
					if(type != null && type.length() > 0)
					{
						return s + "[" + type;
					}
				}
			}

		}

		return s;
	}

	public static
	@Nullable
	String getArrayElementTypeFromAnnotation(JSAttributeList attributeList)
	{
		return getTypeFromAnnotationParameter(attributeList, ARRAY_ELEMENT_TYPE_ANNOTATION_NAME, null);
	}

	public static String getType(JSNamedElement element)
	{
		final String typeFromDeclaration = getTypeFromDeclaration(element);
		if(typeFromDeclaration != null)
		{
			return typeFromDeclaration;
		}

		return JSDocumentationUtils.findTypeFromComments(element);
	}

	public static void updateFileName(JSQualifiedNamedElement jsClassBase, final String newName, final String oldName) throws IncorrectOperationException
	{
		final PsiFile containingFile = jsClassBase.getContainingFile();

		if(containingFile.getContext() == null)
		{
			final VirtualFile virtualFile = containingFile.getVirtualFile();

			if(virtualFile != null && virtualFile.getNameWithoutExtension().equals(oldName))
			{
				final String s = containingFile.getName();
				containingFile.setName(newName + "." + s.substring(s.lastIndexOf('.') + 1));
			}
		}
	}

	public static
	@Nullable
	JSPackageStatement findPackageStatement(JSFile file)
	{
		JSPackageStatement packageStatement = null;

		for(PsiElement statement : file.getChildren())
		{
			if(statement instanceof JSPackageStatement)
			{
				packageStatement = (JSPackageStatement) statement;
				break;
			}
		}
		return packageStatement;
	}

	static
	@NotNull
	PsiElement findTopLevelNavigatableElement(@NotNull JSQualifiedNamedElement jsClass)
	{
		PsiElement sourceElement = findTopLevelNavigatableElementWithSource(jsClass, null);
		if(sourceElement != null)
		{
			return sourceElement;
		}
		return jsClass;
	}

	@Nullable
	public static PsiElement findTopLevelNavigatableElementWithSource(@NotNull JSQualifiedNamedElement jsClass, @Nullable Consumer<JSQualifiedNamedElement> candidatesConsumer)
	{
		if(candidatesConsumer != null)
		{
			candidatesConsumer.consume(jsClass);
		}

		PsiElement sourceElement = findSourceElement(jsClass);
		if(sourceElement != null)
		{
			return sourceElement;
		}

		if(DumbService.isDumb(jsClass.getProject()))
		{
			return null;
		}

		GlobalSearchScope searchScope = jsClass.getResolveScope();
		final String qName = jsClass.getQualifiedName();
		final Collection<JSQualifiedNamedElement> candidates = StubIndex.getElements(JavaScriptIndexKeys.ELEMENTS_BY_QNAME, qName, jsClass.getProject(), searchScope, JSQualifiedNamedElement.class);
		for(Iterator<JSQualifiedNamedElement> i = candidates.iterator(); i.hasNext(); )
		{
			if(!qName.equals(i.next().getQualifiedName()))
			{
				i.remove();
			}
		}

		for(JSQualifiedNamedElement candidate : candidates)
		{
			if(candidate == jsClass)
			{
				continue;
			}

			if(candidatesConsumer != null)
			{
				candidatesConsumer.consume(candidate);
			}

			PsiElement candidateSourceElement = findSourceElement(candidate);
			if(candidateSourceElement != null)
			{
				return candidateSourceElement;
			}
		}
		return null;
	}

	@Nullable
	private static PsiElement findSourceElement(JSQualifiedNamedElement jsClass)
	{
		PsiFile containingFile = jsClass.getContainingFile();
		if(containingFile == null)
		{
			return null;
		}

		VirtualFile vFile = containingFile.getVirtualFile();
		ProjectFileIndex projectFileIndex = ProjectRootManager.getInstance(jsClass.getProject()).getFileIndex();
		if(vFile == null || projectFileIndex.getClassRootForFile(vFile) == null)
		{
			return null;
		}

		final List<OrderEntry> orderEntries = projectFileIndex.getOrderEntriesForFile(vFile);

		String qName = jsClass.getQualifiedName();
		String baseSourceName = jsClass.getName();
		int index = qName != null ? qName.lastIndexOf('.') : -1;
		String packageName = index != -1 ? qName.substring(0, index) : "";
		String relativeFilePath = packageName.length() == 0 ? baseSourceName : packageName.replace('.', '/') + '/' + baseSourceName;
		String relativeFilePath2 = relativeFilePath + ".mxml";
		String relativeFilePath3 = relativeFilePath + ".mxm";
		relativeFilePath += ".as";

		for(OrderEntry orderEntry : orderEntries)
		{
			VirtualFile[] files = orderEntry.getFiles(OrderRootType.SOURCES);

			for(VirtualFile file : files)
			{
				VirtualFile source = file.findFileByRelativePath(relativeFilePath);

				if(source != null)
				{
					PsiFile psiSource = jsClass.getManager().findFile(source);

					if(psiSource instanceof JSFile)
					{
						JSPackageStatement statement = findPackageStatement((JSFile) psiSource);

						if(statement != null)
						{
							for(JSSourceElement el : statement.getStatements())
							{
								if(el.getClass() == jsClass.getClass() && jsClass.getName().equals(el.getName()))
								{
									return el;
								}
							}
						}
						return psiSource;
					}
				}
				else
				{
					source = file.findFileByRelativePath(relativeFilePath2);
					if(source == null)
					{
						source = file.findFileByRelativePath(relativeFilePath3);
					}
					if(source != null)
					{
						PsiFile psiSource = jsClass.getManager().findFile(source);
						if(psiSource instanceof XmlFile)
						{
							return XmlBackedJSClassImpl.getXmlBackedClass((XmlFile) psiSource);
						}
					}
				}
			}
		}
		return null;
	}

	static String getQName(final JSNamedElement element)
	{
		final PsiElement node = element.getNameIdentifier();
		final String name = node != null ? node.getText() : null;
		PsiElement responsibleParent = element.getParent();

		if(responsibleParent instanceof JSVarStatement)
		{
			responsibleParent = responsibleParent.getParent();
		}

		if(responsibleParent instanceof JSPackageStatement && name != null)
		{
			final String packageName = ((JSPackageStatement) responsibleParent).getQualifiedName();
			if(!StringUtil.isEmpty(packageName))
			{
				return packageName.concat(".").concat(name);
			}
		}
		return name;
	}

	public static
	@Nullable
	String getTypeFromAnnotationParameter(@NotNull JSAttributeList attributeList, @NotNull String annotationName, @Nullable String annotationParameter)
	{
		String arrayType = null;
		final JSAttribute[] byName = attributeList.getAttributesByName(annotationName);
		if(byName.length > 0)
		{
			final JSAttributeNameValuePair jsAttributeNameValuePair = byName[0].getValueByName(annotationParameter);
			arrayType = jsAttributeNameValuePair != null ? jsAttributeNameValuePair.getSimpleValue() : null;
		}
		return arrayType;
	}

	/**
	 * @see <code>QUALIFIED_NAME_HASHING_STRATEGY</code>
	 */
	static boolean isTheSameClass(final PsiElement typeSource, final JSClass jsClass)
	{
		if(typeSource == jsClass)
		{
			return true;
		}
		if(typeSource instanceof JSClass && jsClass != null)
		{
			final String qName = ((JSClass) typeSource).getQualifiedName();
			return qName != null && qName.equals(jsClass.getQualifiedName());
		}
		return false;
	}

	public static final TObjectHashingStrategy<JSQualifiedNamedElement> QUALIFIED_NAME_HASHING_STRATEGY = new TObjectHashingStrategy<JSQualifiedNamedElement>()
	{
		@Override
		public int computeHashCode(final JSQualifiedNamedElement object)
		{
			return object == null || object.getQualifiedName() == null ? 0 : object.getQualifiedName().hashCode();
		}

		@Override
		public boolean equals(final JSQualifiedNamedElement o1, final JSQualifiedNamedElement o2)
		{
			return Comparing.equal(o1.getQualifiedName(), o2.getQualifiedName());
		}
	};

	static void doRenameParentDirectoryIfNeeded(VirtualFile file, String name, Object requestor) throws IOException
	{
		VirtualFile directory = file.isDirectory() ? file : file.getParent();
		if(!name.equals(directory.getName()))
		{
			directory.rename(requestor, name);
		}
	}

	static
	@Nullable
	String getQNameForMove(@NotNull PsiElement targetElement, PsiElement elementToBind)
	{
		String qName = null;
		Project project = targetElement.getProject();

		if(elementToBind instanceof PsiFile)
		{
			String newName = ((PsiNamedElement) elementToBind).getName();
			int index = newName.lastIndexOf('.');
			if(index != -1)
			{
				newName = newName.substring(0, index);
			}
			VirtualFile elementToBindFile = elementToBind.getContainingFile().getVirtualFile();

			String packageName = JSResolveUtil.getExpectedPackageNameFromFile(elementToBindFile, project, false);

			if(targetElement instanceof JSReferenceExpression)
			{
				JSExpression qualifier = ((JSReferenceExpression) targetElement).getQualifier();
				String targetElementPackageName = qualifier != null ? qualifier.getText() : JSResolveUtil.getExpectedPackageNameFromFile(targetElement.getContainingFile().getVirtualFile(), project,
						false);
				if(!differentPackageName(targetElementPackageName, packageName))
				{
					return null;
				}
				if(qualifier == null)
				{
					if(!(targetElement.getParent() instanceof JSImportStatement))
					{
						return null;
					}
				}
			}

			qName = packageName.isEmpty() ? newName : packageName + "." + newName;
		}
		else if(elementToBind instanceof PsiDirectoryContainer)
		{
			PsiDirectory[] directories = ((PsiDirectoryContainer) elementToBind).getDirectories(targetElement.getResolveScope());
			if(directories.length > 0)
			{
				qName = JSResolveUtil.getExpectedPackageNameFromFile(directories[0].getVirtualFile(), project, false);
			}
		}
		return qName;
	}

	public static boolean differentPackageName(final String s, final String expectedPackageNameFromFile)
	{
		final boolean sIsEmpty = isEmpty(s);
		final boolean expectedIsEmpty = isEmpty(expectedPackageNameFromFile);

		return (sIsEmpty && !expectedIsEmpty) || (!sIsEmpty && (expectedIsEmpty || !s.equals(expectedPackageNameFromFile)));
	}

	public static boolean isEmpty(final String expectedPackageNameFromFile)
	{
		return expectedPackageNameFromFile == null || expectedPackageNameFromFile.length() == 0;
	}
}
