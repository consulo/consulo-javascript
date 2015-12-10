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

import gnu.trove.THashSet;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.javascript.documentation.JSDocumentationProcessor;
import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlDocument;
import com.intellij.psi.xml.XmlFile;
import com.intellij.psi.xml.XmlTag;

/**
 * @by maxim.mossienko, yole
 */
public class JSSymbolUtil
{
	@NonNls
	private static final String PROTOTYPE_FIELD_NAME = "prototype";
	@NonNls
	private static final String EXTEND_PROPERTY_NAME = "extend";
	@NonNls
	private static final String MEMBERS_PROPERTY_NAME = "members";
	@NonNls
	private static final String STATICS_PROPERTY_NAME = "statics";
	@NonNls
	private static final String J_QUERY_VAR_NAME = "jQuery";
	@NonNls
	private static final String FN_FUN_NAME = "fn";

	interface JavaScriptSymbolProcessingHost
	{
		boolean isCurrentItemDeprecated();

		String getCurrentItemType();

		JSAttributeList.AccessType getAccessType();

		void resetState();
	}

	interface JavaScriptSymbolProcessorEx extends JavaScriptSymbolProcessor
	{
		void setProcessingHost(JavaScriptSymbolProcessingHost processingHost);
	}

	public static void visitSymbols(final PsiFile file, JSNamespace namespace, final JavaScriptSymbolProcessorEx symbolVisitor)
	{
		file.acceptChildren(new JSSymbolVisitor(namespace, symbolVisitor, file));
	}

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

	public static String[] buildNameIndexArray(final JSElement _expr, JSNamespace contextNamespace, final JavaScriptIndex index)
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
			visitReferenceExpressionComponentsInRootFirstOrder(expr, contextNamespace, new ReferenceExpressionProcessor()
			{
				@Override
				public void processNamespace(JSNamespace ns)
				{
					nameComponents.add(ns.getNameId());
				}

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
		void processNamespace(JSNamespace ns);

		void processExpression(JSReferenceExpression expr);

		void processUnresolvedThis();

		boolean isTopLevel(final JSReferenceExpression expression);
	}

	private static void visitNamespaceComponentsInRootFirstOrder(JSNamespace namespace, ReferenceExpressionProcessor processor)
	{
		final JSNamespace parentNs = namespace.getParent();
		if(parentNs != null)
		{
			visitNamespaceComponentsInRootFirstOrder(parentNs, processor);
		}
		processor.processNamespace(namespace);
	}

	private static void visitReferenceExpressionComponentsInRootFirstOrder(JSReferenceExpression expr, final JSNamespace contextNamespace,
			ReferenceExpressionProcessor processor)
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
			visitReferenceExpressionComponentsInRootFirstOrder((JSReferenceExpression) qualifier, contextNamespace, processor);
		}

		if(qualifier instanceof JSThisExpression)
		{
			if(contextNamespace != null)
			{
				visitNamespaceComponentsInRootFirstOrder(contextNamespace, processor);
			}
			else
			{
				processor.processUnresolvedThis();
			}
		}

		final String refName = expr.getReferencedName();

		if(refName != null && (!refName.equals(PROTOTYPE_FIELD_NAME) || processor.isTopLevel(expr)))
		{
			processor.processExpression(expr);
		}
	}

	private static class JSSymbolVisitor extends JSElementVisitor implements ReferenceExpressionProcessor, JSDocumentationProcessor,
			JavaScriptSymbolProcessingHost
	{
		private final JavaScriptSymbolProcessorEx mySymbolVisitor;
		private JSNamespace myThisNamespace;
		private JSNamespace myNamespace;
		private final JSNamespace myFileNamespace;
		private boolean myInsideWithStatement;
		private JSFunction myFunction;
		private JSClass myClazz;
		private JavaScriptIndex myIndex;
		private JSTypeEvaluateManager myTypeEvaluateManager;

		private JSNamespace currentNamespace;
		private JSElement topExpression;
		private JSReferenceExpression currentlyDefinedTypeReference;
		private JSElement currentlyDefinedTypeInstance;
		private PsiComment myComment;

		private boolean myDeprecated;
		private String myType;
		private JSAttributeList.AccessType myAccessType;
		private static final
		@NonNls
		String YAHOO_NAME = "YAHOO";
		private PsiFile myFile;
		@NonNls
		private static final String DOJO_NAME = "dojo";
		private String myClassName;
		private String myExtendsName;
		private final boolean myNamedTagsAreMembersOfDocument;

		public JSSymbolVisitor(JSNamespace namespace, final JavaScriptSymbolProcessorEx symbolVisitor, PsiFile file)
		{
			myFile = file;
			Project project = file.getProject();
			mySymbolVisitor = symbolVisitor;
			myThisNamespace = myFileNamespace = myNamespace = namespace;
			myIndex = JavaScriptIndex.getInstance(project);
			myTypeEvaluateManager = JSTypeEvaluateManager.getInstance(project);
			symbolVisitor.setProcessingHost(this);

			myNamedTagsAreMembersOfDocument = !JavaScriptSupportLoader.isFlexMxmFile(myFile) && (!(myFile instanceof XmlFile) || !XmlBackedJSClassImpl
					.doProcessAllTags((XmlFile) myFile));
		}

		@Override
		public void processNamespace(JSNamespace ns)
		{
			if(ns.getNameId() == null)
			{
				currentNamespace = myFileNamespace;
			}
			else
			{
				currentNamespace = currentNamespace.getChildNamespace(ns.getNameId());
			}
		}

		@Override
		public void processExpression(JSReferenceExpression expr)
		{
			if(expr != topExpression)
			{
				final JSNamespace jsNamespace = (currentNamespace != null) ? currentNamespace : myFileNamespace;
				String reference = expr.getReferencedName();

				if(myFunction != null && expr.getQualifier() == null)
				{
					final PsiElement element = JSResolveUtil.getLocalVariableRef(myFunction, expr);

					if(element != null)
					{
						if(!(element instanceof JSParameter))
						{
							reference = ""; // members derived from local vars do not go into indexing
						}

						final JSReferenceExpression forClassEx = findReferenceExpressionUsedForClassExtending(expr);
						if(forClassEx != expr)
						{
							currentNamespace = getNestedNsWithName(forClassEx.getText(), jsNamespace);
							return;
						}
					}
				}

				final String referencedNamedId = reference != null ? reference : "";

				currentNamespace = jsNamespace.getChildNamespace(referencedNamedId);
			}
		}

		@Override
		public void processUnresolvedThis()
		{
			currentNamespace = myFileNamespace.getChildNamespace("");
		}

		@Override
		public boolean isTopLevel(final JSReferenceExpression expression)
		{
			return expression == topExpression;
		}

		private JSNamespace findNamespace(JSElement _expr, JSNamespace contextNamespace)
		{
			JSElement nameComponent = findNameComponent(_expr);
			JSReferenceExpression expr = null;

			if(nameComponent instanceof JSVariable)
			{
				return myFileNamespace;
			}

			if(nameComponent instanceof JSReferenceExpression)
			{
				expr = (JSReferenceExpression) nameComponent;
			}

			if(expr != null)
			{
				currentNamespace = null;
				topExpression = _expr;
				visitReferenceExpressionComponentsInRootFirstOrder(expr, contextNamespace, this);
				topExpression = null;
				if(currentNamespace != null)
				{
					return currentNamespace;
				}
			}

			return myFileNamespace;
		}

		@Override
		public void visitComment(final PsiComment comment)
		{
			if(comment.getTokenType() == JSTokenTypes.DOC_COMMENT)
			{
				try
				{
					myComment = comment;
					resetState();
					JSDocumentationUtils.processDocumentationTextFromComment(comment.getNode(), this);
				}
				finally
				{
					myComment = null;
				}
			}
		}

		@Override
		public void resetState()
		{
			myDeprecated = false;
			myType = null;
			myAccessType = JSAttributeList.AccessType.PACKAGE_LOCAL;
			myClassName = null;
			myExtendsName = null;
		}

		@Override
		public void visitJSFunctionDeclaration(final JSFunction node)
		{
			final ASTNode nameNode = node.findNameIdentifier();

			if(nameNode != null)
			{
				final JSNamespace previousNs = myNamespace;
				try
				{
					String nameId = node.getName();
					final PsiElement parentElement = node.getParent();

					final String nameNodeText = nameNode.getText();
					if(parentElement instanceof PsiFile ||
							parentElement instanceof JSClass ||
							parentElement instanceof JSPackageStatement)
					{ // global function declaration

						if(!updateNsFromAttributeList(node))
						{
							return;
						}
						final JSNamespace previousMyNs = myNamespace;
						myNamespace = getNestedNsWithName(nameNodeText, previousMyNs);
						if(previousMyNs == myNamespace && PROTOTYPE_FIELD_NAME.equals(nameNodeText))
						{
							myNamespace = myNamespace.getChildNamespace(nameId);
						}
						mySymbolVisitor.processFunction(myNamespace.getParent(), nameId, node);
					}
					else
					{
						myNamespace = getNestedNsWithName(nameNodeText, myNamespace);
					}

					processFunctionBody(myNamespace, node);
				}
				finally
				{
					myNamespace = previousNs;
				}
			}
		}

		@Override
		public void visitJSVarStatement(final JSVarStatement node)
		{
			node.acceptChildren(this);
		}

		@Override
		public void visitJSVariable(final JSVariable node)
		{
			final ASTNode nameNode = node.findNameIdentifier();
			if(nameNode == null)
			{
				return;
			}
			final String s = node.getName();
			final String nameId = s != null ? s : "";

			if(myFunction == null)
			{
				final JSNamespace previousNs = myNamespace;
				try
				{
					updateNsFromAttributeList(node);
					myNamespace = getNestedNsWithName(nameNode.getText(), myNamespace);
					mySymbolVisitor.processVariable(myNamespace.getParent(), nameId, node);
				}
				finally
				{
					myNamespace = previousNs;
				}
			}

			final JSExpression initializer = node.getInitializer();
			if(initializer != null)
			{
				JSReferenceExpression initializedPrototype = evaluateInitializedPrototype(initializer);

				visitWithNamespace(myNamespace.getChildNamespace(nameId), initializer, false);
				if(initializedPrototype != null)
				{
					currentlyDefinedTypeReference = initializedPrototype;
					currentlyDefinedTypeInstance = node;
				}
			}
		}

		private boolean updateNsFromAttributeList(final JSAttributeListOwner node)
		{
			final JSAttributeList attributeList = node.getAttributeList();
			if(attributeList != null)
			{
				final String ns = attributeList.getNamespace();
				if(ResolveProcessor.AS3_NAMESPACE.equals(ns))
				{
					return false;
				}
				if(ns != null)
				{
					myNamespace = getNestedNsWithName(ns, myNamespace);
				}
				myAccessType = attributeList.getAccessType();
			}
			return true;
		}

		private void processReferenceExpression(JSReferenceExpression element, JSExpression rOperand, JSNamespace suggestedNs)
		{
			final PsiElement parent = element.getParent();
			if(!(parent instanceof JSNamedElement))
			{
				return; // some wrong tree
			}
			JSNamespace namespace = suggestedNs != null ? suggestedNs : findNamespace(element, myThisNamespace);

			if(namespace.getParent() == null && myInsideWithStatement)
			{
				namespace = namespace.getChildNamespace("");
			}

			String name = element.getReferencedName();
			if(name == null)
			{
				return;
			}
			final String nameId = name;

			try
			{
				final JSExpression qualifier = element.getQualifier();

				if((qualifier == null && myFunction != null) || (qualifier instanceof JSThisExpression && myClazz != null))
				{
					final ResolveProcessor processor = new ResolveProcessor(name);
					processor.setLocalResolve(true);

					if(myFunction != null)
					{
						JSAttributeList jsAttributeList = myFunction.getAttributeList();
						processor.setProcessStatics(qualifier == null && jsAttributeList != null && jsAttributeList.hasModifier(JSAttributeList.ModifierType.STATIC));
					}
					else
					{
						processor.setProcessStatics(qualifier == null);
					}

					processor.configureClassScope(myClazz);

					JSResolveUtil.treeWalkUp(processor, element, element.getParent(), element);

					final PsiElement jsElement = processor.getResult();

					if(jsElement != null)
					{
						return;
					}

					if(myClazz != null)
					{
						final JSAttributeList attributeList = myClazz.getAttributeList();
						if(attributeList == null || !attributeList.hasModifier(JSAttributeList.ModifierType.DYNAMIC))
						{
							return;
						}
					}
				}

				mySymbolVisitor.processDefinition(namespace, nameId, (JSNamedElement) parent);
			}
			finally
			{
				if(rOperand != null)
				{
					JSReferenceExpression previousDefinedTypeReference = currentlyDefinedTypeReference;
					visitWithNamespace(getNestedNsWithName(name, namespace), rOperand, rOperand instanceof JSObjectLiteralExpression);
					if(currentlyDefinedTypeReference != previousDefinedTypeReference)
					{
						currentlyDefinedTypeInstance = element;
					}
				}
			}
		}

		@Override
		public void visitJSWithStatement(final JSWithStatement node)
		{
			boolean oldInsideWith = myInsideWithStatement;
			try
			{
				myInsideWithStatement = true;
				super.visitJSWithStatement(node);
			}
			finally
			{
				myInsideWithStatement = oldInsideWith;
			}
		}

		@Override
		public void visitJSDefinitionExpression(final JSDefinitionExpression node)
		{
		}

		@Override
		public void visitElement(final PsiElement element)
		{
			if(element instanceof XmlDocument)
			{
				element.acceptChildren(this);
			}
			else if(element instanceof XmlTag)
			{
				processXmlTag((XmlTag) element);
			}

			super.visitElement(element);
		}

		private void processXmlTag(XmlTag element)
		{
			if(element.getAttributeValue("name") != null && !JavaScriptSupportLoader.isFlexMxmFile(element.getContainingFile()))
			{
				JSNamespace ns = myNamespace;
				if(ns == myFileNamespace && myNamedTagsAreMembersOfDocument)
				{
					ns = myFileNamespace.getChildNamespace("document");
				}
				String nameId = element.getAttributeValue("name");
				mySymbolVisitor.processTag(ns, nameId, element, "name");
				visitWithNamespace(ns.getChildNamespace(nameId), element, true);
			}
			else
			{
				String id = element.getAttributeValue("id");
				if(id != null)
				{
					mySymbolVisitor.processTag(myFileNamespace, id, element, "id");
				}

				//name = element.getAttributeValue("class");
				//if (name != null) {
				//  int nameId = myIndex.getIndexOf(name);
				//  mySymbolVisitor.processTag(myNamespace, nameId, element, "class");
				//}
				element.acceptChildren(this);
			}
		}

		@Override
		public void visitJSElement(JSElement element)
		{
			element.acceptChildren(this);
		}

		@Override
		public void visitJSObjectLiteralExpression(final JSObjectLiteralExpression node)
		{
			JSNamespace candidateNs = null;
			JSElement qualifyingExpression = null;
			JSNamespace namespace = myNamespace;

			if(namespace.getParent() == null)
			{
				// Find some ns that is used for extending
				final PsiElement parent = node.getParent();
				if(parent instanceof JSArgumentList)
				{
					qualifyingExpression = findQualifyingExpressionFromArgumentList((JSArgumentList) parent);

					if(qualifyingExpression != null)
					{
						candidateNs = getNestedNsWithName(JSResolveUtil.ContextResolver.getQualifierOfExprAsString(qualifyingExpression), myFileNamespace);
					}
				}

				if(candidateNs == null)
				{
					candidateNs = namespace.getChildNamespace("");
				}
				namespace = candidateNs;
			}

			visitWithNamespace(namespace, node, true);

			if(qualifyingExpression != null && qualifyingExpression.getParent() instanceof JSArrayLiteralExpression)
			{
				for(JSExpression expr : ((JSArrayLiteralExpression) qualifyingExpression.getParent()).getExpressions())
				{
					if(expr.getClass() != qualifyingExpression.getClass() || expr == qualifyingExpression)
					{
						continue;
					}
					candidateNs = getNestedNsWithName(JSResolveUtil.ContextResolver.getQualifierOfExprAsString(expr), myFileNamespace);
					visitWithNamespace(candidateNs, node, true);
				}
			}
		}

		private JSNamespace findNsForExpr(final JSExpression expr)
		{
			if(expr instanceof JSLiteralExpression)
			{
				return getNestedNsWithName(StringUtil.stripQuotesAroundValue(expr.getText()), myFileNamespace);
			}

			JSNamespace candidateNs = findNamespace(expr, null);

			if(candidateNs != null)
			{
				String name = null;
				if(expr instanceof JSReferenceExpression)
				{
					name = ((JSReferenceExpression) expr).getReferencedName();
				}

				if(name != null)
				{
					candidateNs = getNestedNsWithName(name, candidateNs);
				}
			}
			return candidateNs;
		}

		private JSNamespace getNestedNsWithName(String name, JSNamespace contextNs)
		{
			return JSSymbolUtil.getNestedNsWithName(name, contextNs, myIndex);
		}

		private void visitWithNamespace(final JSNamespace namespace, final PsiElement node, boolean fromChildren)
		{
			final JSNamespace previousNamespace = myNamespace;
			try
			{
				myNamespace = namespace;
				if(fromChildren)
				{
					node.acceptChildren(this);
				}
				else
				{
					node.accept(this);
				}
			}
			finally
			{
				myNamespace = previousNamespace;
			}
		}

		@Override
		public void visitJSProperty(final JSProperty node)
		{
			final JSExpression value = node.getValue();

			ASTNode nameIdentifier = node.findNameIdentifier();
			String name = nameIdentifier != null ? StringUtil.stripQuotesAroundValue(nameIdentifier.getText()) : null;
			IElementType type;
			boolean toProcessProperty = (nameIdentifier != null ? ((type = nameIdentifier.getElementType()) != JSTokenTypes.NUMERIC_LITERAL && (type !=
					JSTokenTypes.STRING_LITERAL || isIdentifier(name))) : false);
			final String nameId = name;

			if(value instanceof JSFunction)
			{
				final JSFunction function = (JSFunction) value;
				if(toProcessProperty)
				{
					mySymbolVisitor.processFunction(myNamespace, nameId, function);
				}
				final JSNamespace childNs = nameId != null && toProcessProperty ? myNamespace.getChildNamespace(nameId) : myNamespace;
				processFunctionBody(childNs, function);
			}
			else if(value != null)
			{
				if(value instanceof JSCallExpression || value instanceof JSObjectLiteralExpression)
				{
					if(MEMBERS_PROPERTY_NAME.equals(name) || STATICS_PROPERTY_NAME.equals(name))
					{
						toProcessProperty = false;
					}

					final JSNamespace childNs = nameId != null && toProcessProperty ? myNamespace.getChildNamespace(nameId) : myNamespace;
					visitWithNamespace(childNs, node, true);
				}
				else
				{
					PsiElement grandParent;
					if((EXTEND_PROPERTY_NAME.equals(name) || "Extends".equals(name) || "Implements".equals(name)) &&
							value instanceof JSReferenceExpression &&
							(grandParent = node.getParent().getParent()) instanceof JSArgumentList)
					{
						final JSElement qualifyingExpression = findQualifyingExpressionFromArgumentList((JSArgumentList) grandParent);

						if(qualifyingExpression != null)
						{
							toProcessProperty = false;
							myTypeEvaluateManager.setBaseType(myNamespace, JSResolveUtil.ContextResolver.getQualifierOfExprAsString(qualifyingExpression),
									value.getText());
						}
					}
					value.acceptChildren(this);
				}

				if(toProcessProperty)
				{
					mySymbolVisitor.processProperty(myNamespace, nameId, node);
				}
			}
		}

		private static boolean isIdentifier(String name)
		{
			for(int i = 0; i < name.length(); ++i)
			{
				if(!Character.isJavaIdentifierPart(name.charAt(0)))
				{
					return false;
				}
			}
			return name.length() > 0;
		}

		@Override
		public void visitJSCallExpression(final JSCallExpression node)
		{
			final JSExpression methodExpression = node.getMethodExpression();

			if(methodExpression instanceof JSReferenceExpression && !(node instanceof JSNewExpression))
			{
				final JSReferenceExpression referenceExpression = (JSReferenceExpression) methodExpression;
				final JSExpression qualifier = referenceExpression.getQualifier();
				final @NonNls String calledMethodName = referenceExpression.getReferencedName();
				@NonNls String qualifierText;

				if("call".equals(calledMethodName) &&
						qualifier != null && myFunction != null &&
						!(myFunction instanceof JSFunctionExpression))
				{
					final JSExpression[] jsExpressions = node.getArgumentList().getArguments();

					if(jsExpressions.length == 1)
					{
						for(JSExpression expr : jsExpressions)
						{
							if(expr instanceof JSThisExpression)
							{
								//System.out.println(
								//  myThisNamespace.getQualifiedName(myIndex) + "," + myFunction.getName() + "," + qualifier.getText() + "," +
								//                   namespace.getQualifiedName(myIndex)
								//);
								myTypeEvaluateManager.setBaseType(myThisNamespace, myFunction.getName(), qualifier.getText());
								break;
							}
						}
					}
				}
				else if(("extend".equals(calledMethodName) && (qualifier == null ||
						"Object".equals(qualifierText = qualifier.getText()) ||
						YAHOO_NAME.equals(qualifierText) ||
						"Ext".equals(qualifierText))) || (qualifier == null && myFunction == null))
				{
					JSExpression[] jsExpressions = node.getArgumentList().getArguments();
					if(jsExpressions.length >= 2 &&
							jsExpressions[0] instanceof JSReferenceExpression &&
							!(jsExpressions[1] instanceof JSReferenceExpression))
					{
						// check if it is aaa = extend(bbb, ...)
						if(node.getParent() instanceof JSAssignmentExpression)
						{
							JSExpression assignedTo = ((JSDefinitionExpression) (((JSAssignmentExpression) node.getParent()).getLOperand())).getExpression();
							if(assignedTo instanceof JSReferenceExpression)
							{
								JSExpression[] jsExpressions2 = new JSExpression[jsExpressions.length + 1];
								jsExpressions2[0] = assignedTo;
								System.arraycopy(jsExpressions, 0, jsExpressions2, 1, jsExpressions.length);
								jsExpressions = jsExpressions2;
							}
						}
					}
					if(jsExpressions.length >= 2 &&
							jsExpressions[0] instanceof JSReferenceExpression &&
							jsExpressions[1] instanceof JSReferenceExpression)
					{
						final JSNamespace namespace = findNsForExpr(jsExpressions[0]);
						//System.out.println(
						//  namespace.getQualifiedName(myIndex) + "," + jsExpressions[0].getText() + "," + jsExpressions[1].getText() + "," +
						//                   namespace2.getQualifiedName(myIndex)
						//);
						JSReferenceExpression typeExpression = (JSReferenceExpression) jsExpressions[0];
						if(PROTOTYPE_FIELD_NAME.equals(typeExpression.getReferencedName()))
						{
							final JSExpression typeQualifier = typeExpression.getQualifier();
							typeExpression = (typeQualifier instanceof JSReferenceExpression) ? (JSReferenceExpression) typeQualifier : null;
						}

						if(typeExpression != null)
						{
							currentlyDefinedTypeReference = typeExpression;
							final JSReferenceExpression baseType = evaluateInitializedPrototype(jsExpressions[1]);
							myTypeEvaluateManager.setBaseType(namespace, currentlyDefinedTypeReference.getText(), (baseType != null ? baseType : jsExpressions[1])
									.getText());
						}
					}
				}
				else if("provide".equals(calledMethodName) &&
						qualifier != null &&
						DOJO_NAME.equals(qualifier.getText()))
				{
					final JSExpression[] jsExpressions = node.getArgumentList().getArguments();
					if(jsExpressions.length == 1 && jsExpressions[0] instanceof JSLiteralExpression)
					{
						String namespace = StringUtil.stripQuotesAroundValue(jsExpressions[0].getText());

						doAddNSFromQName(node, namespace);
					}
				}
				else if(("namespace".equals(calledMethodName) || "ns".equals(calledMethodName) || "defineClass".equals(calledMethodName) || "define".equals
						(calledMethodName)) && qualifier != null)
				{
					final JSExpression[] jsExpressions = node.getArgumentList().getArguments();
					for(JSExpression e : jsExpressions)
					{
						if(!(e instanceof JSLiteralExpression))
						{
							continue;
						}

						String namespace = StringUtil.stripQuotesAroundValue(e.getText());
						final @NonNls String YAHOO_PREFIX = YAHOO_NAME + ".";
						if(!namespace.startsWith(YAHOO_PREFIX) && YAHOO_NAME.equals(qualifier.getText()))
						{
							namespace = YAHOO_PREFIX + namespace;
						}
						if(namespace.indexOf('.') == -1)
						{
							mySymbolVisitor.processImplicitNamespace(myFileNamespace, namespace, node, false);
						}
						else
						{
							doAddNSFromQName(node, namespace);
						}
					}
				}
				else if("addProperty".equals(calledMethodName) && qualifier != null)
				{
					final JSExpression[] jsExpressions = node.getArgumentList().getArguments();
					if(jsExpressions.length == 2 && jsExpressions[0] instanceof JSLiteralExpression)
					{
						final JSNamespace namespace = findNsForExpr(qualifier);
						String propertyName = StringUtil.stripQuotesAroundValue(jsExpressions[0].getText());
						String type = jsExpressions[1].getText().toLowerCase();

						if(type.indexOf("read") != -1)
						{
							mySymbolVisitor.processImplicitFunction(namespace, suggestGetterName(propertyName), node);
						}
						if(type.indexOf("write") != -1)
						{
							mySymbolVisitor.processImplicitFunction(namespace, suggestSetterName(propertyName), node);
						}

						mySymbolVisitor.processImplicitVariable(namespace, "_" + propertyName, node);
					}
				}
				else if(("__defineGetter__".equals(calledMethodName) || "__defineSetter__".equals(calledMethodName)) && qualifier != null)
				{
					final JSExpression[] jsExpressions = node.getArgumentList().getArguments();
					if(jsExpressions.length == 2 && jsExpressions[0] instanceof JSLiteralExpression)
					{
						final JSNamespace namespace = findNsForExpr(qualifier);
						String propertyName = StringUtil.stripQuotesAroundValue(jsExpressions[0].getText());

						mySymbolVisitor.processImplicitVariable(namespace, propertyName, node);
					}
				}
				else if("declare".equals(calledMethodName) &&
						qualifier != null &&
						DOJO_NAME.equals(qualifier.getText()))
				{
					final JSExpression[] jsExpressions = node.getArgumentList().getArguments();
					if(jsExpressions.length > 1 && jsExpressions[0] instanceof JSLiteralExpression)
					{
						final JSNamespace ns = findNsForExpr(jsExpressions[0]);

						mySymbolVisitor.processImplicitVariable(ns.getParent(), ns.getNameId(), node);
					}
				}
				else if("alias".equals(calledMethodName) && qualifier != null)
				{
					final JSExpression[] jsExpressions = node.getArgumentList().getArguments();
					if(jsExpressions.length == 2 &&
							jsExpressions[0] instanceof JSLiteralExpression &&
							jsExpressions[1] instanceof JSLiteralExpression)
					{                  // TODO: aliasing could be processed better
						doAddNSFromQName(node, qualifier.getText() + "." + StringUtil.stripQuotesAroundValue(jsExpressions[1].getText()));
					}
				}
			}

			super.visitJSCallExpression(node);
		}

		@NonNls
		private static String suggestSetterName(final String propertyName)
		{
			return "set" + StringUtil.capitalize(propertyName);
		}

		@NonNls
		private static String suggestGetterName(final String propertyName)
		{
			return "get" + StringUtil.capitalize(propertyName);
		}

		private JSNamespace doAddNSFromQName(final PsiElement node, String namespace)
		{
			JSNamespace candidateNs = myFileNamespace;
			final int lastDot;
			lastDot = namespace.lastIndexOf('.');
			String nsName = null;
			if(lastDot != -1)
			{
				nsName = namespace.substring(lastDot + 1);
				namespace = namespace.substring(0, lastDot);
			}
			else
			{
				nsName = namespace;
			}

			if(nsName != null)
			{
				int lastI = 0;
				int i = namespace.indexOf('.');

				while(i != -1)
				{
					final String id = namespace.substring(lastI, i);
					if(candidateNs.findChildNamespace(id) == null)
					{
						mySymbolVisitor.processImplicitNamespace(candidateNs, id, node, false);
					}
					candidateNs = candidateNs.getChildNamespace(id);
					lastI = i + 1;
					i = namespace.indexOf('.', lastI);
				}

				if(i != namespace.length())
				{
					final String id = namespace.substring(lastI, namespace.length());
					if(candidateNs.findChildNamespace(id) == null)
					{
						mySymbolVisitor.processImplicitNamespace(candidateNs, id, node, false);
					}
					candidateNs = candidateNs.getChildNamespace(id);
				}

				mySymbolVisitor.processImplicitNamespace(candidateNs, nsName, node, true);
			}
			return candidateNs;
		}

		@Override
		public void visitJSAssignmentExpression(final JSAssignmentExpression node)
		{
			JSExpression _lOperand = node.getLOperand();
			if(_lOperand instanceof JSDefinitionExpression)
			{
				_lOperand = ((JSDefinitionExpression) _lOperand).getExpression();
			}

			JSNamespace suggestedNamespace = null;

			if(_lOperand instanceof JSReferenceExpression)
			{
				JSReferenceExpression lOperand = (JSReferenceExpression) _lOperand;
				JSExpression lqualifier = lOperand.getQualifier();

				if(lqualifier instanceof JSReferenceExpression && FN_FUN_NAME.equals(((JSReferenceExpression) lqualifier).getReferencedName()))
				{
					JSExpression qualifier = ((JSReferenceExpression) lqualifier).getQualifier();

					if(qualifier instanceof JSReferenceExpression && J_QUERY_VAR_NAME.equals(qualifier.getText()))
					{
						suggestedNamespace = getNestedNsWithName(qualifier.getText(), myThisNamespace);
					}
				}

				JSExpression rOperand = node.getROperand();

				if(currentlyDefinedTypeInstance != null &&
						currentlyDefinedTypeReference != null &&
						lqualifier instanceof JSReferenceExpression &&
						isTheSameExpr(lqualifier, currentlyDefinedTypeInstance))
				{
					lqualifier = currentlyDefinedTypeReference;
					final PsiElement qualifierParent = lqualifier.getParent();
					suggestedNamespace = qualifierParent instanceof JSReferenceExpression ? findNamespace((JSElement) qualifierParent,
							myThisNamespace) : getNestedNsWithName(lqualifier.getText(), myThisNamespace);
				}

				if(rOperand instanceof JSFunction)
				{
					handleFunction(lOperand, rOperand, lqualifier, suggestedNamespace);
				}
				else if(rOperand instanceof JSObjectLiteralExpression)
				{
					JSObjectLiteralExpression literalExpr = (JSObjectLiteralExpression) rOperand;

					if(PROTOTYPE_FIELD_NAME.equals(lOperand.getReferencedName()) && lqualifier instanceof JSReferenceExpression)
					{
						final JSNamespace namespace = suggestedNamespace != null ? suggestedNamespace : findNamespace(lOperand, null);

						for(PsiElement el = literalExpr.getFirstChild(); el != null; el = el.getNextSibling())
						{
							if(el instanceof JSProperty)
							{
								final JSProperty prop = (JSProperty) el;
								final JSExpression expression = prop.getValue();
								final String s = prop.getName();
								final String nameId = s != null ? s : "";

								if(expression instanceof JSFunction)
								{
									mySymbolVisitor.processFunction(namespace, nameId, (JSFunction) expression);
									processFunctionBody(namespace.getChildNamespace(nameId), (JSFunction) expression);
								}
								else if(expression != null)
								{
									mySymbolVisitor.processProperty(namespace, nameId, prop);
									expression.accept(this);
								}
							}
							else if(el instanceof PsiComment)
							{
								el.accept(this);
							}
						}
					}
					else
					{
						processReferenceExpression(lOperand, rOperand, suggestedNamespace);
					}
				}
				else if(rOperand instanceof JSCallExpression)
				{
					JSExpression methodExpression = null;

					if(rOperand instanceof JSNewExpression)
					{
						methodExpression = ((JSNewExpression) rOperand).getMethodExpression();
					}

					if(PROTOTYPE_FIELD_NAME.equals(lOperand.getReferencedName()) && lqualifier instanceof JSReferenceExpression)
					{

						if(!(rOperand instanceof JSNewExpression))
						{
							for(JSExpression expr : ((JSCallExpression) rOperand).getArgumentList().getArguments())
							{
								if(expr instanceof JSNewExpression)
								{
									methodExpression = ((JSNewExpression) expr).getMethodExpression();
									break;
								}
							}
						}

						if(methodExpression instanceof JSReferenceExpression)
						{
							final JSNamespace subTypeNS = suggestedNamespace != null ? suggestedNamespace : findNamespace(lOperand, myThisNamespace);
							final String superType = methodExpression.getText();

							myTypeEvaluateManager.setBaseType(subTypeNS, lqualifier.getText(), superType);
						}
					}

					processReferenceExpression(lOperand, rOperand, suggestedNamespace);
				}
				else
				{
					processReferenceExpression(lOperand, rOperand, suggestedNamespace);
					final JSReferenceExpression initializedPrototype = evaluateInitializedPrototype(rOperand);
					if(lqualifier == null && initializedPrototype != null)
					{
						currentlyDefinedTypeReference = initializedPrototype;
						currentlyDefinedTypeInstance = lOperand;
					}
				}
			}
			else
			{
				int a = 1;
			}
		}

		private void handleFunction(final JSReferenceExpression lOperand, final JSExpression rOperand, final JSExpression lqualifier,
				final JSNamespace suggestedNamespace)
		{
			final JSNamespace namespace = suggestedNamespace != null ? suggestedNamespace : findNamespace(lOperand, myThisNamespace);
			final String s = lOperand.getReferencedName();
			final String nameId = s != null ? s : "";
			final JSNamespace childNamespace = namespace.getChildNamespace(nameId);

			if(lqualifier instanceof JSReferenceExpression)
			{
				final JSFunction function = (JSFunction) rOperand;
				mySymbolVisitor.processFunction(namespace, nameId, function);
				processFunctionBody(childNamespace, function);
			}
			else
			{
				mySymbolVisitor.processFunction(namespace, nameId, (JSFunction) rOperand);
				processFunctionBody(childNamespace, (JSFunction) rOperand);
			}
		}

		private static boolean isTheSameExpr(final JSExpression lqualifier, JSElement currentlyDefinedTypeInstance)
		{
			if(lqualifier == currentlyDefinedTypeInstance)
			{
				return true;
			}

			if(currentlyDefinedTypeInstance instanceof JSVariable && lqualifier instanceof JSReferenceExpression)
			{
				return ((JSReferenceExpression) lqualifier).getReferencedName().equals(((JSVariable) currentlyDefinedTypeInstance).getName());
			}
			if(lqualifier instanceof JSReferenceExpression && currentlyDefinedTypeInstance instanceof JSReferenceExpression)
			{

				final JSReferenceExpression lqualifierRefExpr = ((JSReferenceExpression) lqualifier);
				final String name = lqualifierRefExpr.getReferencedName();
				final JSReferenceExpression currentlyDefinedTypeInstanceRefExpr = ((JSReferenceExpression) currentlyDefinedTypeInstance);
				final String name2 = currentlyDefinedTypeInstanceRefExpr.getReferencedName();

				return name != null && name.equals(name2) && isTheSameExpr(lqualifierRefExpr.getQualifier(), currentlyDefinedTypeInstanceRefExpr.getQualifier());
			}
			return false;
		}

		@Override
		public void visitJSNamespaceDeclaration(final JSNamespaceDeclaration namespaceDeclaration)
		{
			final String nsName = namespaceDeclaration.getName();
			JSNamespace ns = myNamespace;
			JSNamespace thisns = myThisNamespace;
			try
			{
				mySymbolVisitor.processNamespace(myNamespace, nsName, namespaceDeclaration);
			}
			finally
			{
				myNamespace = ns;
				myThisNamespace = thisns;
			}
		}

		@Override
		public void visitJSPackageStatement(final JSPackageStatement packageStatement)
		{
			final String name = packageStatement.getQualifiedName();
			JSNamespace ns = myNamespace;
			try
			{
				myNamespace = name != null ? getNestedNsWithName(name, myFileNamespace) : myFileNamespace;
				super.visitJSPackageStatement(packageStatement);
			}
			finally
			{
				myNamespace = ns;
			}
		}

		@Override
		public void visitJSClass(final JSClass clazz)
		{
			ASTNode node = clazz.findNameIdentifier();

			JSNamespace ns = myNamespace;
			JSNamespace thisns = myThisNamespace;
			JSClass saveclazz = myClazz;
			try
			{
				final String fullName = node != null ? node.getText() : null;
				myNamespace = node != null ? getNestedNsWithName(fullName, myNamespace) : myNamespace;
				updateNsFromAttributeList(clazz);
				final String className = clazz.getName();
				mySymbolVisitor.processClass(myNamespace.getParent(), className, clazz);
				myThisNamespace = myNamespace;
				myClazz = clazz;

				super.visitJSClass(clazz);

				final JSReferenceList extendsList = clazz.getExtendsList();
				if(extendsList != null)
				{
					final JSReferenceExpression[] referenceExpressions = extendsList.getExpressions();

					if(referenceExpressions != null)
					{
						for(JSReferenceExpression expr : referenceExpressions)
						{
							//System.out.println(
							//  myThisNamespace.getQualifiedName(myIndex) + "," + myFunction.getName() + "," + qualifier.getText() + "," +
							//                   namespace.getQualifiedName(myIndex)
							//);
							myTypeEvaluateManager.setBaseType(myThisNamespace, myThisNamespace.getQualifiedName(myIndex), expr.getText());
						}
					}
				}
			}
			finally
			{
				myNamespace = ns;
				myThisNamespace = thisns;
				myClazz = saveclazz;
			}
		}

		@Override
		public void visitJSFunctionExpression(final JSFunctionExpression node)
		{
			processFunctionBody(myNamespace, node.getFunction());
		}

		private void processFunctionBody(final JSNamespace namespace, final JSFunction node)
		{
			ProgressManager.getInstance().checkCanceled();

			final JSNamespace previousNamespace = myNamespace;
			final JSFunction previousFunction = myFunction;
			final JSNamespace previousThisNamespace = myThisNamespace;
			final JSReferenceExpression previouslyDefinedTypeReference = currentlyDefinedTypeReference;
			final JSElement previouslyDefinedTypeInstance = currentlyDefinedTypeInstance;

			myThisNamespace = namespace;
			currentlyDefinedTypeReference = null;
			currentlyDefinedTypeInstance = null;

			final PsiElement nodeParent = node.getParent();

			if(node instanceof JSFunctionExpression)
			{
				if(nodeParent instanceof JSProperty)
				{
					myThisNamespace = myThisNamespace.getParent();
				}
				else
				{
					final JSResolveUtil.ContextResolver contextResolver = new JSResolveUtil.ContextResolver(node.getFirstChild());
					String qualifierString = contextResolver.getQualifierAsString();
					if(qualifierString != null)
					{
						myThisNamespace = getNestedNsWithName(qualifierString, myFileNamespace);
					}
				}
			}
			else
			{
				if(nodeParent instanceof JSClass)
				{
					myThisNamespace = previousThisNamespace;
				}
			}

			myNamespace = namespace;
			myFunction = node;
			try
			{
				for(JSSourceElement srcElement : node.getBody())
				{
					srcElement.acceptChildren(this);
				}
			}
			finally
			{
				myNamespace = previousNamespace;
				myFunction = previousFunction;
				myThisNamespace = previousThisNamespace;
				currentlyDefinedTypeInstance = previouslyDefinedTypeInstance;
				currentlyDefinedTypeReference = previouslyDefinedTypeReference;
			}
		}

		@Override
		public boolean needsPlainCommentData()
		{
			return false;
		}

		@Override
		public boolean onCommentLine(@NotNull String line)
		{
			return true;
		}

		@Override
		public boolean onPatternMatch(@NotNull MetaDocType type, @Nullable String matchName, @Nullable final String matchValue,
				@Nullable String remainingLineContent, @NotNull final String line, final String patternMatched)
		{
			if(type == MetaDocType.NAMESPACE && matchName != null)
			{
				doAddNSFromQName(myComment, matchName);
			}
			else if(type == MetaDocType.DEPRECATED)
			{
				myDeprecated = true;
			}
			else if(type == MetaDocType.PUBLIC)
			{
				myAccessType = JSAttributeList.AccessType.PUBLIC;
			}
			else if(type == MetaDocType.PRIVATE)
			{
				myAccessType = JSAttributeList.AccessType.PRIVATE;
			}
			else if(type == MetaDocType.PROTECTED)
			{
				myAccessType = JSAttributeList.AccessType.PROTECTED;
			}
			else if(type == MetaDocType.EXTENDS)
			{
				if(myClassName != null)
				{
					if(remainingLineContent != null)
					{
						myTypeEvaluateManager.setBaseType(getNestedNsWithName(myClassName, myFileNamespace), myClassName, remainingLineContent);
					}
					else
					{
						myExtendsName = remainingLineContent;
					}
				}
			}
			else if(type == MetaDocType.CLASS)
			{
				myClassName = matchName;
			}
			else if(type == MetaDocType.FIELD)
			{
				doAddNSFromQName(myComment, matchName + "." + remainingLineContent);
			}
			return true;
		}

		@Override
		public boolean isCurrentItemDeprecated()
		{
			return myDeprecated;
		}

		@Override
		public String getCurrentItemType()
		{
			return myType;
		}

		@Override
		public JSAttributeList.AccessType getAccessType()
		{
			if(myAccessType == null)
			{
				myAccessType = JSAttributeList.AccessType.PACKAGE_LOCAL;
			}
			return myAccessType;
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
						visited = new THashSet<String>();
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

	public static JSNamespace getNestedNsWithName(String name, JSNamespace contextNs, JavaScriptIndex myIndex)
	{
		if(!PROTOTYPE_FIELD_NAME.equals(name))
		{
			int lastI = 0;
			int i = name.indexOf('.');

			while(i != -1)
			{
				contextNs = contextNs.getChildNamespace(name.substring(lastI, i));
				lastI = i + 1;
				i = name.indexOf('.', lastI);
			}

			if(i != name.length())
			{
				contextNs = contextNs.getChildNamespace(name.substring(lastI, name.length()));
			}
		}
		return contextNs;
	}

	public static
	@Nullable
	JSNamespace findNestedNsWithName(String name, JSNamespace contextNs, JavaScriptIndex myIndex)
	{
		if(!PROTOTYPE_FIELD_NAME.equals(name))
		{
			int lastI = 0;
			int i = name.indexOf('.');

			while(i != -1)
			{
				contextNs = contextNs.findChildNamespace(name.substring(lastI, i));
				if(contextNs == null)
				{
					return null;
				}
				lastI = i + 1;
				i = name.indexOf('.', lastI);
			}

			if(i != name.length())
			{
				contextNs = contextNs.findChildNamespace(name.substring(lastI, name.length()));
			}
		}
		return contextNs;
	}
}
