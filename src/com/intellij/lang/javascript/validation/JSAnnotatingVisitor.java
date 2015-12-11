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

package com.intellij.lang.javascript.validation;

import gnu.trove.THashMap;
import gnu.trove.THashSet;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.PropertyKey;
import com.intellij.codeInsight.CodeInsightUtilBase;
import com.intellij.codeInsight.daemon.EmptyResolveMessageProvider;
import com.intellij.codeInsight.daemon.impl.quickfix.RenameFileFix;
import com.intellij.codeInsight.intention.IntentionAction;
import com.intellij.codeInsight.template.Template;
import com.intellij.codeInsight.template.TemplateManager;
import com.intellij.codeInsight.template.impl.MacroCallNode;
import com.intellij.codeInsight.template.macro.CompleteMacro;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.LocalQuickFixProvider;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.ASTNode;
import com.intellij.lang.annotation.Annotation;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.Annotator;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.impl.JSPackageStatementImpl;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.psi.util.JSUtils;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiLanguageInjectionHost;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.xml.XmlTagChild;
import com.intellij.util.IncorrectOperationException;

/**
 * @by max, maxim.mossienko
 */
public class JSAnnotatingVisitor extends JSElementVisitor implements Annotator
{
	private AnnotationHolder myHolder;

	@Override
	public synchronized void annotate(PsiElement psiElement, AnnotationHolder holder)
	{
		myHolder = holder;
		psiElement.accept(this);
		myHolder = null;
	}

	@Override
	public void visitJSAttributeNameValuePair(final JSAttributeNameValuePair attributeNameValuePair)
	{
		checkReferences(attributeNameValuePair, ProblemKind.ERROR);
	}

	@Override
	public void visitJSIncludeDirective(final JSIncludeDirective includeDirective)
	{
		checkReferences(includeDirective, ProblemKind.ERROR);
	}

	@Override
	public void visitJSLiteralExpression(JSLiteralExpression node)
	{
		checkReferences(node, ProblemKind.ERROR);
	}

	static enum ProblemKind
	{
		ERROR, WARNING, INFO
	}

	private void checkReferences(final PsiElement includeDirective, ProblemKind kind)
	{
		for(PsiReference ref : includeDirective.getReferences())
		{
			if(!ref.isSoft() && hasBadResolve(ref))
			{
				final TextRange elementRange = ref.getElement().getTextRange();
				final TextRange textRange = ref.getRangeInElement();

				final TextRange range = new TextRange(elementRange.getStartOffset() + textRange.getStartOffset(), elementRange.getStartOffset() + textRange
						.getEndOffset());
				final String message = MessageFormat.format(((EmptyResolveMessageProvider) ref).getUnresolvedMessagePattern(), ref.getCanonicalText());
				Annotation annotation = kind == ProblemKind.ERROR ? myHolder.createErrorAnnotation(range, message) : kind == ProblemKind.WARNING ? myHolder
						.createWarningAnnotation(range, message) : myHolder.createInfoAnnotation(range, message);

				if(ref instanceof LocalQuickFixProvider)
				{
					for(LocalQuickFix fix : ((LocalQuickFixProvider) ref).getQuickFixes())
					{
						if(fix instanceof IntentionAction)
						{
							annotation.registerFix((IntentionAction) fix, new TextRange(annotation.getStartOffset(), annotation.getEndOffset()));
						}
					}
				}
			}
		}
	}

	private boolean hasBadResolve(final PsiReference ref)
	{
		if(ref instanceof PsiPolyVariantReference)
		{
			return ((PsiPolyVariantReference) ref).multiResolve(false).length == 0;
		}
		return ref.resolve() == null;
	}

	@Override
	public void visitJSCallExpression(final JSCallExpression node)
	{
		final JSExpression methodExpression = node.getMethodExpression();

		if(methodExpression instanceof JSLiteralExpression)
		{
			myHolder.createErrorAnnotation(methodExpression, JavaScriptBundle.message("javascript.parser.message.expected.function.name"));
		}
	}

	@Override
	public void visitJSDocTagValue(final JSDocTagValue tagValue)
	{
		checkReferences(tagValue, ProblemKind.WARNING);
	}

	@Override
	public void visitJSDocTag(final JSDocTag tagValue)
	{
		checkReferences(tagValue, ProblemKind.WARNING);
	}

	@Override
	public void visitJSReferenceList(final JSReferenceList referenceList)
	{
		final JSClass jsClass = (JSClass) referenceList.getParent();
		if(JSResolveUtil.isArtificialClassUsedForReferenceList(jsClass))
		{
			return; // implements="MyInterface" in mxml has artificial class created
		}

		final boolean withinExtends = jsClass.getExtendsList() == referenceList;
		final boolean withinImplements = jsClass.getImplementsList() == referenceList;

		if(withinImplements && jsClass.isInterface())
		{
			myHolder.createErrorAnnotation(referenceList, JavaScriptBundle.message("javascript.validation.message.implements.for.interface.not.allowed"));
			return;
		}

		final Map<String, JSReferenceExpression> nameToExprMap = new THashMap<String, JSReferenceExpression>();

		final JSReferenceExpression[] referenceExpressions = referenceList.getExpressions();
		if(referenceExpressions != null)
		{
			for(JSReferenceExpression expr : referenceExpressions)
			{
				final String s = expr.getReferencedName();
				if(s != null)
				{
					nameToExprMap.put(s, expr);
				}
			}
		}

		for(JSClass clazz : referenceList.getReferencedClasses())
		{
			final boolean b = clazz.isInterface();
			final JSReferenceExpression expr = nameToExprMap.get(clazz.getName());

			if(!b && withinImplements)
			{
				myHolder.createErrorAnnotation(expr, JavaScriptBundle.message("javascript.validation.message.interface.name.expected.here"));
			}
			else if(withinExtends && b != jsClass.isInterface())
			{
				myHolder.createErrorAnnotation(expr, JavaScriptBundle.message(!b ? "javascript.validation.message.interface.name.expected.here" : "javascript.validation" +
						".message.class.name.expected.here"));
			}
			if(clazz == jsClass)
			{
				myHolder.createErrorAnnotation(expr, JavaScriptBundle.message("javascript.validation.message.circular.dependency")).registerFix(new RemoveASTNodeFix
						(referenceList.getNode(), "javascript.fix.remove.circular.dependency"));
			}
		}

		if(withinImplements)
		{
			checkImplementedMethods(jsClass, new SimpleErrorReportingClient());
		}
	}

	public interface ErrorReportingClient
	{
		enum ProblemKind
		{
			ERROR, WARNING
		}

		void reportError(final ASTNode nameIdentifier, final String s, ProblemKind kind, final IntentionAction implementMethodsFix);
	}

	public static void checkImplementedMethods(final JSClass jsClass, final ErrorReportingClient reportingClient)
	{
		final JSResolveUtil.CollectMethodsToImplementProcessor implementedMethodProcessor = new ImplementedMethodProcessor(jsClass)
		{
			ImplementMethodsFix implementMethodsFix = null;

			@Override
			protected void addNonimplementedFunction(final JSFunction function)
			{
				final ASTNode node = myJsClass.findNameIdentifier();
				if(node == null)
				{
					return;
				}
				if(implementMethodsFix == null)
				{
					implementMethodsFix = new ImplementMethodsFix(myJsClass);
				}
				implementMethodsFix.addElementToProcess(function);
				reportingClient.reportError(node, JavaScriptBundle.message("javascript.validation.message.interface.method.not.implemented", function.getName(),
						((JSClass) JSResolveUtil.findParent(function)).getQualifiedName()), ErrorReportingClient.ProblemKind.ERROR, implementMethodsFix);
			}

			@Override
			protected void addImplementedFunction(final JSFunction interfaceFunction, final JSFunction implementationFunction)
			{
				final JSAttributeList attributeList = implementationFunction.getAttributeList();
				if(attributeList == null || attributeList.getAccessType() != JSAttributeList.AccessType.PUBLIC)
				{
					final ASTNode node = findElementForAccessModifierError(implementationFunction, attributeList);
					reportingClient.reportError(node, JavaScriptBundle.message("javascript.validation.message.interface.method.invalid.access.modifier"),
							ErrorReportingClient.ProblemKind.ERROR, null   // TODO: quickfix
					);
				}

				final SignatureMatchResult incompatibleSignature = checkCompatibleSignature(implementationFunction, interfaceFunction);

				if(incompatibleSignature != SignatureMatchResult.COMPATIBLE_SIGNATURE)
				{
					PsiElement parent = JSResolveUtil.findParent(implementationFunction);
					if(parent instanceof JSFile)
					{
						parent = JSResolveUtil.getClassReferenceForXmlFromContext(parent);
					}

					if(parent != myJsClass)
					{
						// some parent incorrectly implements method from our interface
						addNonimplementedFunction(interfaceFunction);
						return;
					}

					if(incompatibleSignature == SignatureMatchResult.PARAMETERS_DIFFERS)
					{
						final JSParameterList parameterList = implementationFunction.getParameterList();
						final JSParameterList expectedParameterList = interfaceFunction.getParameterList();
						reportingClient.reportError(parameterList.getNode(), JavaScriptBundle.message("javascript.validation.message.interface.method.invalid.signature",
								expectedParameterList != null ? expectedParameterList.getText() : "()"), ErrorReportingClient.ProblemKind.ERROR, null);  // TODO: quickfix
					}
					else if(incompatibleSignature == SignatureMatchResult.RETURN_TYPE_DIFFERS)
					{
						PsiElement implementationReturnTypeExpr = implementationFunction.getReturnTypeElement();
						PsiElement interfaceReturnTypeExpr = interfaceFunction.getReturnTypeElement();
						reportingClient.reportError(implementationReturnTypeExpr != null ? implementationReturnTypeExpr.getNode() : implementationFunction
								.findNameIdentifier(), JavaScriptBundle.message("javascript.validation.message.interface.method.invalid.signature2",
								interfaceReturnTypeExpr != null ? interfaceReturnTypeExpr.getText() : "*"), ErrorReportingClient.ProblemKind.ERROR, null);  // TODO: quickfix
					}
				}
			}
		};
		JSResolveUtil.processInterfaceMethods(jsClass, implementedMethodProcessor);
	}

	private static ASTNode findElementForAccessModifierError(final @NotNull JSFunction o, final JSAttributeList attributeList)
	{
		if(attributeList != null)
		{
			final PsiElement accessTypeElement = attributeList.findAccessTypeElement();
			if(accessTypeElement != null)
			{
				return accessTypeElement.getNode();
			}
		}
		return o.findNameIdentifier();
	}

	@Override
	public void visitJSAttributeList(JSAttributeList attributeList)
	{
		PsiElement parentForCheckingNsOrAccessModifier = null;

		PsiElement namespaceElement = attributeList.getNamespaceElement();
		PsiElement accessTypeElement = attributeList.findAccessTypeElement();
		PsiElement namespaceOrAccessModifierElement = namespaceElement;

		if(namespaceOrAccessModifierElement == null)
		{
			namespaceOrAccessModifierElement = accessTypeElement;
		}
		else if(accessTypeElement != null)
		{
			myHolder.createErrorAnnotation(namespaceOrAccessModifierElement, JavaScriptBundle.message("javascript.validation.message.use.namespace.reference.or" +
					".access.modifier")).registerFix(new RemoveASTNodeFix(namespaceOrAccessModifierElement.getNode(), "javascript.fix.remove.namespace.reference"));

			myHolder.createErrorAnnotation(accessTypeElement, JavaScriptBundle.message("javascript.validation.message.use.namespace.reference.or.access.modifier"))
					.registerFix(new RemoveASTNodeFix(accessTypeElement.getNode(), "javascript.fix.remove.namespace.reference"));
		}

		if(namespaceOrAccessModifierElement != null)
		{
			parentForCheckingNsOrAccessModifier = JSResolveUtil.findParent(attributeList.getParent());
			if(!(parentForCheckingNsOrAccessModifier instanceof JSClass))
			{
				String typeElementText;
				boolean nodeUnderPackage;

				if(((!(nodeUnderPackage = (parentForCheckingNsOrAccessModifier instanceof JSPackageStatement)) && (!(parentForCheckingNsOrAccessModifier
						instanceof JSFile) || attributeList.getAccessType() != JSAttributeList.AccessType.PACKAGE_LOCAL)) || (!"public".equals(typeElementText =
						namespaceOrAccessModifierElement.getText())) && !"internal".equals(typeElementText)))
				{
					boolean nsRef = namespaceOrAccessModifierElement instanceof JSReferenceExpression;
					myHolder.createErrorAnnotation(namespaceOrAccessModifierElement, JavaScriptBundle.message(nodeUnderPackage ? "javascript.validation.message.access" +
							".modifier.allowed.only.for.package.members" : nsRef ? "javascript.validation.message.namespace.allowed.only.for.class.members" : "javascript" +
							".validation.message.access.modifier.allowed.only.for.class.members")).registerFix(new RemoveASTNodeFix(namespaceOrAccessModifierElement
							.getNode(), nsRef ? "javascript.fix.remove.namespace.reference" : "javascript.fix.remove.access.modifier"));
				}
			}
			else if(((JSClass) parentForCheckingNsOrAccessModifier).isInterface())
			{

				if(attributeList.getAccessType() != JSAttributeList.AccessType.PACKAGE_LOCAL || attributeList.getNode().findChildByType(JSTokenTypes
						.INTERNAL_KEYWORD) != null

						)
				{
					final ASTNode astNode = attributeList.getNode().findChildByType(JSTokenTypes.ACCESS_MODIFIERS);
					final Annotation annotation = myHolder.createErrorAnnotation(astNode, JavaScriptBundle.message("javascript.validation.message.interface.members.cannot" +
							".have.access.modifiers"));

					annotation.registerFix(new RemoveASTNodeFix(astNode, "javascript.fix.remove.access.modifier"));
				}
			}
		}
	}

	@Override
	public void visitJSReferenceExpression(final JSReferenceExpression node)
	{
		final PsiElement parent = node.getParent();

		if(parent instanceof JSNamedElement)
		{
			JSNamedElement namedElement = (JSNamedElement) parent;
			final ASTNode nameIdentifier = namedElement.findNameIdentifier();

			if(nameIdentifier != null && nameIdentifier.getPsi() == node)
			{
				if(parent instanceof JSPackageStatement)
				{
					checkPackageStatement((JSPackageStatement) parent);
				}
				else if(!(parent instanceof JSImportStatement) && parent.getParent() instanceof JSPackageStatement)
				{
					checkNamedObjectIsInCorrespondingFile(namedElement);
				}
				else if(parent instanceof JSFunction)
				{
					JSFunction function = (JSFunction) parent;

					if(function.isConstructor())
					{
						final JSClass clazz;
						if(parent.getParent() instanceof JSClass)
						{
							clazz = (JSClass) parent.getParent();
						}
						else
						{
							assert parent.getParent() instanceof JSFile;
							clazz = JSResolveUtil.getXmlBackedClass((JSFile) parent.getParent());
							assert clazz != null;
						}

						checkMissedSuperCall(node, function, clazz);
					}
					else if(function.isSetProperty())
					{
						String typeString = function.getReturnTypeString();

						if(typeString != null && !"void".equals(typeString))
						{
							// TODO: fix!
							myHolder.createErrorAnnotation(function.getReturnTypeElement(), JavaScriptBundle.message("javascript.validation.message.set.method.should.be.void.or" +
									".without.type"));
						}

						JSParameterList parameterList = function.getParameterList();
						if(parameterList != null && parameterList.getParameters().length != 1)
						{
							// TODO: fix!
							myHolder.createErrorAnnotation(parameterList, JavaScriptBundle.message("javascript.validation.message.set.method.should.have.one.parameter"));
						}
					}
					else if(function.isGetProperty())
					{
						String typeString = function.getReturnTypeString();

						if(typeString == null || "void".equals(typeString))
						{
							// TODO: fix!
							myHolder.createErrorAnnotation(typeString != null ? function.getReturnTypeElement() : nameIdentifier.getPsi(),
									JavaScriptBundle.message("javascript.validation.message.get.method.should.be.valid.type", typeString != null ? typeString : "empty"));
						}

						JSParameterList parameterList = function.getParameterList();
						if(parameterList != null && parameterList.getParameters().length != 0)
						{
							// TODO: fix!
							myHolder.createErrorAnnotation(parameterList, JavaScriptBundle.message("javascript.validation.message.get.method.should.have.no.parameter"));
						}
					}
				}

				if(parent instanceof JSClass)
				{
					final JSClass jsClass = (JSClass) parent;
					final JSFunction constructor = jsClass.findFunctionByName(jsClass.getName());
					if(constructor == null)
					{
						checkMissedSuperCall(node, constructor, jsClass);
					}

					PsiElement clazzParent = jsClass.getParent();
					if(!(clazzParent instanceof JSPackageStatement) && !(clazzParent instanceof JSFile))
					{
						myHolder.createErrorAnnotation(node, JavaScriptBundle.message("javascript.validation.message.nested.classes.are.not.allowed"));
					}
				}
			}
		}

		if(node.getQualifier() == null)
		{
			if(!(parent instanceof JSCallExpression))
			{
				if("arguments".equals(node.getText()))
				{
					JSFunction fun = PsiTreeUtil.getParentOfType(node, JSFunction.class);
					if(fun == null)
					{
						myHolder.createErrorAnnotation(node, JavaScriptBundle.message("javascript.validation.message.arguments.out.of.function"));
					}
					else
					{
						JSParameterList parameterList = fun.getParameterList();
						if(parameterList != null)
						{
							for(JSParameter p : parameterList.getParameters())
							{
								if(p.isRest())
								{
									myHolder.createErrorAnnotation(node, JavaScriptBundle.message("javascript.validation.message.arguments.with.rest.parameter"));
								}
							}
						}
					}
				}
			}
		}
	}

	private void checkMissedSuperCall(JSReferenceExpression node, JSFunction constructor, JSClass jsClass)
	{
		if(jsClass.isInterface())
		{
			return;
		}
		JSFunction nontrivialSuperClassConstructor = getNontrivialSuperClassConstructor(jsClass);

		if(nontrivialSuperClassConstructor != null)
		{
			Annotation annotation = null;

			if(!hasSuperConstructorCall(constructor))
			{
				annotation = myHolder.createErrorAnnotation(node, JavaScriptBundle.message("javascript.validation.message.missed.super.constructor.call"));
			}

			if(annotation != null)
			{
				if(constructor == null)
				{
					annotation.registerFix(new AddConstructorAndSuperInvokationFix(node, nontrivialSuperClassConstructor));
				}
				else
				{
					annotation.registerFix(new AddSuperInvokationFix(node, nontrivialSuperClassConstructor));
				}
			}
		}
	}

	private boolean hasSuperConstructorCall(JSFunction jsFunction)
	{
		if(jsFunction == null)
		{
			return false;
		}
		final JSSourceElement[] body = (jsFunction).getBody();
		final JSStatement[] statements = body.length > 0 ? ((JSBlockStatement) body[0]).getStatements() : JSStatement.EMPTY;
		JSExpression expr;

		for(JSStatement st : statements)
		{
			if(st instanceof JSExpressionStatement &&
					(expr = ((JSExpressionStatement) st).getExpression()) instanceof JSCallExpression &&
					(((JSCallExpression) expr).getMethodExpression()) instanceof JSSuperExpression)
			{
				return true;
			}
		}

		return false;
	}

	public static JSFunction getNontrivialSuperClassConstructor(JSClass clazz)
	{
		final JSClass[] classes = clazz.getSuperClasses();

		if(classes.length > 0)
		{
			final JSFunction constructor = classes[0].findFunctionByName(classes[0].getName());

			if(constructor != null)
			{
				final JSParameter[] jsParameters = constructor.getParameterList().getParameters();
				boolean hasRequiredParameters = false;
				for(JSParameter p : jsParameters)
				{
					if(!p.isRest() && !p.hasInitializer())
					{
						hasRequiredParameters = true;
						break;
					}
				}
				return hasRequiredParameters ? constructor : null;
			}
		}

		return null;
	}

	@Override
	public void visitJSParameterList(JSParameterList node)
	{
		boolean foundRest = false;
		boolean initializerPresent = false;

		for(JSParameter parameter : node.getParameters())
		{
			JSExpression initializer = parameter.getInitializer();
			boolean hasInitializer = initializer != null;

			if(hasInitializer && !initializerPresent)
			{
				initializerPresent = true;
			}
			else if(!hasInitializer && initializerPresent && !parameter.isRest())
			{
				myHolder.createErrorAnnotation(parameter, JavaScriptBundle.message("javascript.validation.message.parameter.should.be.initialized")).registerFix(new
						RemoveASTNodeFix(parameter.getNode(), "javascript.fix.remove.parameter"));
			}
			else if(hasInitializer && parameter.isRest())
			{
				myHolder.createErrorAnnotation(parameter, JavaScriptBundle.message("javascript.validation.message.rest.parameter.should.not.be.initialized"))
						.registerFix(new RemoveASTNodeFix("javascript.fix.remove.initializer", getNodesBefore(initializer, JSTokenTypes.EQ)));
			}

			if(parameter.isRest() && !foundRest)
			{
				foundRest = true;
				PsiElement typeElement = parameter.getTypeElement();
				if(typeElement != null && !"Array".equals(typeElement.getText()))
				{
					myHolder.createErrorAnnotation(typeElement, JavaScriptBundle.message("javascript.validation.message.unexpected.type.for.rest.parameter")).registerFix
							(new RemoveASTNodeFix("javascript.fix.remove.type.reference", getNodesBefore(typeElement, JSTokenTypes.COLON)));
				}
			}
			else if(foundRest)
			{
				myHolder.createErrorAnnotation(parameter, JavaScriptBundle.message("javascript.validation.message.parameter.is.not.allowed.after.rest.parameter"))
						.registerFix(new RemoveASTNodeFix(parameter.getNode(), "javascript.fix.remove.parameter"));
			}
		}
	}

	private static ASTNode[] getNodesBefore(PsiElement initializer, IElementType eq)
	{
		List<ASTNode> nodes = new ArrayList<ASTNode>();
		PsiElement element = initializer.getPrevSibling();
		PsiElement lastElement = element;

		if(element instanceof PsiWhiteSpace)
		{
			nodes.add(element.getNode());
			lastElement = element.getPrevSibling();
		}

		if(lastElement != null && lastElement.getNode().getElementType() == eq)
		{
			nodes.add(lastElement.getNode());
		}

		nodes.add(initializer.getNode());
		return nodes.toArray(new ASTNode[nodes.size()]);
	}

	@Override
	public void visitJSPackageStatement(final JSPackageStatement packageStatement)
	{
		for(PsiElement el = packageStatement.getPrevSibling(); el != null; el = el.getPrevSibling())
		{
			if(!(el instanceof PsiWhiteSpace) && !(el instanceof PsiComment))
			{
				myHolder.createErrorAnnotation(packageStatement.getFirstChild().getNode(), JavaScriptBundle.message("javascript.validation.message.package.shouldbe" +
						".first.statement"));
				break;
			}
		}
		final ASTNode node = packageStatement.findNameIdentifier();
		if(node == null)
		{
			checkPackageStatement(packageStatement);
		}
	}

	@Override
	public void visitJSAssignmentExpression(final JSAssignmentExpression expression)
	{
		JSExpression lExpr = expression.getLOperand();
		if(lExpr == null)
		{
			return;
		}
		if(lExpr instanceof JSDefinitionExpression)
		{
			lExpr = ((JSDefinitionExpression) lExpr).getExpression();
		}

		if(lExpr instanceof JSReferenceExpression)
		{
			PsiElement resolved = ((JSReferenceExpression) lExpr).resolve();
			if(resolved instanceof JSVariable && ((JSVariable) resolved).isConst())
			{
				myHolder.createErrorAnnotation(lExpr, JavaScriptBundle.message("javascript.validation.message.assignment.to.const"));
			}
		}

		if(!JSUtils.isLHSExpression(lExpr))
		{
			myHolder.createErrorAnnotation(lExpr, JavaScriptBundle.message("javascript.validation.message.must.be.lvalue"));
		}
	}

	@Override
	public void visitJSArrayLiteralExpression(final JSArrayLiteralExpression node)
	{
		final PsiElement lastChild = node.getLastChild();
		PsiElement child = lastChild != null ? lastChild.getPrevSibling() : null;
		if(child instanceof PsiWhiteSpace)
		{
			child = child.getPrevSibling();
		}
		ASTNode childNode;

		if(child != null && (childNode = child.getNode()) != null && childNode.getElementType() == JSTokenTypes.COMMA)
		{
			final Annotation annotation = myHolder.createWarningAnnotation(child, JavaScriptBundle.message("javascript.validation.message.unneeded.comma"));
			annotation.registerFix(new RemoveASTNodeFix(childNode, "javascript.validation.message.remove.unneeded.comma.fix"));
		}
	}

	@Override
	public void visitJSTryStatement(final JSTryStatement node)
	{
		final JSCatchBlock[] blocks = node.getAllCatchBlocks();

		if(blocks.length > 1)
		{
			final Set<String> typeToCatch = new THashSet<String>();

			for(JSCatchBlock block : blocks)
			{
				final JSParameter p = block.getParameter();
				if(p == null)
				{
					continue;
				}

				String s = p.getTypeString();
				if(s == null)
				{
					s = "";
				}

				if(typeToCatch.contains(s))
				{
					final Annotation annotation = myHolder.createErrorAnnotation(block, JavaScriptBundle.message("javascript.validation.message.duplicated.catch.block"));
					annotation.registerFix(new RemoveASTNodeFix(block.getNode(), "javascript.validation.message.duplicated.catch.block.fix"));
				}
				else
				{
					typeToCatch.add(s);
				}
			}
		}
	}

	@Override
	public void visitJSVariable(final JSVariable var)
	{
		if(var.isConst() && var.getInitializer() == null)
		{
			JSAttributeList attributeList = var.getAttributeList();
			if(attributeList == null || attributeList.getAttributesByName("Embed").length == 0)
			{
				myHolder.createWarningAnnotation(var, JavaScriptBundle.message("javascript.validation.message.const.variable.without.initializer."));
			}
		}

		if(var.getParent().getParent() instanceof JSPackageStatement)
		{
			checkNamedObjectIsInCorrespondingFile(var);
		}
	}

	@Override
	public void visitJSContinueStatement(final JSContinueStatement node)
	{
		if(node.getStatementToContinue() == null)
		{
			myHolder.createErrorAnnotation(node, JavaScriptBundle.message("javascript.validation.message.continue.without.target"));
		}
	}

	@Override
	public void visitJSBreakStatement(final JSBreakStatement node)
	{
		if(node.getStatementToBreak() == null)
		{
			myHolder.createErrorAnnotation(node, JavaScriptBundle.message("javascript.validation.message.break.without.target"));
		}
	}

	@Override
	public void visitJSThisExpression(final JSThisExpression node)
	{
		checkClassReferenceInStaticContext(node, "javascript.validation.message.this.referenced.from.static.context");
	}

	private void checkClassReferenceInStaticContext(final JSExpression node, @PropertyKey(resourceBundle = JavaScriptBundle.BUNDLE) String key)
	{
		PsiElement element = PsiTreeUtil.getParentOfType(node, JSFunction.class, JSFile.class, JSClass.class, JSObjectLiteralExpression.class,
				XmlTagChild.class);

		if(element instanceof JSFunction)
		{
			final JSFunction function = (JSFunction) element;

			final JSAttributeList attributeList = function.getAttributeList();
			if(attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC))
			{
				myHolder.createErrorAnnotation(node, JavaScriptBundle.message(key));
				return;
			}
		}

		PsiElement elementParent;
		if(node instanceof JSSuperExpression && (element == null || (!((elementParent = element.getParent()) instanceof JSClass) && (!(elementParent
				instanceof JSFile) || elementParent.getContext() == null))))
		{
			myHolder.createErrorAnnotation(node, JavaScriptBundle.message("javascript.validation.message.super.referenced.without.class.instance.context"));
		}
	}

	@Override
	public void visitJSSuperExpression(final JSSuperExpression node)
	{
		checkClassReferenceInStaticContext(node, "javascript.validation.message.super.referenced.from.static.context");
	}

	@Override
	public void visitJSFunctionDeclaration(final JSFunction node)
	{
		final ASTNode nameIdentifier = node.findNameIdentifier();
		if(nameIdentifier == null)
		{
			return;
		}
		PsiElement parent = node.getParent();

		if(parent instanceof JSFile)
		{
			parent = JSResolveUtil.getClassReferenceForXmlFromContext(parent);

			if(parent instanceof JSClass && node.getName().equals(((JSClass) parent).getName()) && JavaScriptSupportLoader.isFlexMxmFile(parent
					.getContainingFile()))
			{
				final Annotation annotation = myHolder.createErrorAnnotation(nameIdentifier, JavaScriptBundle.message("javascript.validation.message.constructor.in.mxml" +
						".is.not.allowed"));

				annotation.registerFix(new RemoveASTNodeFix(node.getNode(), "javascript.fix.remove.constructor"));
			}
		}

		if(parent instanceof JSPackageStatement)
		{
			checkNamedObjectIsInCorrespondingFile(node);
		}

		if(parent instanceof JSClass && !node.isConstructor())
		{
			final JSAttributeList attributeList = node.getAttributeList();
			final JSClass clazz = (JSClass) parent;

			if(attributeList == null || (!attributeList.hasModifier(JSAttributeList.ModifierType.STATIC) && (attributeList.getAccessType() != JSAttributeList
					.AccessType.PRIVATE || attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE))))
			{
				final String qName = clazz.getQualifiedName();
				final boolean hasOverride = attributeList != null ? attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE) : false;

				final Ref<JSFunction> set = new Ref<JSFunction>();
				boolean b = JSResolveUtil.iterateType(node, parent, qName, new JSResolveUtil.OverrideHandler()
				{
					@Override
					public boolean process(final ResolveProcessor processor, final PsiElement scope, final String className)
					{
						if(qName == className || (qName != null && qName.equals(className)))
						{
							return true;
						}
						set.set((JSFunction) processor.getResult());
						if("Object".equals(className))
						{
							if(hasOverride && !attributeList.hasModifier(JSAttributeList.ModifierType.NATIVE))
							{ /*native modifier is written always*/
								final ASTNode astNode = attributeList.getNode().findChildByType(JSTokenTypes.OVERRIDE_KEYWORD);
								final Annotation annotation = myHolder.createErrorAnnotation(astNode, JavaScriptBundle.message("javascript.validation.message.function.override.for" +
										".object.method"));

								annotation.registerFix(new RemoveASTNodeFix(astNode, "javascript.fix.remove.override.modifier"));
							}
							return false;
						}
						else if(!hasOverride)
						{
							final Annotation annotation = myHolder.createErrorAnnotation(nameIdentifier, JavaScriptBundle.message("javascript.validation.message.function" +
									".override.without.override.modifier", className));

							annotation.registerFix(new AddOverrideIntentionAction(node));
						}
						return false;
					}
				});

				if(b && hasOverride)
				{
					final ASTNode astNode = attributeList.getNode().findChildByType(JSTokenTypes.OVERRIDE_KEYWORD);
					final Annotation annotation = myHolder.createErrorAnnotation(astNode, JavaScriptBundle.message("javascript.validation.message.function.override.without" +
							".parent.method"));

					annotation.registerFix(new RemoveASTNodeFix(astNode, "javascript.fix.remove.override.modifier"));
				}

				if(!b && hasOverride)
				{
					final JSFunction override = set.get();
					final JSAttributeList overrideAttrList = override.getAttributeList();
					String overrideNs = null;

					if((overrideAttrList == null && (attributeList.getAccessType() != JSAttributeList.AccessType.PACKAGE_LOCAL)) ||
							(overrideAttrList != null && attributeList.getAccessType() != overrideAttrList.getAccessType()) ||
							overrideAttrList != null && (overrideNs = overrideAttrList.getNamespace()) != null && !overrideNs.equals(attributeList.getNamespace()))
					{
						final Annotation annotation1 = myHolder.createErrorAnnotation(findElementForAccessModifierError(node, attributeList),
								JavaScriptBundle.message("javascript.validation.message.function.override.incompatible.access.modifier",
										overrideNs != null ? overrideNs : (overrideAttrList != null ? overrideAttrList.getAccessType().toString() : JSAttributeList.AccessType
												.PACKAGE_LOCAL.toString()).toLowerCase()));

						// TODO: quickfix
						//annotation.registerFix(
						//    new RemoveASTNodeFix(astNode,"javascript.fix.remove.override.modifier")
						//);
					}

					final SignatureMatchResult incompatibleSignature = checkCompatibleSignature(node, override);

					if(incompatibleSignature == SignatureMatchResult.PARAMETERS_DIFFERS)
					{
						final JSParameterList nodeParameterList = node.getParameterList();
						final JSParameterList overrideParameterList = override.getParameterList();

						final Annotation annotation = myHolder.createErrorAnnotation(nodeParameterList != null ? nodeParameterList.getNode() : node.findNameIdentifier
								(), JavaScriptBundle.message("javascript.validation.message.function.override.incompatible.signature",
								overrideParameterList != null ? overrideParameterList.getText() : "()"));

						// TODO: quickfix
						//annotation.registerFix(
						//    new RemoveASTNodeFix(astNode,"javascript.fix.remove.override.modifier")
						//);
					}
					else if(incompatibleSignature == SignatureMatchResult.RETURN_TYPE_DIFFERS)
					{
						PsiElement returnTypeExpr = node.getReturnTypeElement();
						PsiElement overrideReturnTypeExpr = override.getReturnTypeElement();
						final Annotation annotation = myHolder.createErrorAnnotation(returnTypeExpr != null ? returnTypeExpr.getNode() : node.findNameIdentifier(),
								JavaScriptBundle.message("javascript.validation.message.function.override.incompatible.signature2",
										overrideReturnTypeExpr != null ? overrideReturnTypeExpr.getText() : "*"));

						// TODO: quickfix
						//annotation.registerFix(
						//    new RemoveASTNodeFix(astNode,"javascript.fix.remove.override.modifier")
						//);
					}
				}
			}
		}
	}

	enum SignatureMatchResult
	{
		PARAMETERS_DIFFERS,
		RETURN_TYPE_DIFFERS,
		COMPATIBLE_SIGNATURE
	}

	private static SignatureMatchResult checkCompatibleSignature(final JSFunction fun, final JSFunction override)
	{
		JSParameterList nodeParameterList = fun.getParameterList();
		JSParameterList overrideParameterList = override.getParameterList();
		final JSParameter[] parameters = nodeParameterList != null ? nodeParameterList.getParameters() : JSParameter.EMPTY_ARRAY;
		final JSParameter[] overrideParameters = overrideParameterList != null ? overrideParameterList.getParameters() : JSParameter.EMPTY_ARRAY;

		SignatureMatchResult result = parameters.length != overrideParameters.length ? SignatureMatchResult.PARAMETERS_DIFFERS : SignatureMatchResult
				.COMPATIBLE_SIGNATURE;

		if(result == SignatureMatchResult.COMPATIBLE_SIGNATURE)
		{
			for(int i = 0; i < parameters.length; ++i)
			{
				if(!compatibleType(overrideParameters[i].getTypeString(), parameters[i].getTypeString(), overrideParameterList,
						nodeParameterList) || overrideParameters[i].hasInitializer() != parameters[i].hasInitializer())
				{
					result = SignatureMatchResult.PARAMETERS_DIFFERS;
					break;
				}
			}
		}

		if(result == SignatureMatchResult.COMPATIBLE_SIGNATURE)
		{
			if(!compatibleType(override.getReturnTypeString(), fun.getReturnTypeString(), override, fun))
			{
				result = SignatureMatchResult.RETURN_TYPE_DIFFERS;
			}
		}
		return result;
	}

	private static boolean compatibleType(String overrideParameterType, String parameterType, PsiElement overrideContext, PsiElement funContext)
	{
		// TODO: This should be more accurate
		if(overrideParameterType != null && !overrideParameterType.equals(parameterType))
		{
			parameterType = JSImportHandlingUtil.resolveTypeName(parameterType, funContext);
			overrideParameterType = JSImportHandlingUtil.resolveTypeName(overrideParameterType, overrideContext);

			return overrideParameterType.equals(parameterType);
		}
		else if(overrideParameterType == null && parameterType != null && !"*".equals(parameterType))
		{
			return false;
		}

		return true;
	}

	@Override
	public void visitJSReturnStatement(final JSReturnStatement node)
	{
		final PsiElement element = PsiTreeUtil.getParentOfType(node, JSFunction.class, XmlTagChild.class, XmlAttributeValue.class, JSFile.class);
		if((element instanceof JSFile && !(element.getContext() instanceof PsiLanguageInjectionHost)) || (element instanceof XmlTagChild && !(element
				.getParent() instanceof XmlAttributeValue)))
		{
			myHolder.createErrorAnnotation(node, JavaScriptBundle.message("javascript.validation.message.return.outside.function.definition"));
		}

		if(element instanceof JSFunction)
		{
			final @NonNls String typeString = ((JSFunction) element).getReturnTypeString();
			if(typeString != null && !"void".equals(typeString) && node.getExpression() == null)
			{
				myHolder.createErrorAnnotation(node, JavaScriptBundle.message("javascript.validation.message.return.value.of.type.is.required", typeString));
			}
		}
	}

	@Override
	public void visitJSLabeledStatement(final JSLabeledStatement node)
	{
		final String label = node.getLabel();
		if(label != null)
		{
			PsiElement run = node.getParent();
			while(run != null)
			{
				if(run instanceof JSLabeledStatement)
				{
					if(label.equals(((JSLabeledStatement) run).getLabel()))
					{
						myHolder.createErrorAnnotation(node.getLabelIdentifier(), JavaScriptBundle.message("javascript.validation.message.duplicate.label"));
						break;
					}
				}

				if(run instanceof JSFunction)
				{
					break;
				}
				run = run.getParent();
			}
		}
	}

	private void checkNamedObjectIsInCorrespondingFile(final JSNamedElement aClass)
	{
		final PsiFile containingFile = aClass.getContainingFile();

		if(containingFile.getContext() != null)
		{
			return;
		}
		final VirtualFile file = containingFile.getVirtualFile();

		if(file != null && !file.getNameWithoutExtension().equals(aClass.getName()))
		{
			final ASTNode node = aClass.findNameIdentifier();

			if(node != null)
			{
				final String name = aClass.getName();
				String nameWithExtension = name + "." + file.getExtension();
				final String message = JavaScriptBundle.message(aClass instanceof JSClass ? "javascript.validation.message.class.should.be.in.file" : aClass instanceof
						JSNamespaceDeclaration ? "javascript.validation.message.namespace.should.be.in.file" : aClass instanceof JSVariable ? "javascript.validation" +
						".message.variable.should.be.in.file" : "javascript.validation.message.function.should.be.in.file", name, nameWithExtension);
				final Annotation annotation = myHolder.createErrorAnnotation(node, message);

				annotation.registerFix(new RenameFileFix(nameWithExtension));
		/*annotation.registerFix(new RenamePublicClassFix(aClass) {
          final String text;
          final String familyName;

          {
            String term = getTerm(message);
            text = super.getText().replace("class", StringUtil.decapitalize(term));
            familyName = super.getFamilyName().replace("Class", term);
          }
          @NotNull
          @Override
          public String getText() {
            return text;
          }

          @NotNull
          @Override
          public String getFamilyName() {
            return familyName;
          }
        }); */
			}
		}

		checkFileUnderSourceRoot(aClass, new SimpleErrorReportingClient());
	}

	private String getTerm(String message)
	{
		String term = message.substring(0, message.indexOf(' '));
		return term;
	}

	public static void checkFileUnderSourceRoot(final JSNamedElement aClass, ErrorReportingClient client)
	{
		PsiElement nameIdentifier = aClass.getNameIdentifier();
		if(nameIdentifier == null)
		{
			nameIdentifier = aClass.getFirstChild();
		}

		final PsiFile containingFile = aClass.getContainingFile();
		final VirtualFile file = containingFile.getVirtualFile();
		if(file == null)
		{
			return;
		}
		final VirtualFile rootForFile = ProjectRootManager.getInstance(containingFile.getProject()).getFileIndex().getSourceRootForFile(file);

		if(rootForFile == null)
		{
			client.reportError(nameIdentifier.getNode(), JavaScriptBundle.message("javascript.validation.message.file.should.be.under.source.root"),
					ErrorReportingClient.ProblemKind.WARNING, null);
		}
	}

	private void checkPackageStatement(final JSPackageStatement packageStatement)
	{
		final String s = packageStatement.getQualifiedName();

		final PsiFile containingFile = packageStatement.getContainingFile();
		final String expected = JSResolveUtil.getExpectedPackageNameFromFile(containingFile.getVirtualFile(), containingFile.getProject(), true);

		if(expected != null && ((s == null && expected.length() != 0) || (s != null && !expected.equals(s))))
		{
			final ASTNode nameIdentifier = packageStatement.findNameIdentifier();
			final Annotation annotation = myHolder.createErrorAnnotation(nameIdentifier != null ? nameIdentifier : packageStatement.getFirstChild().getNode()
					, JavaScriptBundle.message("javascript.validation.message.incorrect.package.name", s, expected));
			annotation.registerFix(new IntentionAction()
			{
				@Override
				@NotNull
				public String getText()
				{
					return JavaScriptBundle.message("javascript.fix.package.name", expected);
				}

				@Override
				@NotNull
				public String getFamilyName()
				{
					return getText();
				}

				@Override
				public boolean isAvailable(@NotNull final Project project, final Editor editor, final PsiFile file)
				{
					return packageStatement.isValid();
				}

				@Override
				public void invoke(@NotNull final Project project, final Editor editor, final PsiFile file) throws IncorrectOperationException
				{
					JSPackageStatementImpl.doChangeName(project, packageStatement, expected);
				}

				@Override
				public boolean startInWriteAction()
				{
					return true;
				}
			});
		}

		final Set<JSNamedElement> elements = new THashSet<JSNamedElement>();

		for(JSSourceElement statement : packageStatement.getStatements())
		{
			if(statement instanceof JSNamedElement && !(statement instanceof JSImportStatement))
			{
				elements.add((JSNamedElement) statement);
			}
		}

		if(elements.size() > 1)
		{
			for(JSNamedElement el : elements)
			{
				final ASTNode nameIdentifier = el.findNameIdentifier();
				myHolder.createErrorAnnotation(nameIdentifier != null ? nameIdentifier : el.getFirstChild().getNode(),
						JavaScriptBundle.message("javascript.validation.message.more.than.one.externally.visible.symbol")).registerFix(new RemoveASTNodeFix(el.getNode(),
						"javascript.fix.remove.externally.visible.symbol"));
			}
		}

		checkFileUnderSourceRoot(packageStatement, new SimpleErrorReportingClient());
	}

	public static class RemoveASTNodeFix implements IntentionAction, LocalQuickFix
	{
		private final ASTNode[] myAstNodes;
		private final String myPropKey;

		public RemoveASTNodeFix(final ASTNode astNode, @PropertyKey(resourceBundle = JavaScriptBundle.BUNDLE) String propKey)
		{
			this(propKey, astNode);
		}

		public RemoveASTNodeFix(@PropertyKey(resourceBundle = JavaScriptBundle.BUNDLE) String propKey, final ASTNode... astNodes)
		{
			myPropKey = propKey;
			myAstNodes = astNodes;
		}

		@Override
		@NotNull
		public String getText()
		{
			return JavaScriptBundle.message(myPropKey);
		}

		@Override
		@NotNull
		public String getName()
		{
			return getText();
		}

		@Override
		@NotNull
		public String getFamilyName()
		{
			return getText();
		}

		@Override
		public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor)
		{
			invoke(project, null, descriptor.getPsiElement().getContainingFile());
		}

		@Override
		public boolean isAvailable(@NotNull final Project project, final Editor editor, final PsiFile file)
		{
			for(ASTNode astNode : myAstNodes)
			{
				if(!astNode.getPsi().isValid())
				{
					return false;
				}
			}

			return true;
		}

		@Override
		public void invoke(@NotNull final Project project, final Editor editor, final PsiFile file) throws IncorrectOperationException
		{
			if(!CodeInsightUtilBase.getInstance().prepareFileForWrite(file))
			{
				return;
			}
			for(ASTNode astNode : myAstNodes)
			{
				if(astNode.getPsi().isValid())
				{
					astNode.getPsi().delete();
				}
			}
		}

		@Override
		public boolean startInWriteAction()
		{
			return true;
		}
	}

	private static class AddOverrideIntentionAction implements IntentionAction
	{
		private final JSFunction myNode;

		public AddOverrideIntentionAction(final JSFunction node)
		{
			myNode = node;
		}

		@Override
		@NotNull
		public String getText()
		{
			return JavaScriptBundle.message("javascript.fix.add.override.modifier");
		}

		@Override
		@NotNull
		public String getFamilyName()
		{
			return getText();
		}

		@Override
		public boolean isAvailable(@NotNull final Project project, final Editor editor, final PsiFile file)
		{
			return myNode.isValid();
		}

		@Override
		public void invoke(@NotNull final Project project, final Editor editor, final PsiFile file) throws IncorrectOperationException
		{
			if(!CodeInsightUtilBase.getInstance().prepareFileForWrite(file))
			{
				return;
			}
			final ASTNode fromText = JSChangeUtil.createJSTreeFromText(project, "override class A {}");
			final JSAttributeList jsAttributeList = myNode.getAttributeList();
			final JSAttributeList createdAttrList = ((JSClass) fromText.getPsi()).getAttributeList();

			if(jsAttributeList != null)
			{
				jsAttributeList.add(createdAttrList.getFirstChild());
			}
			else
			{
				myNode.addBefore(createdAttrList, myNode.getFirstChild());
			}
		}

		@Override
		public boolean startInWriteAction()
		{
			return true;
		}
	}

	private class SimpleErrorReportingClient implements ErrorReportingClient
	{
		@Override
		public void reportError(final ASTNode nameIdentifier, final String s, ProblemKind kind, final IntentionAction implementMethodsFix)
		{
			final Annotation annotation = kind == ProblemKind.ERROR ? myHolder.createErrorAnnotation(nameIdentifier,
					s) : myHolder.createWarningAnnotation(nameIdentifier, s);
			if(implementMethodsFix != null)
			{
				annotation.registerFix(implementMethodsFix);
			}
		}
	}

	private static class AddSuperInvokationFix implements IntentionAction
	{
		private final JSReferenceExpression node;
		private final JSFunction superConstructor;

		public AddSuperInvokationFix(JSReferenceExpression node, JSFunction superConstructor)
		{
			this.node = node;
			this.superConstructor = superConstructor;
		}

		@Override
		@NotNull
		public String getText()
		{
			return JavaScriptBundle.message("javascript.fix.create.invoke.super");
		}

		@Override
		@NotNull
		public String getFamilyName()
		{
			return getText();
		}

		@Override
		public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file)
		{
			return superConstructor.isValid() && node.isValid();
		}

		@Override
		public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException
		{
			if(!CodeInsightUtilBase.getInstance().prepareFileForWrite(file))
			{
				return;
			}
			Template t = TemplateManager.getInstance(project).createTemplate("", "");
			t.setToReformat(true);

			t.addTextSegment("super(");
			boolean first = true;
			for(JSParameter p : superConstructor.getParameterList().getParameters())
			{
				if(p.isRest())
				{
					break;
				}
				if(!first)
				{
					t.addTextSegment(", ");
				}
				first = false;
				MacroCallNode node = new MacroCallNode(new CompleteMacro());
				t.addVariable(p.getName(), node, node, true);
			}
			t.addTextSegment(")");
			String s = JSChangeUtil.getSemicolon(project);
			if(s.length() > 0)
			{
				t.addTextSegment(s);
			}

			JSFunction fun = PsiTreeUtil.getParentOfType(node, JSFunction.class);
			JSSourceElement[] body = fun != null ? fun.getBody() : JSSourceElement.EMPTY_ARRAY;

			if(body.length > 0 && body[0] instanceof JSBlockStatement)
			{
				PsiElement firstChild = body[0].getFirstChild();
				editor.getCaretModel().moveToOffset(firstChild.getTextRange().getEndOffset());
				TemplateManager.getInstance(project).startTemplate(editor, t);
			}
		}

		@Override
		public boolean startInWriteAction()
		{
			return false;
		}
	}

	private static class AddConstructorAndSuperInvokationFix implements IntentionAction
	{
		private final JSReferenceExpression node;
		private final JSFunction superConstructor;

		AddConstructorAndSuperInvokationFix(JSReferenceExpression _node, JSFunction _superCall)
		{
			node = _node;
			superConstructor = _superCall;
		}

		@Override
		@NotNull
		public String getText()
		{
			return JavaScriptBundle.message("javascript.fix.create.constructor.invoke.super");
		}

		@Override
		@NotNull
		public String getFamilyName()
		{
			return getText();
		}

		@Override
		public boolean isAvailable(@NotNull Project project, Editor editor, PsiFile file)
		{
			return node.isValid() && superConstructor.isValid();
		}

		@Override
		public void invoke(@NotNull Project project, Editor editor, PsiFile file) throws IncorrectOperationException
		{
			if(!CodeInsightUtilBase.getInstance().prepareFileForWrite(file))
			{
				return;
			}
			final JSClass jsClass = PsiTreeUtil.getParentOfType(node, JSClass.class);
			if(jsClass == null)
			{
				return;
			}
			final JSAttributeList attributeList = jsClass.getAttributeList();
			String fun = "";

			if(attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PUBLIC)
			{
				fun += "public ";
			}

			fun += "function ";

			final JSParameterList parameterList = superConstructor.getParameterList();
			fun += jsClass.getName() + parameterList.getText() + "{\n";
			fun += "super(";
			int i = 0;

			for(JSParameter p : parameterList.getParameters())
			{
				if(i != 0)
				{
					fun += ",";
				}
				++i;
				fun += p.getName();
			}
			fun += ")" + JSChangeUtil.getSemicolon(project);
			fun += "\n}";

			jsClass.add(JSChangeUtil.createJSTreeFromText(project, fun).getPsi());
		}

		@Override
		public boolean startInWriteAction()
		{
			return true;
		}
	}
}
