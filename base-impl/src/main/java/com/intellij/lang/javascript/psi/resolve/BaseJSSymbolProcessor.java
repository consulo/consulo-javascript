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

package com.intellij.lang.javascript.psi.resolve;

import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.ASTNode;
import com.intellij.lang.injection.InjectedLanguageManager;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.index.JSSymbolUtil;
import com.intellij.lang.javascript.index.JSTypeEvaluateManager;
import com.intellij.lang.javascript.psi.*;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.*;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlFile;
import com.intellij.psi.xml.XmlTag;
import com.intellij.psi.xml.XmlToken;
import com.intellij.util.ArrayUtil;
import consulo.javascript.lang.JavaScriptFeature;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.lang.JavaScriptVersionUtil;
import consulo.util.dataholder.Key;
import gnu.trove.THashSet;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

/**
 * @author Maxim.Mossienko
 */
abstract public class BaseJSSymbolProcessor implements PsiScopeProcessor
{
	protected final PsiFile myTargetFile;
	protected PsiFile myCurrentFile;
	protected final boolean mySkipDclsInTargetFile;
	protected final PsiElement myContext;
	protected final String myWindowIndex;
	protected final String myFunctionIndex;

	protected String[] myContextNameIds;
	@Deprecated
	protected boolean ecmal4;
	protected Set<JavaScriptFeature> myFeatures;
	protected boolean myDefinitelyGlobalReference;
	protected boolean myDefinitelyNonglobalReference;
	protected boolean myAddOnlyCompleteMatches;
	protected boolean myAllowPartialResults = true;

	@NonNls
	private static final String ANY_TYPE = "*";
	@NonNls
	public static final String ARRAY_TYPE_NAME = "Array";
	@NonNls
	public static final String STRING_TYPE_NAME = "String";
	@NonNls
	public static final String REG_EXP_TYPE_NAME = "RegExp";
	@NonNls
	public static final String XML_TYPE_NAME = "XML";
	@NonNls
	public static final String XML_LIST_TYPE_NAME = "XMLList";
	@NonNls
	public static final String NUMBER_TYPE_NAME = "Number";
	@NonNls
	public static final String FUNCTION_TYPE_NAME = "Function";
	@NonNls
	public static final String HTML_ELEMENT_TYPE_NAME = "HTMLElement";
	@NonNls
	private static final String INT_TYPE = "int";
	@NonNls
	private static final String BOOLEAN_TYPE_NAME = "Boolean";
	protected String myIteratedTypeName;

	protected BaseJSSymbolProcessor(final PsiFile targetFile, final boolean skipDclsInTargetFile, final PsiElement context, String[] contextIds)
	{
		myTargetFile = targetFile;
		mySkipDclsInTargetFile = skipDclsInTargetFile;
		myContext = context;

		myWindowIndex = "window";
		myFunctionIndex = "Function";

		myFeatures = JavaScriptVersionUtil.getFeatures(targetFile);
		ecmal4 = myFeatures.contains(JavaScriptFeature.CLASS);

		if(contextIds == null && context instanceof JSReferenceExpression)
		{
			contextIds = calculateContextIds((JSReferenceExpression) context);
		}

		if(contextIds == null)
		{ // TODO: workaround for NPE
			contextIds = new String[0];
		}

		myContextNameIds = contextIds;

		final boolean notWithinWithStatement = PsiTreeUtil.getParentOfType(myContext, JSWithStatement.class) == null;

		if(((contextIds == null || contextIds.length == 0) &&
				myContext instanceof JSReferenceExpression &&
				((JSReferenceExpression) myContext).getQualifier() == null && notWithinWithStatement &&
				JSResolveUtil.getRealRefExprQualifier((JSReferenceExpression) myContext) == null) || (contextIds != null && contextIds.length == 1 &&
				contextIds[0].equals(myWindowIndex) && notWithinWithStatement))
		{
			myDefinitelyGlobalReference = true;
		}

		if(contextIds != null && (contextIds.length > 1 || (contextIds.length == 1 &&
				contextIds[0].length() > 0 &&
				!contextIds[0].equals(myWindowIndex))))
		{
			myDefinitelyNonglobalReference = true;
		}
	}

	protected abstract String[] calculateContextIds(final JSReferenceExpression jsReferenceExpression);

	public void setAddOnlyCompleteMatches(final boolean addOnlyCompleteMatches)
	{
		myAddOnlyCompleteMatches = addOnlyCompleteMatches;
		myAllowPartialResults = false;
	}


	protected boolean isFromRelevantFileOrDirectory()
	{
		final PsiFile psiFile = myCurrentFile;

		return myTargetFile == psiFile || false;
		//(myTargetFile != null &&
		// psiFile != null &&
		// myTargetFile.getContainingDirectory() == psiFile.getContainingDirectory()
		//);
	}

	private static ThreadLocal<EvaluateContext> contextHolder = new ThreadLocal<EvaluateContext>();

	public static void doEvalForExpr(JSExpression rawqualifier, final PsiFile myTargetFile, TypeProcessor typeProcessor)
	{
		EvaluateContext context = contextHolder.get();
		boolean contextHolderInitialized = false;

		if(context == null)
		{
			context = new EvaluateContext(myTargetFile);
			contextHolder.set(context);
			contextHolderInitialized = true;
		}
		else if(context.isAlreadyProcessingItem(rawqualifier))
		{
			return;
		}
		else
		{
			context.addProcessingItem(rawqualifier);
		}

		try
		{
			doEvalForExpr(rawqualifier, typeProcessor, contextHolderInitialized ? context : new EvaluateContext(myTargetFile));
		}
		finally
		{
			if(!contextHolderInitialized)
			{
				context.removeProcessingItem(rawqualifier);
			}
			if(contextHolderInitialized)
			{
				contextHolder.set(null);
			}
		}
	}

	public static JSExpression getOriginalQualifier(JSExpression rawqualifier)
	{
		if(rawqualifier instanceof JSReferenceExpression)
		{
			JSReferenceExpression qualifier = ((JSReferenceExpression) rawqualifier);
			final TextRange textRange = qualifier.getTextRange();
			PsiFile targetFile = rawqualifier.getContainingFile();
			final PsiElement context = targetFile.getContext();
			if(context != null)
			{
				targetFile = context.getContainingFile();
			}

			PsiFile originalFile = targetFile.getOriginalFile();

			if(!targetFile.isPhysical() && originalFile != null)
			{
				PsiElement startElement;
				if(context != null)
				{
					final PsiElement at = PsiTreeUtil.getNonStrictParentOfType(originalFile.findElementAt(context.getTextOffset()), PsiLanguageInjectionHost.class);
					if(at instanceof PsiLanguageInjectionHost)
					{
						List<Pair<PsiElement, TextRange>> injectedPsiFiles = InjectedLanguageManager.getInstance(at.getProject()).getInjectedPsiFiles(at);
						if(injectedPsiFiles != null)
						{
							for(Pair<PsiElement, TextRange> pair : injectedPsiFiles)
							{
								if(pair.getFirst() instanceof JSFile)
								{
									originalFile = (PsiFile) pair.getFirst();
									break;
								}
							}
						}
					}
				}
				startElement = originalFile.findElementAt(qualifier.getTextOffset());

				do
				{
					qualifier = PsiTreeUtil.getParentOfType(startElement, JSReferenceExpression.class);
					if(qualifier == null)
					{
						break;
					}
					startElement = qualifier;
				}
				while(!textRange.equals(qualifier.getTextRange()));
			}

			rawqualifier = qualifier;
		}
		else if(rawqualifier instanceof JSIndexedPropertyAccessExpression)
		{
			final JSExpression qualifier = ((JSIndexedPropertyAccessExpression) rawqualifier).getQualifier();
			if(qualifier != null)
			{
				final JSExpression jsExpression = getOriginalQualifier(qualifier);
				if(jsExpression != null && jsExpression.getParent() instanceof JSIndexedPropertyAccessExpression)
				{
					return (JSExpression) jsExpression.getParent();
				}
			}
		}
		else if(rawqualifier instanceof JSCallExpression)
		{
			final JSExpression qualifier = ((JSCallExpression) rawqualifier).getMethodExpression();
			if(qualifier != null)
			{
				final JSExpression jsExpression = getOriginalQualifier(qualifier);
				if(jsExpression != null && jsExpression.getParent() instanceof JSCallExpression)
				{
					return (JSExpression) jsExpression.getParent();
				}
			}
		}
		return rawqualifier;
	}

	private static void doEvalForExpr(JSExpression rawqualifier, TypeProcessor typeProcessor, EvaluateContext context)
	{
		if(rawqualifier instanceof JSDefinitionExpression)
		{
			rawqualifier = ((JSDefinitionExpression) rawqualifier).getExpression();
		}

		if(rawqualifier instanceof JSNewExpression)
		{
			JSExpression methodExpr = ((JSNewExpression) rawqualifier).getMethodExpression();
			if(methodExpr != null)
			{
				String text = methodExpr.getText();
				if(methodExpr instanceof JSReferenceExpression && typeProcessor.ecma())
				{
					final SimpleTypeProcessor processor = new SimpleTypeProcessor(typeProcessor.getFeatures());
					doEvalForExpr(methodExpr, context.targetFile, processor);

					if(processor.type != null && !"*".equals(processor.type))
					{
						text = "Class".equals(processor.type) ? "*" : processor.type;
					}
				}
				addType(text, typeProcessor, context, methodExpr);
			}
			return;
		}

		if(rawqualifier instanceof JSCallExpression)
		{
			rawqualifier = ((JSCallExpression) rawqualifier).getMethodExpression();
		}

		if(rawqualifier instanceof JSReferenceExpression)
		{
			JSReferenceExpression qualifier = ((JSReferenceExpression) rawqualifier);

			boolean wasPrototype = false;

			if(qualifier != null && "prototype".equals(qualifier.getReferencedName()))
			{
				final JSExpression expression = qualifier.getQualifier();
				if(expression instanceof JSReferenceExpression)
				{
					qualifier = (JSReferenceExpression) expression;
					wasPrototype = true;
				}
			}

			boolean topLevelQualifier = qualifier.getQualifier() == null;

			if(topLevelQualifier)
			{
				final JSReferenceExpression expression = typeProcessor.ecma() ? qualifier : JSSymbolUtil.findReferenceExpressionUsedForClassExtending(qualifier);
				if(!typeProcessor.ecma() && expression != qualifier && expression.getQualifier() == null)
				{
					addType(expression.getText(), typeProcessor, context, qualifier);
					return;
				}
			}

			final ResolveResult[] resolveResults = qualifier != null ? qualifier.multiResolve(false) : ResolveResult.EMPTY_ARRAY;

			if(resolveResults.length > 0)
			{
				for(ResolveResult r : resolveResults)
				{
					PsiElement psiElement = r.getElement();
					context.setSource(psiElement);
					if(psiElement == qualifier.getParent())
					{
						continue;
					}

					//if (psiElement == qualifier && ((JSReferenceExpressionImpl)qualifier).isAttributeReference()) {
					//   addType(STRING_TYPE_NAME, typeProcessor, context, null); continue;
					//}
					String type = psiElement instanceof JSNamedElement ? null :  null;

					if(type == null)
					{
						if(psiElement instanceof JSVariable)
						{
							final JSVariable jsVariable = (JSVariable) psiElement;
							String parameterType = jsVariable.getTypeString();

							if(isSomeFunctionCall(rawqualifier, parameterType))
							{
								parameterType = ANY_TYPE;

								if(jsVariable.hasInitializer())
								{
									JSExpression jsExpression = jsVariable.getInitializer();
									if(jsExpression instanceof JSFunctionExpression)
									{
										String typeString = ((JSFunctionExpression) jsExpression).getFunction().getReturnTypeString();
										if(isValidType(typeString))
										{
											parameterType = typeString;
										}
									}
								}
							}

							if(isValidType(parameterType))
							{
								addType(JSImportHandlingUtil.resolveTypeName(parameterType, jsVariable), typeProcessor, context, null);
							}

							if(!typeProcessor.ecma() || parameterType == null)
							{
								final JSExpression expression = jsVariable.getInitializer();
								if(expression != null)
								{
									doEvalForExpr(expression, context.targetFile, typeProcessor);
								}
								else
								{
									boolean hasSomeInfoAboutVar = false;

									if(topLevelQualifier)
									{
										final ResolveProcessor processor = new ResolveProcessor(qualifier.getText(), true);
										hasSomeInfoAboutVar = findDef(typeProcessor, context, qualifier, processor);
									}

									if(!hasSomeInfoAboutVar && parameterType == null)
									{
										PsiElement grandParent;

										if((grandParent = jsVariable.getParent().getParent()) instanceof JSForInStatement && jsVariable.getParent() == ((JSForInStatement)
												grandParent).getDeclarationStatement())
										{
											parameterType = evalComponentTypeFromArrayExpression(rawqualifier, typeProcessor, context,
													((JSForInStatement) grandParent).getCollectionExpression());
										}
									}
									if(!hasSomeInfoAboutVar && parameterType == null)
									{
										typeProcessor.setUnknownElement(jsVariable);
									}
								}
							}
						}
						else
						{
							boolean hasSomeType = false;

							if(psiElement instanceof JSProperty)
							{
								psiElement = ((JSProperty) psiElement).getValue();
							}

							if(psiElement instanceof JSClass)
							{
								addType(((JSClass) psiElement).getQualifiedName(), typeProcessor, context, psiElement);
								hasSomeType = true;
							}
							else if(psiElement instanceof JSObjectLiteralExpression && typeProcessor instanceof ResolveProcessor)
							{
								addTypeFromObjectLiteralExpression((JSExpression) psiElement, typeProcessor);
							}
							else if(psiElement instanceof JSDefinitionExpression)
							{
								PsiElement parent;
								String parameterType = null;

								if((parent = psiElement.getParent()) instanceof JSForInStatement && psiElement == ((JSForInStatement) parent).getVariableExpression())
								{
									parameterType = evalComponentTypeFromArrayExpression(rawqualifier, typeProcessor, context,
											((JSForInStatement) parent).getCollectionExpression());
								}
								if(parameterType == null)
								{
									addTypeFromDefExpr(typeProcessor, context, psiElement);
								}
							}
							else if(psiElement instanceof XmlToken)
							{
								hasSomeType = true;

								final TagContextBuilder builder = new TagContextBuilder(psiElement, HTML_ELEMENT_TYPE_NAME);
								final PsiElement element = builder.element;
								final String typeName = builder.typeName;

								if(HTML_ELEMENT_TYPE_NAME.equals(typeName))
								{
									hasSomeType = false;
								}

								addType(typeName, typeProcessor, context, element);
							}
							else if(psiElement instanceof JSNamedElement)
							{
								if(psiElement instanceof JSFunction)
								{
									final JSFunction function = (JSFunction) psiElement;
									final boolean inCall = rawqualifier.getParent() instanceof JSCallExpression;

									if(!inCall && (!function.isGetProperty() && !function.isSetProperty()))
									{
										addType(FUNCTION_TYPE_NAME, typeProcessor, context, psiElement);
										hasSomeType = true;
										if(wasPrototype)
										{
											final String name = ((JSNamedElement) psiElement).getName();
											if(name != null)
											{
												addType(name, typeProcessor, context, null);
											}
										}
									}
									else
									{
										if(function.isConstructor())
										{
											PsiElement parentClass = JSResolveUtil.findParent(function);
											if(parentClass instanceof JSClass)
											{
												final JSClass clazz = (JSClass) parentClass;
												addType(clazz.getQualifiedName(), typeProcessor, context, clazz);
												hasSomeType = true;
											}
										}
										else
										{
											String s = null;
											if(function.isSetProperty())
											{
												final JSParameterList parameterList = function.getParameterList();
												final JSParameter[] parameters = parameterList != null ? parameterList.getParameters() : JSParameter.EMPTY_ARRAY;
												s = parameters.length == 1 ? parameters[0].getTypeString() : null;
											}

											if(s == null)
											{
												s = function.getReturnTypeString();
												if(isSomeFunctionCall(rawqualifier, s))
												{
													s = ANY_TYPE;
												}
											}
											if(isValidType(s))
											{
												addType(JSImportHandlingUtil.resolveTypeName(s, function), typeProcessor, context, null);
												hasSomeType = true;
											}
											else
											{
												addQNameFromElementAsType(typeProcessor, context, psiElement, false);
											}
										}
									}
								}
								else
								{
									boolean hasTypeKnowledge = false;
									boolean passSource = false;

									if(!hasTypeKnowledge || wasPrototype)
									{
										addQNameFromElementAsType(typeProcessor, context, psiElement, passSource);
									}
								}
							}
							else if(psiElement instanceof JSLiteralExpression)
							{
								getTypeFromConstant((JSExpression) psiElement, typeProcessor, context);
								hasSomeType = true;
							}
							else if(psiElement instanceof JSExpressionStatement)
							{
								final String s = JSDocumentationUtils.findType(psiElement);
								if(isValidType(s))
								{
									addType(JSImportHandlingUtil.resolveTypeName(s, psiElement), typeProcessor, context, null);
									hasSomeType = true;
								}
							}
							if(!hasSomeType && psiElement != null)
							{
								typeProcessor.setUnknownElement(psiElement);
							}
						}
						//myTargetFiles.add(psiElement.getContainingFile());
					}
					else if(!context.visitedTypes.contains(type))
					{
						//if (context.typeEvaluateManager.isArrayType(type))) type = ARRAY_TYPE_NAME;
						addType(type, typeProcessor, context, null);
					}
				}
			}
			else
			{
				typeProcessor.setUnknownElement(qualifier);
			}

			JSReferenceExpression localQualifier = qualifier;

			while(true)
			{
				JSExpression expression = localQualifier.getQualifier();

				if(expression instanceof JSCallExpression)
				{
					JSExpression methodExpression = ((JSCallExpression) expression).getMethodExpression();

					if(methodExpression instanceof JSReferenceExpression)
					{
						localQualifier = (JSReferenceExpression) methodExpression;
						continue;
					}
				}

				break;
			}

			if("$".equals(localQualifier.getText()))
			{
				addType("jQuery", typeProcessor, context, localQualifier);
			}
			if("$".equals(qualifier.getText()))
			{
				addType(HTML_ELEMENT_TYPE_NAME, typeProcessor, context, null);
			}
			else if("getComponentById".equals(qualifier.getReferencedName()))
			{
				tryAddBindowsType(qualifier, typeProcessor, context);
			}
		}
		else if(rawqualifier instanceof JSBinaryExpression)
		{
			final JSBinaryExpression binaryExpression = (JSBinaryExpression) rawqualifier;
			final IElementType sign = binaryExpression.getOperationSign();
			final JSExpression rOperand = binaryExpression.getROperand();
			final JSExpression lOperand = binaryExpression.getLOperand();

			if(rOperand != null)
			{
				if(sign == JSTokenTypes.AS_KEYWORD)
				{
					addType(JSImportHandlingUtil.resolveTypeName(rOperand.getText(), rOperand), typeProcessor, context, null);
				}
				else if(JSTokenTypes.RELATIONAL_OPERATIONS.contains(sign) || JSTokenTypes.EQUALITY_OPERATIONS.contains(sign) ||
						sign == JSTokenTypes.IS_KEYWORD)
				{
					addType(BOOLEAN_TYPE_NAME, typeProcessor, context, null);
				}
				else if(JSTokenTypes.ADDITIVE_OPERATIONS.contains(sign) ||
						JSTokenTypes.MULTIPLICATIVE_OPERATIONS.contains(sign) ||
						sign == JSTokenTypes.ANDAND ||
						sign == JSTokenTypes.OROR)
				{

					final SimpleTypeProcessor lprocessor = new SimpleTypeProcessor(typeProcessor.getFeatures());
					final SimpleTypeProcessor rprocessor = new SimpleTypeProcessor(typeProcessor.getFeatures());
					doEvalForExpr(lOperand, lprocessor, context);
					doEvalForExpr(rOperand, rprocessor, context);

					String evaluatedType = lprocessor.type != null && lprocessor.type.equals(rprocessor.type) ? lprocessor.type : null;
					if(evaluatedType != null)
					{
						addType(evaluatedType, typeProcessor, context, rawqualifier);
					}
					else
					{
						typeProcessor.setUnknownElement(rawqualifier);
					}
				}
				else if(sign == JSTokenTypes.EQ)
				{
					final SimpleTypeProcessor rprocessor = new SimpleTypeProcessor(typeProcessor.getFeatures());
					doEvalForExpr(rOperand, rprocessor, context);

					String evaluatedType = rprocessor.type;
					if(evaluatedType != null)
					{
						addType(evaluatedType, typeProcessor, context, rawqualifier);
					}
					else
					{
						typeProcessor.setUnknownElement(rawqualifier);
					}
				}
			}
		}
		else if(rawqualifier instanceof JSLiteralExpression)
		{
			getTypeFromConstant(rawqualifier, typeProcessor, context);
		}
		else if(rawqualifier instanceof JSArrayLiteralExpression)
		{
			addType(ARRAY_TYPE_NAME, typeProcessor, context, rawqualifier);
			if(typeProcessor instanceof ResolveProcessor)
			{
				((ResolveProcessor) typeProcessor).execute(rawqualifier, ResolveState.initial());
			}
		}
		else if(rawqualifier instanceof JSIndexedPropertyAccessExpression)
		{
			final JSIndexedPropertyAccessExpression propertyAccessExpression = (JSIndexedPropertyAccessExpression) rawqualifier;
			final SimpleTypeProcessor lprocessor = new SimpleTypeProcessor(typeProcessor.getFeatures());

			doEvalForExpr(propertyAccessExpression.getQualifier(), lprocessor, context);

			if(lprocessor.result instanceof JSArrayLiteralExpression && typeProcessor instanceof ResolveProcessor)
			{
				for(JSExpression expr : ((JSArrayLiteralExpression) lprocessor.result).getExpressions())
				{
					if(expr instanceof JSObjectLiteralExpression)
					{
						addTypeFromObjectLiteralExpression(expr, typeProcessor);
					}
				}
			}

			addComponentTypeFromProcessor(rawqualifier, typeProcessor, context, lprocessor);
		}
		else if(rawqualifier instanceof JSObjectLiteralExpression && typeProcessor instanceof ResolveProcessor)
		{
			addTypeFromObjectLiteralExpression(rawqualifier, typeProcessor);
		}
		else if(rawqualifier instanceof JSParenthesizedExpression)
		{
			doEvalForExpr(((JSParenthesizedExpression) rawqualifier).getInnerExpression(), typeProcessor, context);
		}
		else if(rawqualifier instanceof JSThisExpression)
		{
			final JSClass jsClass = JSResolveUtil.getClassOfContext(rawqualifier);
			if(jsClass != null)
			{
				addType(jsClass.getQualifiedName(), typeProcessor, context, jsClass);
			}
			else
			{
				typeProcessor.setUnknownElement(rawqualifier);
			}
		}
		else if(rawqualifier instanceof JSSuperExpression)
		{
			final JSClass jsClass = JSResolveUtil.getClassOfContext(rawqualifier);
			if(jsClass != null)
			{
				final JSClass[] classes = jsClass.getSuperClasses();
				if(classes.length > 0)
				{
					addType(classes[0].getQualifiedName(), typeProcessor, context, classes[0]);
				}
			}
		}
		else if(rawqualifier != null)
		{
			typeProcessor.setUnknownElement(rawqualifier);
		}
	}

	private static boolean isSomeFunctionCall(JSExpression rawqualifier, String parameterType)
	{
		return FUNCTION_TYPE_NAME.equals(parameterType) && rawqualifier.getParent() instanceof JSCallExpression;
	}

	private static void addQNameFromElementAsType(TypeProcessor typeProcessor, EvaluateContext context, PsiElement psiElement, boolean passSource)
	{
		String name = ((JSNamedElement) psiElement).getName();
		if(name == null)
		{
			return;
		}
		if(psiElement instanceof JSQualifiedNamedElement)
		{
			String qName = ((JSQualifiedNamedElement) psiElement).getQualifiedName();
			if(qName != null)
			{
				name = qName;
			}
		}
		addType(name, typeProcessor, context, passSource ? psiElement : null);
	}

	private static String evalComponentTypeFromArrayExpression(JSExpression rawqualifier, TypeProcessor typeProcessor, EvaluateContext context,
			JSExpression collectionExpression)
	{
		if(collectionExpression != null)
		{
			if(context.isAlreadyProcessingItem(rawqualifier))
			{
				return null;
			}
			context.addProcessingItem(rawqualifier);
			final SimpleTypeProcessor lprocessor = new SimpleTypeProcessor(typeProcessor.getFeatures());
			doEvalForExpr(collectionExpression, lprocessor, context);
			return addComponentTypeFromProcessor(rawqualifier, typeProcessor, context, lprocessor);
		}

		return null;
	}

	private static String addComponentTypeFromProcessor(JSExpression rawqualifier, TypeProcessor typeProcessor, EvaluateContext context,
			SimpleTypeProcessor lprocessor)
	{
		if(lprocessor.type != null)
		{
			String type = lprocessor.type;
			if(JSTypeEvaluateManager.isArrayType(type))
			{
				type = JSTypeEvaluateManager.getComponentType(type);
			}
			else
			{
				type = "*";
			}
			addType(JSImportHandlingUtil.resolveTypeName(type, rawqualifier), typeProcessor, context, rawqualifier);
			return type;
		}
		return null;
	}

	private static boolean isValidType(String parameterType)
	{
		return parameterType != null && parameterType.length() > 0;
	}

	private static boolean findDef(final TypeProcessor typeProcessor, final EvaluateContext context, final JSReferenceExpression qualifier,
			final ResolveProcessor processor)
	{
		final PsiElement parent = qualifier.getParent();
		JSResolveUtil.treeWalkUp(processor, qualifier, parent, qualifier);

		PsiElement jsElement = processor.getResult();
		if(jsElement instanceof JSDefinitionExpression)
		{
			addTypeFromDefExpr(typeProcessor, context, jsElement);
			return true;
		}
		return false;
	}

	private static void tryAddBindowsType(final JSReferenceExpression qualifier, final TypeProcessor typeProcessor, final EvaluateContext context)
	{
		final PsiElement element = qualifier.getParent();
		if(!(element instanceof JSCallExpression))
		{
			return;
		}
		final JSArgumentList argumentList = ((JSCallExpression) element).getArgumentList();
		if(argumentList == null)
		{
			return;
		}
		final JSExpression[] expressions = argumentList.getArguments();
		if(expressions.length == 0 || !(expressions[0] instanceof JSLiteralExpression))
		{
			return;
		}

		final String val = StringUtil.stripQuotesAroundValue(expressions[0].getText());
		final PsiElement contextElement = qualifier.getContainingFile().getContext();

		if(contextElement != null)
		{
			final PsiFile containingFile = contextElement.getContainingFile();

			if(isBindowsXml(containingFile))
			{
			}
		}
	}

	private static boolean isBindowsXml(final PsiFile containingFile)
	{
		return containingFile.getName().endsWith(".xml") && containingFile instanceof XmlFile;
	}

	private static void addTypeFromObjectLiteralExpression(final JSExpression rawqualifier, final TypeProcessor typeProcessor)
	{
		final JSProperty[] properties = ((JSObjectLiteralExpression) rawqualifier).getProperties();

		final boolean b = properties.length > 0 ? rawqualifier.processDeclarations((ResolveProcessor) typeProcessor, ResolveState.initial(),
				properties[0], properties[0]) : true;
		if(b)
		{
			typeProcessor.setUnknownElement(rawqualifier);
		}
	}

	private static void addTypeFromDefExpr(final TypeProcessor typeProcessor, final EvaluateContext context, final PsiElement psiElement)
	{
		final String type;
		type = psiElement.getText();

		if(!context.visitedTypes.contains(type))
		{
			context.visitedTypes.add(type);
			final PsiElement parentElement = psiElement.getParent();
			if(parentElement instanceof JSAssignmentExpression)
			{
				JSExpression expr = ((JSAssignmentExpression) parentElement).getROperand();
				while(expr instanceof JSAssignmentExpression)
				{
					expr = ((JSAssignmentExpression) expr).getROperand();
				}
				if(expr != null)
				{
					doEvalForExpr(expr, typeProcessor, context);
				}
			}
			else
			{
				typeProcessor.setUnknownElement(parentElement);
			}
		}
	}

	private static void getTypeFromConstant(final JSExpression rawqualifier, TypeProcessor typeProcessor, EvaluateContext context)
	{
		final ASTNode childNode = rawqualifier.getNode().getFirstChildNode();
		IElementType constantType = childNode.getElementType();

		String type = JavaScriptTokenSets.STRING_LITERALS.contains(constantType) ? STRING_TYPE_NAME :
				constantType == JSTokenTypes.NUMERIC_LITERAL ? NUMBER_TYPE_NAME : constantType == JSTokenTypes.REGEXP_LITERAL ? REG_EXP_TYPE_NAME : constantType
						== JSTokenTypes.XML_START_TAG_START ? XML_TYPE_NAME : constantType == JSTokenTypes.XML_START_TAG_LIST ? XML_LIST_TYPE_NAME : constantType ==
						JSTokenTypes.TRUE_KEYWORD || constantType == JSTokenTypes.FALSE_KEYWORD ? BOOLEAN_TYPE_NAME : null;

		if(type == NUMBER_TYPE_NAME && typeProcessor.ecma())
		{
			final String text = childNode.getText();
			if(text.indexOf('.') == -1)
			{
				type = INT_TYPE;
			}
		}

		if(type != null)
		{
			addType(type, typeProcessor, context, rawqualifier);
		}
		else
		{
			typeProcessor.setUnknownElement(rawqualifier);
		}
	}

	protected void addPackageScope(final List<String[]> possibleNameIds, final JSClass jsClass, final PsiElement expression)
	{
		final String packageQualifier = JSResolveUtil.findPackageStatementQualifier(expression);
		String qName;

		if(packageQualifier != null)
		{
			buildIndexListFromQNameAndCorrectQName(packageQualifier, jsClass, possibleNameIds);
		}
		else if(jsClass != null && (qName = jsClass.getQualifiedName()) != null && !qName.equals(jsClass.getName()))
		{
			final int index = qName.lastIndexOf('.');
			if(index > 0)
			{
				buildIndexListFromQNameAndCorrectQName(qName.substring(0, index), jsClass, possibleNameIds);
			}
		}
		else if(jsClass == null)
		{
			String s = JSResolveUtil.findPackageForMxml(expression);
			if(isValidType(s))
			{
				buildIndexListFromQNameAndCorrectQName(s, expression, possibleNameIds);
			}
		}
	}

	protected String buildIndexListFromQNameAndCorrectQName(String type, final PsiElement source, List<String[]> possibleNameIds)
	{
		final List<String> list = new ArrayList<String>();
		type = addIndexListFromQName(type, source, list);

		possibleNameIds.add(ArrayUtil.toStringArray(list));
		return type;
	}

	public static String addIndexListFromQName(String type, final PsiElement source, final List<String> list)
	{
		int i = type.indexOf('.');
		int lastI = 0;

		while(i != -1)
		{
			list.add(type.substring(lastI, i));
			lastI = i + 1;
			i = type.indexOf('.', lastI);
		}

		if(i != type.length())
		{
			String s = type.substring(lastI, type.length());
			if(source == null)
			{
				s = StringUtil.capitalize(s); // when type name goes from comments
			}
			if(lastI == 0 && !s.equals(type))
			{
				type = s;
			}
			list.add(s);
		}
		return type;
	}

	private static void addType(String type, TypeProcessor typeProcessor, EvaluateContext context, final PsiElement source)
	{
		if(type == null)
		{
			return;
		}
		int spacePos = type.indexOf(' ');
		if(spacePos != -1)
		{
			type = type.substring(0, spacePos);
		}

		if(!(typeProcessor instanceof GenericTypeParametersClient))
		{
			final int gtPos = type.indexOf('<');
			if(gtPos > 0 && type.charAt(gtPos - 1) == '.')
			{
				type = type.substring(0, gtPos - 1);
			}
		}

		if(!typeProcessor.ecma())
		{
			type = JSTypeEvaluateManager.getInstanceNameByType(type);
		}
		typeProcessor.process(type, context, source);
	}

	@Override
	public <T> T getHint(final Key<T> hintClass)
	{
		return null;
	}

	@Override
	public void handleEvent(Event event, Object associated)
	{
	}

	enum MatchType
	{
		COMPLETE_TYPE, COMPLETE, COMPLETE_NS, PARTIAL, NOMATCH
	}

	interface HierarchyProcessor
	{
		boolean processClass(JSClass clazz);
	}

	protected void doIterateTypeHierarchy(final String[] contextIds, final HierarchyProcessor processor)
	{
		StringBuilder builder = new StringBuilder();
		for(String cnameId : contextIds)
		{
			if(builder.length() > 0)
			{
				builder.append('.');
			}
			builder.append(cnameId);
		}

		final String typeName = builder.toString();
		if(typeName.length() == 0)
		{
			return;
		}

		myIteratedTypeName = typeName;
		doIterateHierarchy(typeName, processor);
		myIteratedTypeName = null;
	}

	protected void doIterateHierarchy(final String typeName, final HierarchyProcessor processor)
	{
		final PsiElement clazz = JSResolveUtil.findClassByQName(typeName, myContext);
		if(clazz instanceof JSClass)
		{
			for(JSClass superClazz : ((JSClass) clazz).getSuperClasses())
			{
				superClazz.processDeclarations(new ResolveProcessor(null)
				{
					{
						setToProcessMembers(false);
						setToProcessHierarchy(true);
						setTypeContext(true);
					}

					@Override
					public boolean execute(final PsiElement element, final ResolveState state)
					{
						if(element instanceof JSClass)
						{
							if(!processor.processClass((JSClass) element))
							{
								return false;
							}
						}
						return true;
					}
				}, ResolveState.initial(), null, clazz);
			}

			return;
		}
	}


	public static class EvaluateContext
	{
		public final PsiFile targetFile;
		public final Set<String> visitedTypes = new THashSet<String>();
		private Set<JSExpression> processingItems;
		private PsiElement source;

		public EvaluateContext(final PsiFile targetFile)
		{
			this.targetFile = targetFile;
		}

		public boolean isAlreadyProcessingItem(final JSExpression rawqualifier)
		{
			if(processingItems != null)
			{
				return processingItems.contains(rawqualifier);
			}
			return false;
		}

		public void addProcessingItem(final JSExpression rawqualifier)
		{
			if(processingItems == null)
			{
				processingItems = new THashSet<JSExpression>();
			}
			processingItems.add(rawqualifier);
		}

		public void removeProcessingItem(final JSExpression rawqualifier)
		{
			processingItems.remove(rawqualifier);
		}

		public PsiElement getSource()
		{
			return source;
		}

		public void setSource(final PsiElement source)
		{
			this.source = source;
		}
	}

	public interface TypeProcessor
	{
		void process(@Nonnull String type, @Nonnull EvaluateContext evaluateContext, final PsiElement source);

		@Deprecated
		boolean ecma();

		Set<JavaScriptFeature> getFeatures();

		void setUnknownElement(@Nonnull PsiElement element);
	}

	public interface GenericTypeParametersClient
	{
	}

	public static class SimpleTypeProcessor extends ResolveProcessor implements TypeProcessor, GenericTypeParametersClient
	{
		private String type;
		private PsiElement result;
		private PsiElement source;
		private final Set<JavaScriptFeature> myFeatures;

		public SimpleTypeProcessor(Set<JavaScriptFeature> features)
		{
			super(null);
			myFeatures = features;
		}

		@Override
		public void process(@Nonnull final String _type, @Nonnull final EvaluateContext evaluateContext, final PsiElement _source)
		{
			setType(type != null && !type.equals(_type) ? ANY_TYPE : _type, _source);
		}

		private void setType(final String s, final PsiElement _source)
		{
			type = s;
			source = _source;
		}

		@Override
		public Set<JavaScriptFeature> getFeatures()
		{
			return myFeatures;
		}

		@Override
		public boolean ecma()
		{
			return myFeatures.contains(JavaScriptFeature.CLASS);
		}

		@Override
		public boolean execute(final PsiElement element, final ResolveState state)
		{
			result = element;
			return true;
		}

		@Override
		public void setUnknownElement(@Nonnull final PsiElement _element)
		{
			setType(ANY_TYPE, _element);
		}

		public String getType()
		{
			return type;
		}

		public PsiElement getSource()
		{
			return source;
		}
	}

	public static class TagContextBuilder
	{
		public final PsiElement element;
		public final String typeName;

		public TagContextBuilder(PsiElement psiElement, String defaultName)
		{
			String typeName = null;
			final XmlTag tag = PsiTreeUtil.getParentOfType(psiElement, XmlTag.class);
			JSClass element = null;

			if(tag != null)
			{
				final PsiFile containingFile = tag.getContainingFile();

				if(isBindowsXml(containingFile))
				{
					typeName = "Bi" + tag.getLocalName();
				}
				else
				{
					element = JSResolveUtil.getClassFromTagNameInMxml(tag);
					typeName = element != null ? element.getQualifiedName() : null;
				}
			}

			if(typeName == null)
			{
				typeName = defaultName;
			}

			this.typeName = typeName;
			this.element = element;
		}
	}
}
