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

package com.intellij.lang.javascript.inspections;

import java.util.LinkedList;
import java.util.List;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.codeInsight.template.Template;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.AddImportECMAScriptClassOrFunctionAction;
import com.intellij.lang.javascript.formatter.JSCodeStyleSettings;
import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSThisExpression;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.util.JSUtils;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.codeStyle.CodeStyleSettingsManager;
import com.intellij.psi.util.PsiTreeUtil;

/**
 * @author Maxim.Mossienko
 */
public class JSUnresolvedVariableInspection extends JSInspection
{
	@NonNls
	private static final String SHORT_NAME = "JSUnresolvedVariable";

	@Override
	@NotNull
	public String getGroupDisplayName()
	{
		return JavaScriptBundle.message("js.inspection.group.name");
	}

	@Override
	@NotNull
	public String getDisplayName()
	{
		return JavaScriptBundle.message("js.unresolved.variable.inspection.name");
	}

	@Override
	@NotNull
	@NonNls
	public String getShortName()
	{
		return SHORT_NAME;
	}

	@Override
	protected JSElementVisitor createVisitor(final ProblemsHolder holder)
	{
		return new JSElementVisitor()
		{
			@Override
			public void visitJSReferenceExpression(final JSReferenceExpression node)
			{
				final PsiElement parentElement = node.getParent();

				if(node.shouldCheckReferences() && !(parentElement instanceof JSCallExpression))
				{
					final ResolveResult[] resolveResults = node.multiResolve(false);
					boolean emptyResolve = resolveResults.length == 0;
					boolean noCompleteResolve = true;

					for(ResolveResult r : resolveResults)
					{
						if(r.isValidResult())
						{
							noCompleteResolve = false;
							break;
						}
					}

					if(emptyResolve || noCompleteResolve)
					{
						final PsiElement nameIdentifier = node.getReferenceNameElement();

						if(nameIdentifier != null)
						{
							final List<LocalQuickFix> fixes = new LinkedList<LocalQuickFix>();
							final JSExpression qualifier = node.getQualifier();

							if(myOnTheFly)
							{
								final PsiFile containingFile = node.getContainingFile();
								final boolean ecma = containingFile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;

								if((qualifier == null ||
										JSUtils.isLHSExpression(qualifier) ||
										qualifier instanceof JSThisExpression) && (!(parentElement instanceof JSDefinitionExpression) || ecma))
								{
									final String referencedName = node.getReferencedName();
									boolean isField = qualifier != null;
									JSClass contextClass = null;

									if(!isField && ecma)
									{
										contextClass = JSResolveUtil.getClassOfContext(node);
										if(contextClass != null)
										{
											isField = true;
										}
									}

									if(!JSResolveUtil.isExprInTypeContext(node))
									{
										if(node.getParent() instanceof JSArgumentList)
										{
											fixes.add(new JSUnresolvedFunctionInspection.CreateJSFunctionIntentionAction(referencedName, !(qualifier == null || (qualifier instanceof
													JSThisExpression && ecma)))
											{
												@Override
												protected void addParameters(Template template, JSReferenceExpression referenceExpression, PsiFile file, boolean ecma)
												{
													JSExpression method = ((JSCallExpression) referenceExpression.getParent().getParent()).getMethodExpression();
													if(method instanceof JSReferenceExpression && "bindSetter".equals(((JSReferenceExpression) method).getReferencedName()))
													{
														MyExpression expression = new MyExpression("value");
														template.addVariable("value", expression, expression, false);
														if(ecma)
														{
															template.addTextSegment(":int");
														}
													}
												}

												@Override
												protected void addReturnType(Template template, JSReferenceExpression referenceExpression, PsiFile file)
												{
													template.addTextSegment("void");
												}
											});
										}

										boolean suggestCreateVar = true;

										JSClass targetClass = contextClass;

										if(qualifier instanceof JSReferenceExpression)
										{
											final JSClass clazz = JSResolveUtil.findClassOfQualifier(qualifier, containingFile);
											if(clazz != null)
											{
												targetClass = clazz;
											}
										}

										if(targetClass != null)
										{
											suggestCreateVar = !targetClass.isInterface();
										}

										if(suggestCreateVar)
										{
											fixes.add(new CreateJSVariableIntentionAction(referencedName, isField, false));

											if(ecma)
											{
												fixes.add(new CreateJSVariableIntentionAction(referencedName, isField, true));
											}
										}

										if(ecma)
										{
											boolean getter = !(node.getParent() instanceof JSDefinitionExpression);
											String invokedName = nameIdentifier.getText();
											fixes.add(new CreateJSPropertyAccessorIntentionAction(invokedName, getter));
											JSCallExpression expression = PsiTreeUtil.getParentOfType(node, JSCallExpression.class);

											if(expression != null)
											{
												final JSExpression methodExpression = expression.getMethodExpression();

												if(methodExpression instanceof JSReferenceExpression)
												{
													final String methodName = ((JSReferenceExpression) methodExpression).getReferencedName();

													if("addEventListener".equals(methodName) || "removeEventListener".equals(methodName))
													{
														final JSArgumentList argumentList = expression.getArgumentList();
														final JSExpression[] params = argumentList != null ? argumentList.getArguments() : JSExpression.EMPTY_ARRAY;

														if(params.length >= 2 && params[0] instanceof JSReferenceExpression)
														{
															final JSExpression eventNameQualifier = ((JSReferenceExpression) params[0]).getQualifier();
															if(eventNameQualifier != null)
															{
																fixes.add(new CreateJSEventMethod(invokedName, eventNameQualifier));
															}
														}
													}
												}
											}
										}
									}
									if(qualifier != null && !ecma)
									{
										fixes.add(new CreateJSNamespaceIntentionAction(referencedName));
									}

									if(ecma)
									{
										if(qualifier == null)
										{
											fixes.add(new AddImportECMAScriptClassOrFunctionAction(null, node));
											fixes.add(new CreateClassOrInterfaceAction(node, false));
											fixes.add(new CreateClassOrInterfaceAction(node, true));
										}
										else
										{
											fixes.add(new AddImportECMAScriptClassOrFunctionAction(null, node));
										}
									}
								}
							}

							final String key = node.getQualifier() == null ? JSResolveUtil.isExprInTypeContext(node) ? "javascript.unresolved.type.name.message" :
									"javascript.unresolved.variable.or.type.name.message" : "javascript.unresolved.variable.name.message";

							holder.registerProblem(nameIdentifier, JavaScriptBundle.message(key, node.getReferencedName()),
									JSUnresolvedFunctionInspection.getUnresolveReferenceHighlightType(qualifier, node),
									fixes.size() > 0 ? fixes.toArray(new LocalQuickFix[fixes.size()]) : null);
						}
					}
				}
				super.visitJSReferenceExpression(node);
			}
		};
	}

	private abstract static class BaseCreateJSVariableIntentionAction extends BaseCreateFix
	{
		protected final String myReferencedName;

		BaseCreateJSVariableIntentionAction(String referencedName)
		{
			myReferencedName = referencedName;
		}

		@Override
		@NotNull
		public String getFamilyName()
		{
			return JavaScriptBundle.message("javascript.create.variable.intention.family");
		}
	}

	private static class CreateJSNamespaceIntentionAction extends BaseCreateJSVariableIntentionAction
	{
		CreateJSNamespaceIntentionAction(String referencedName)
		{
			super(referencedName);
		}

		@Override
		@NotNull
		public String getName()
		{
			return JavaScriptBundle.message("javascript.create.namespace.intention.name", myReferencedName);
		}

		@Override
		protected void buildTemplate(final Template template, final JSReferenceExpression referenceExpression, final boolean ecma, boolean staticContext,
				final PsiFile file, final PsiElement anchorParent)
		{
			template.addTextSegment("/** @namespace ");
			template.addTextSegment(referenceExpression.getText() + " */");
			template.addEndVariable();
		}
	}

	private static class CreateJSVariableIntentionAction extends BaseCreateJSVariableIntentionAction
	{
		@NonNls
		private static final String VAR_STATEMENT_START = "var ";
		@NonNls
		private static final String CONSTANT_STATEMENT_START = "const ";
		private boolean isField;
		private boolean isConstant;

		CreateJSVariableIntentionAction(String referencedName, boolean isField, boolean isConstant)
		{
			super(referencedName);
			this.isField = isField;
			this.isConstant = isConstant;
		}

		@Override
		@NotNull
		public String getName()
		{
			return JavaScriptBundle.message(isField ? isConstant ? "javascript.create.constant.field.intention.name" : "javascript.create.property.intention.name" :
					isConstant ? "javascript.create.constant.intention.name" : "javascript.create.variable.intention.name", myReferencedName);
		}

		@Override
		protected void buildTemplate(final Template template, final JSReferenceExpression referenceExpression, final boolean ecma, boolean staticContext,
				final PsiFile file, final PsiElement anchorParent)
		{
			final JSExpression qualifier = addAccessModifier(template, referenceExpression, ecma, staticContext);
			if(qualifier == null || ecma)
			{
				template.addTextSegment(isConstant ? CONSTANT_STATEMENT_START : VAR_STATEMENT_START);
			}

			template.addTextSegment(ecma ? referenceExpression.getReferencedName() : referenceExpression.getText());
			template.addEndVariable();
			if(ecma)
			{
				template.addTextSegment(":");
			}
			else
			{
				template.addTextSegment(" = ");
			}

			if(ecma)
			{
				guessTypeAndAddTemplateVariable(template, referenceExpression, file);
				if(isConstant)
				{
					template.addTextSegment(" = ");
					addCompletionVar(template);
				}
			}
			else
			{
				addCompletionVar(template);
			}
			addSemicolonSegment(template, file);
		}

	}

	private static class CreateJSPropertyAccessorIntentionAction extends JSUnresolvedFunctionInspection.CreateJSFunctionIntentionActionBase
	{
		private final boolean myIsGetter;

		public CreateJSPropertyAccessorIntentionAction(String name, boolean getter)
		{
			super(name, getter ? "javascript.create.get.property.intention.name" : "javascript.create.set.property.intention.name");
			myIsGetter = getter;
		}

		@Override
		protected void writeFunctionAndName(Template template, String referencedName, boolean ecma)
		{
			template.addTextSegment("function ");
			template.addTextSegment(myIsGetter ? "get " : "set ");
			template.addTextSegment(referencedName);
		}

		@Override
		protected void addParameters(Template template, JSReferenceExpression refExpr, PsiFile file, boolean ecma)
		{
			if(!myIsGetter)
			{
				template.addTextSegment(refExpr.getReferencedName() + ":");
				guessTypeAndAddTemplateVariable(template, refExpr, file);
			}
		}

		@Override
		protected void addReturnType(Template template, JSReferenceExpression referenceExpression, PsiFile file)
		{
			if(myIsGetter)
			{
				guessTypeAndAddTemplateVariable(template, referenceExpression, file);
			}
			else
			{
				template.addTextSegment("void");
			}
		}

		@Override
		protected void addBody(Template template, JSReferenceExpression refExpr, PsiFile file)
		{
			String varName = refExpr.getReferencedName();
			String paramName = varName;
			JSCodeStyleSettings settings = CodeStyleSettingsManager.getInstance(file.getProject()).getCurrentSettings().getCustomSettings(JSCodeStyleSettings
					.class);
			varName = settings.FIELD_PREFIX + varName;

			if(varName.equals(paramName))
			{
				varName = StringUtil.fixVariableNameDerivedFromPropertyName(varName);
			}

			if(myIsGetter)
			{
				template.addTextSegment("return ");

				addVarName(template, varName);
				template.addEndVariable();
			}
			else
			{
				addVarName(template, varName);
				template.addEndVariable();
				template.addTextSegment(" = " + paramName);
			}
			addSemicolonSegment(template, file);
		}

		private static void addVarName(Template template, String varName)
		{
			MyExpression expression = new MyExpression(varName);
			template.addVariable("name", expression, expression, true);
		}

	}

	private static class CreateJSEventMethod extends JSUnresolvedFunctionInspection.CreateJSFunctionIntentionActionBase
	{
		private JSExpression myEventQualifier;

		public CreateJSEventMethod(String invokedName, JSExpression eventNameQualifier)
		{
			super(invokedName, "javascript.create.event.handler.intention.name");
			myEventQualifier = eventNameQualifier;
		}


		@Override
		protected void addParameters(Template template, JSReferenceExpression refExpr, PsiFile file, boolean ecma)
		{
			template.addTextSegment("event:");
			template.addTextSegment(myEventQualifier.getText());
		}

		@Override
		protected void addReturnType(Template template, JSReferenceExpression referenceExpression, PsiFile psifile)
		{
			template.addTextSegment("void");
		}

		@Override
		protected void addBody(Template template, JSReferenceExpression refExpr, PsiFile file)
		{
			template.addEndVariable();
		}
	}
}
