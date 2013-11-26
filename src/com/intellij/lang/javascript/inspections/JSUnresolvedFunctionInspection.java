/*
 * Copyright 2000-2006 JetBrains s.r.o.
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

import com.intellij.codeInsight.template.Expression;
import com.intellij.codeInsight.template.Template;
import com.intellij.codeInsight.template.TemplateManager;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.validation.JSAnnotatingVisitor;
import com.intellij.lang.javascript.flex.AddImportECMAScriptClassOrFunctionAction;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSPackageWrapper;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.util.JSUtils;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileTypes.StdFileTypes;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.PropertyKey;

import java.util.LinkedList;
import java.util.List;

/**
 * @by Maxim.Mossienko
 */
public class JSUnresolvedFunctionInspection extends JSInspection {
  @NonNls private static final String SHORT_NAME = "JSUnresolvedFunction";

  @NotNull
  public String getGroupDisplayName() {
    return JSBundle.message("js.inspection.group.name");
  }

  @NotNull
  public String getDisplayName() {
    return JSBundle.message("js.unresolved.function.inspection.name");
  }

  @NotNull
  @NonNls
  public String getShortName() {
    return SHORT_NAME;
  }

  protected JSElementVisitor createVisitor(final ProblemsHolder holder) {
    return new JSElementVisitor() {
      @Override public void visitJSCallExpression(final JSCallExpression node) {
        final JSExpression methodExpression = node.getMethodExpression();

        if (methodExpression instanceof JSReferenceExpression) {
          final JSReferenceExpression referenceExpression = (JSReferenceExpression)methodExpression;
          final ResolveResult[] resolveResults = referenceExpression.multiResolve(false);

          boolean noCompleteResolve = true;

          boolean inNewExpression = node instanceof JSNewExpression;
          for(ResolveResult r:resolveResults) {
            if (r.isValidResult()) {
              noCompleteResolve = false;

              PsiElement element = r.getElement();

              if(element instanceof JSVariable) {
                String typeText = ((JSVariable)element).getTypeString();

                if (typeText != null && !"*".equals(typeText)) {
                  if (!allowMemberReference(inNewExpression, typeText)) {
                    holder.registerProblem(
                      ((JSReferenceExpression)methodExpression).getReferenceNameElement(),
                      JSBundle.message("javascript.term.does.not.evaluate.to.function"),
                      getHighlightTypeForTypeOrSignatureProblem(node)
                    );
                  }
                }
              } else if (element instanceof JSFunction && ((JSFunction)element).isGetProperty()) {
                String typeText = ((JSFunction)element).getReturnTypeString();

                if (!allowMemberReference(inNewExpression, typeText)) {
                  JSArgumentList argumentList = node.getArgumentList();
                  LocalQuickFix fixes[] = LocalQuickFix.EMPTY_ARRAY;

                  if (argumentList != null) {
                    fixes = new LocalQuickFix[] {
                      new JSAnnotatingVisitor.RemoveASTNodeFix(
                        argumentList.getNode(),
                        "javascript.term.does.not.evaluate.to.function2.fix")
                    };
                  }

                  holder.registerProblem(
                    ((JSReferenceExpression)methodExpression).getReferenceNameElement(),
                    JSBundle.message("javascript.term.does.not.evaluate.to.function2"),
                    getHighlightTypeForTypeOrSignatureProblem(node),
                    fixes
                  );
                }
              }
              break;
            }
          }

          if (resolveResults.length == 0 || noCompleteResolve) {
            final JSExpression qualifier = referenceExpression.getQualifier();
            final List<LocalQuickFix> quickFixes = new LinkedList<LocalQuickFix>();
            final String refName = referenceExpression.getReferencedName();

            if (myOnTheFly &&
                ((qualifier == null || qualifier instanceof JSThisExpression) || 
                 JSUtils.isLHSExpression(qualifier))
                ) {
              if (methodExpression.getParent() instanceof JSCallExpression) {
                boolean simpleJs = node.getContainingFile().getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4;

                if (!inNewExpression || simpleJs) {
                  quickFixes.add( new CreateJSFunctionIntentionAction(refName, true));
                }

                if (qualifier == null) {
                  if (simpleJs) {
                    quickFixes.add(
                      new CreateJSFunctionIntentionAction(refName, false)
                    );
                  } else {
                    quickFixes.add(new AddImportECMAScriptClassOrFunctionAction(null, referenceExpression));
                    if (inNewExpression) quickFixes.add(new CreateClassOrInterfaceAction(referenceExpression, false));
                  }
                }
              }
            }

            final PsiElement referenceNameElement = referenceExpression.getReferenceNameElement();

            if (referenceNameElement != null) {
              holder.registerProblem(referenceNameElement,
                JSBundle.message(
                  inNewExpression ?
                    "javascript.unresolved.type.name.message":
                    "javascript.unresolved.function.name.message", refName
                ),
                getUnresolveReferenceHighlightType(qualifier, node),
                quickFixes.size() > 0 ? quickFixes.toArray(new LocalQuickFix[quickFixes.size()]):null
              );
            }
          } else {
            PsiElement element = resolveResults[0].getElement();

            if (inNewExpression && element instanceof JSClass && ((JSClass)element).isInterface()) {
              final PsiElement referenceNameElement = referenceExpression.getReferenceNameElement();

              holder.registerProblem(referenceNameElement,
                JSBundle.message("javascript.interface.can.not.be.instantiated.message"),
                getUnresolveReferenceHighlightType(referenceExpression.getQualifier(), node));
            } else {
              checkFunction(node, element, holder);
            }
          }
        } else if (methodExpression instanceof JSSuperExpression) {
          final PsiElement element = (methodExpression.getReference()).resolve();

          if (element != null) {
            checkFunction(node, element, holder);
          }
        } else if (methodExpression instanceof JSNewExpression) {
          JSExpression methodExpr = ((JSNewExpression)methodExpression).getMethodExpression();

          if (methodExpr instanceof JSReferenceExpression) {
            ResolveResult[] results = ((JSReferenceExpression)methodExpr).multiResolve(false);
            PsiElement elt;

            if (results.length > 0 &&
                ( (elt = results[0].getElement()) instanceof JSFunction && ((JSFunction)elt).isConstructor() ||
                  elt instanceof JSClass
                )) {
              holder.registerProblem(methodExpression,
                JSBundle.message(
                    "javascript.term.does.not.evaluate.to.function"
                ),
                getUnresolveReferenceHighlightType(null, node)
              );
            }
          }
        }

        super.visitJSCallExpression(node);
      }

      @Override
      public void visitJSAssignmentExpression(final JSAssignmentExpression node) {
        final JSExpression lOperand = node.getLOperand();
        if (lOperand == null) return;
        final JSExpression rOperand = node.getROperand();
        if (rOperand == null) return;

        final PsiFile containingFile = node.getContainingFile();
        if (containingFile.getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4) return;
        String expressionType = null;

        if (lOperand instanceof JSDefinitionExpression) {
          JSExpression expression = ((JSDefinitionExpression)lOperand).getExpression();
          if (expression instanceof JSReferenceExpression) {
            PsiElement resolve = JSResolveUtil.unwrapProxy(((JSReferenceExpression)expression).resolve());

            if (resolve instanceof JSNamedElement) {
              expressionType = JSResolveUtil.getTypeFromSetAccessor((JSNamedElement)resolve);
              if (expressionType != null) expressionType = JSImportHandlingUtil.resolveTypeName(expressionType, resolve);
            }
          }
        }

        if (expressionType == null) {
          expressionType = JSResolveUtil.getQualifiedExpressionType(lOperand, containingFile);
        }
        checkExpressionIsAssignableToType(
            rOperand, expressionType,
            holder,
            containingFile,
            "javascript.assigned.expression.type.mismatch"
        );
      }

      @Override
      public void visitJSReturnStatement(final JSReturnStatement node) {
        final JSExpression expression = node.getExpression();
        if (expression == null) return;

        final JSFunction fun = PsiTreeUtil.getParentOfType(node, JSFunction.class);
        if (fun == null) return; // TODO: complain about it
        final String typeString = fun.getReturnTypeString();
        if (typeString == null) return;
        final PsiFile containingFile = fun.getContainingFile();
        if (containingFile.getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4) return;

        checkExpressionIsAssignableToType(
            expression,
            JSImportHandlingUtil.resolveTypeName(typeString, fun),
            holder,
            containingFile,
            "javascript.returned.expression.type.mismatch"
        );
      }

      @Override
      public void visitJSVariable(final JSVariable node) {
        final JSExpression initializer = node.getInitializer();
        if (initializer == null) return;
        checkExpressionIsAssignableToVariable(node, initializer, holder, node.getContainingFile(), "javascript.initializer.type.mismatch");
      }

      @Override
      public void visitJSBinaryExpression(JSBinaryExpression node) {
        IElementType sign = node.getOperationSign();

        if (sign == JSTokenTypes.AS_KEYWORD || sign == JSTokenTypes.IS_KEYWORD) {
          JSExpression rOperand = node.getROperand();
          
          if (rOperand instanceof JSReferenceExpression) {
            ResolveResult[] results = ((JSReferenceExpression)rOperand).multiResolve(false);

            if (results.length > 0 && results[0].getElement() instanceof JSVariable) {
              checkTypeIs(rOperand, rOperand, holder, "Class", "javascript.binary.operand.type.mismatch");
            }
          }
        }
      }

      @Override
      public void visitJSForInStatement(JSForInStatement node) {
        if (!node.isForEach()) {
          JSVarStatement statement = node.getDeclarationStatement();

          if (statement != null) {
            String expressionType = JSResolveUtil.getQualifiedExpressionType(node.getCollectionExpression(), node.getContainingFile());
            boolean isDictionary = "flash.utils.Dictionary".equals(expressionType);

            for (JSVariable var: statement.getVariables()) {
              PsiElement typeElement = var.getTypeElement();
              String typeElementText;

              if (typeElement != null &&
                  "Array".equals(expressionType) &&
                  ( "Object".equals(typeElementText = typeElement.getText()) || "*".equals(typeElementText))
                ) {
                holder.registerProblem(typeElement, JSBundle.message("javascript.incorrect.array.type.in.for-in"),ProblemHighlightType.GENERIC_ERROR_OR_WARNING);
                continue;
              }

              if (isDictionary && typeElement != null && "Object".equals(typeElement.getText())) {
                continue;
              }
              checkTypeIs(typeElement, typeElement, holder, "XMLList".equals(expressionType) ? "XML":"String", "javascript.incorrect.variable.type.mismatch");
            }
          }
        }
      }
    };
  }

  private static boolean allowMemberReference(boolean inNewExpression, String typeText) {
    return ("Class".equals(typeText) && inNewExpression) ||
           "Function".equals(typeText);
  }

  private static void checkTypeIs(PsiElement type, PsiElement node, ProblemsHolder holder, String typeName, String key) {
    if (type instanceof JSReferenceExpression) {
      checkTypeIs((JSExpression)type, node, holder, typeName, key);
    } else if (type != null) {
      holder.registerProblem(node, JSBundle.message(key, typeName, type.getText()),
           getHighlightTypeForTypeOrSignatureProblem(node));
    }
  }

  private static void checkTypeIs(JSExpression rOperand, PsiElement node, ProblemsHolder holder, String typeName, String key) {
    String expressionType = JSResolveUtil.getQualifiedExpressionType(rOperand, rOperand.getContainingFile());
    if (!typeName.equals(expressionType)) {
      holder.registerProblem(node, JSBundle.message(key, typeName, expressionType),
                   getHighlightTypeForTypeOrSignatureProblem(node)
      );
    }
  }

  static ProblemHighlightType getUnresolveReferenceHighlightType(final @Nullable JSExpression qualifier, @NotNull JSExpression node) {
    JSClass jsClass;

    final PsiFile containingFile = node.getContainingFile();
    if (qualifier != null) {
      jsClass = JSResolveUtil.findClassOfQualifier(qualifier, containingFile);

      if (jsClass == null && (qualifier instanceof JSReferenceExpression)) {
        ResolveResult[] results = ((JSReferenceExpression)qualifier).multiResolve(false);

        if (results.length != 0) {
          PsiElement resultElement = results[0].getElement();
          if (resultElement instanceof JSPackageWrapper) return ProblemHighlightType.ERROR;
          String type = null;

          if (resultElement instanceof JSVariable) {
            type = ((JSVariable)resultElement).getTypeString();
          } else if (resultElement instanceof JSFunction) {
            type = ((JSFunction)resultElement).getReturnTypeString();
          }

          if ("*".equals(type)) {
            return ProblemHighlightType.LIKE_UNKNOWN_SYMBOL;
          }
          jsClass = JSResolveUtil.getClassOfContext(resultElement);
        }
      }
    } else {
      jsClass = JSResolveUtil.getClassOfContext(node);
    }

    final boolean ecmaL4File = containingFile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;

    if (jsClass != null && ecmaL4File &&
        (!(jsClass instanceof XmlBackedJSClassImpl) ||
         jsClass.getContainingFile().getFileType() == StdFileTypes.XML
        )
       ) {
      final JSAttributeList attributeList = jsClass.getAttributeList();
      if (attributeList == null || !attributeList.hasModifier(JSAttributeList.ModifierType.DYNAMIC)) {
        return ProblemHighlightType.ERROR;
      }
    }

    if (ecmaL4File && jsClass == null && node.getParent() instanceof JSReferenceList) {
      return ProblemHighlightType.ERROR;
    }
    return ProblemHighlightType.LIKE_UNKNOWN_SYMBOL;
  }

  private static void checkFunction(final JSCallExpression node, final PsiElement element,
                             final ProblemsHolder holder) {
    if (element instanceof JSFunction) {
      final JSFunction function = (JSFunction)element;
      if (!function.isGetProperty() || !"Function".equals(function.getReturnTypeString())) {
        final JSParameterList parameterList = function.getParameterList();

        if (parameterList != null) {
          checkCallParameters(node, parameterList.getParameters(), function.isReferencesArguments(), holder);
        }
      }
    } else if (element instanceof JSClass) {
      if (node instanceof JSNewExpression || node.getMethodExpression() instanceof JSSuperExpression) {
        checkCallParameters(node, JSParameter.EMPTY_ARRAY, false, holder);
      } else {
        JSArgumentList argumentList = node.getArgumentList();
        if (argumentList == null || argumentList.getArguments().length != 1) {
          holder
            .registerProblem(argumentList != null ? argumentList : node, JSBundle.message("javascript.invalid.number.of.parameters", "one"),
                             getHighlightTypeForTypeOrSignatureProblem(node));
        }
      }
    }
  }

  private static void checkCallParameters(final JSCallExpression node, final JSParameter[] parameters, boolean functionReferencesArguments,
                                          final ProblemsHolder holder) {
    final JSArgumentList argumentList = node.getArgumentList();
    final JSExpression[] expressions = argumentList != null ? argumentList.getArguments(): JSExpression.EMPTY_ARRAY;

    boolean lastIsRest = false;
    int minParameterLength = 0;
    int maxParameterLength = parameters.length;

    for(int i = 0; i < parameters.length; ++i) {
      final JSParameter parameter = parameters[i];
      if (parameter.isOptional()) break;

      if (i == parameters.length - 1 && parameter.isRest()) {
        lastIsRest = true;
        maxParameterLength = Integer.MAX_VALUE;
        break;
      }
      minParameterLength ++;
    }

    if (!lastIsRest && parameters.length > 0 && parameters[parameters.length - 1].isRest()) {
      lastIsRest = true;
      maxParameterLength = Integer.MAX_VALUE;
    }

    if (( expressions.length < minParameterLength ||
          expressions.length > maxParameterLength
        ) && !functionReferencesArguments
        ) {
      final String s = (lastIsRest ? minParameterLength + " or more ": String.valueOf(minParameterLength) +
                                                                       (minParameterLength != maxParameterLength ? ".." +maxParameterLength:""));
      holder.registerProblem(argumentList != null ? argumentList:node,
        JSBundle.message(
          "javascript.invalid.number.of.parameters", s
        ), getHighlightTypeForTypeOrSignatureProblem(node)
      );
    } else {
      int i = 0;
      final PsiFile containingFile = node.getContainingFile();

      for(JSParameter p:parameters) {
        if (i == expressions.length) break;
        if (p.isRest()) break;
        checkExpressionIsAssignableToVariable(p, expressions[i], holder, containingFile, "javascript.argument.type.mismatch");
        ++i;
      }
    }
  }

  private static void checkExpressionIsAssignableToVariable(final JSVariable p, JSExpression expr, final ProblemsHolder holder,
                                                            final PsiFile containingFile,
                                                            @PropertyKey(resourceBundle = JSBundle.BUNDLE) String problemKey) {
    final String parameterTypeResolved = JSImportHandlingUtil.resolveTypeName(p.getTypeString(), p);
    checkExpressionIsAssignableToType(expr, parameterTypeResolved, holder, containingFile, problemKey);
  }

  private static void checkExpressionIsAssignableToType(final JSExpression expr, final String type, final ProblemsHolder holder,
                                                        final PsiFile containingFile,
                                                        final String problemKey) {
    if ("*".equals(type) || type == null) return; // optimization
    final String expressionType = JSResolveUtil.getQualifiedExpressionType(expr, containingFile);

    if (!JSResolveUtil.isAssignableType(type, expressionType, containingFile)) {
      holder.registerProblem(expr, JSBundle.message(problemKey, type, expressionType),
                             getHighlightTypeForTypeOrSignatureProblem(expr), new JSInsertCastFix(type)
      );
    }
  }

  private static ProblemHighlightType getHighlightTypeForTypeOrSignatureProblem(@NotNull PsiElement node) {
    if (node.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
      return ProblemHighlightType.GENERIC_ERROR;
    }
    return ProblemHighlightType.GENERIC_ERROR_OR_WARNING;
  }

  static abstract class CreateJSFunctionIntentionActionBase extends BaseCreateFix {
    private final String myName;
    private final String myIntentionNameKey;

    CreateJSFunctionIntentionActionBase(String name, @PropertyKey(resourceBundle = JSBundle.BUNDLE) String nameKey) {
      myName = name;
      myIntentionNameKey = nameKey;
    }

    @NotNull
    public String getName() {
      return JSBundle.message(
        myIntentionNameKey,
        myName
      );
    }

    @NotNull
    public String getFamilyName() {
      return JSBundle.message("javascript.create.function.intention.family");
    }

    protected void buildTemplate(Template template, JSReferenceExpression referenceExpression, boolean ecma, boolean staticContext, PsiFile file,
                                 PsiElement anchorParent) {
      String referencedName = ecma ? referenceExpression.getReferencedName():referenceExpression.getText();
      addAccessModifier(template, referenceExpression, ecma, staticContext);
      writeFunctionAndName(template, referencedName, ecma);
      template.addTextSegment("(");

      addParameters(template, referenceExpression, file, ecma);

      template.addTextSegment(")");

      if (ecma) {
        template.addTextSegment(":");
        addReturnType(template, referenceExpression, file);
      }

      JSClass clazz = findClass(file, anchorParent);
      if (clazz == null || !clazz.isInterface())  {
        template.addTextSegment(" {");
        addBody(template, referenceExpression, file);
        template.addTextSegment("}");
      } else {
        addSemicolonSegment(template, file);
        template.addEndVariable();
      }
    }

    protected void writeFunctionAndName(Template template, String referencedName, boolean ecma) {
      template.addTextSegment("function ");
      template.addTextSegment(referencedName);
    }

    protected abstract void addParameters(Template template, JSReferenceExpression refExpr, PsiFile file, boolean ecma);
    protected abstract void addReturnType(Template template, JSReferenceExpression referenceExpression, PsiFile psifile);
    protected abstract void addBody(Template template, JSReferenceExpression refExpr, PsiFile file);
  }

  static class CreateJSFunctionIntentionAction extends CreateJSFunctionIntentionActionBase {
    private final boolean myIsMethod;

    CreateJSFunctionIntentionAction(String name,boolean isMethod) {
      super(name, isMethod ? "javascript.create.method.intention.name":"javascript.create.function.intention.name");
      myIsMethod = isMethod;
    }

    @Override
    protected void writeFunctionAndName(Template template, String createdMethodName, boolean ecma) {
      if (!myIsMethod || ecma) template.addTextSegment("function ");
      template.addTextSegment(createdMethodName);
      if (myIsMethod && !ecma) template.addTextSegment(" = function ");
    }

    protected void addParameters(Template template, JSReferenceExpression referenceExpression, PsiFile file, boolean ecma) {
      JSCallExpression methodInvokation = (JSCallExpression)referenceExpression.getParent();
      final JSArgumentList list = methodInvokation.getArgumentList();
      final JSExpression[] expressions = list.getArguments();
      int paramCount = expressions.length;

      for(int i = 0 ; i < paramCount; ++i) {
        if (i != 0) template.addTextSegment(", ");
        String var = null;

        final JSExpression passedParameterValue = expressions[i];
        if (passedParameterValue instanceof JSReferenceExpression) {
          var = ((JSReferenceExpression)passedParameterValue).getReferencedName();
        }

        if (var == null || var.length() == 0) var = "param" + (i != 0 ? Integer.toString(i + 1):"");

        final String var1 = var;
        Expression expression = new MyExpression(var1);

        template.addVariable(var, expression, expression, true);
        if (ecma) {
          template.addTextSegment(":");
          guessExprTypeAndAddSuchVariable(passedParameterValue, template, var1, file, ecma);
        }
      }

    }

    protected void addReturnType(Template template, JSReferenceExpression referenceExpression, PsiFile file) {
      guessTypeAndAddTemplateVariable(template, referenceExpression, file);
    }

    protected void addBody(Template template, JSReferenceExpression refExpr, PsiFile file) {
      template.addEndVariable();
    }

    protected void buildTemplate(final Template template, JSReferenceExpression referenceExpression,
                                 boolean ecma, boolean staticContext, PsiFile file, PsiElement anchorParent) {
      super.buildTemplate(template, referenceExpression, ecma, staticContext, file, anchorParent);

      if (myIsMethod && !ecma) {
        addSemicolonSegment(template, file);
      }
    }
  }

  private static class JSInsertCastFix implements LocalQuickFix {
    private final String type;

    public JSInsertCastFix(final String type) {
      this.type = type;
    }

    @NotNull
    public String getName() {
      return JSBundle.message("javascript.insert.cast.fix");
    }

    @NotNull
    public String getFamilyName() {
      return getName();
    }

    public void applyFix(@NotNull final Project project, @NotNull final ProblemDescriptor descriptor) {
      final PsiElement element = descriptor.getPsiElement();
      final Editor editor = BaseCreateFix.getEditor(project, element.getContainingFile());
      if (editor == null) return;

      final String shortenedType = JSResolveUtil.getShortenedType(this.type, element);
      final TemplateManager templateManager = TemplateManager.getInstance(project);
      Template template = templateManager.createTemplate("","", shortenedType + "($SELECTION$)");
      template.setToReformat(true);

      final int offset = element.getTextOffset();
      editor.getSelectionModel().setSelection(offset, offset + element.getTextLength());
      editor.getCaretModel().moveToOffset(offset);
      templateManager.startTemplate(editor, element.getText(), template);
    }
  }
}
