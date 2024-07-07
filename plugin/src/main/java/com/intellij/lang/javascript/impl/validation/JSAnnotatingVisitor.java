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

package com.intellij.lang.javascript.impl.validation;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.impl.JSPackageStatementImpl;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.util.JSUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.codeEditor.Editor;
import consulo.document.util.TextRange;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import consulo.javascript.psi.JavaScriptLambdaExpression;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.editor.FileModificationService;
import consulo.language.editor.annotation.*;
import consulo.language.editor.impl.intention.RenameFileFix;
import consulo.language.editor.inspection.LocalQuickFix;
import consulo.language.editor.inspection.LocalQuickFixProvider;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.editor.intention.IntentionAction;
import consulo.language.editor.intention.SyntheticIntentionAction;
import consulo.language.editor.template.Template;
import consulo.language.editor.template.TemplateManager;
import consulo.language.editor.template.macro.CompleteMacro;
import consulo.language.editor.template.macro.MacroCallNode;
import consulo.language.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.module.content.ProjectRootManager;
import consulo.project.Project;
import consulo.util.lang.ref.Ref;
import consulo.virtualFileSystem.VirtualFile;
import consulo.xml.psi.xml.XmlAttributeValue;
import consulo.xml.psi.xml.XmlTagChild;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import java.util.*;

/**
 * @by max, maxim.mossienko
 */
public class JSAnnotatingVisitor extends JSElementVisitor implements Annotator {
  private AnnotationHolder myHolder;

  @Override
  public synchronized void annotate(PsiElement psiElement, AnnotationHolder holder) {
    myHolder = holder;
    psiElement.accept(this);
    myHolder = null;
  }

  @Override
  public void visitJSAttributeNameValuePair(final JSAttributeNameValuePair attributeNameValuePair) {
    checkReferences(attributeNameValuePair, HighlightSeverity.ERROR);
  }

  @Override
  public void visitJSIncludeDirective(final JSIncludeDirective includeDirective) {
    checkReferences(includeDirective, HighlightSeverity.ERROR);
  }

  @Override
  public void visitJSLiteralExpression(JSSimpleLiteralExpression node) {
    checkReferences(node, HighlightSeverity.ERROR);
  }

  @RequiredReadAction
  private void checkReferences(final PsiElement includeDirective, HighlightSeverity kind) {
    for (PsiReference ref : includeDirective.getReferences()) {
      if (!ref.isSoft() && hasBadResolve(ref)) {
        final TextRange elementRange = ref.getElement().getTextRange();
        final TextRange textRange = ref.getRangeInElement();

        final TextRange range = new TextRange(
          elementRange.getStartOffset() + textRange.getStartOffset(),
          elementRange.getStartOffset() + textRange.getEndOffset()
        );
        final LocalizeValue value = ((EmptyResolveMessageProvider) ref).buildUnresolvedMessaged(ref.getCanonicalText());
        AnnotationBuilder builder = myHolder.newAnnotation(kind, value);
        builder = builder.range(range);

        if (ref instanceof LocalQuickFixProvider localQuickFixProvider) {
          for (LocalQuickFix fix : localQuickFixProvider.getQuickFixes()) {
            if (fix instanceof IntentionAction intentionAction) {
               builder = builder.withFix(intentionAction);
            }
          }
        }
        builder.create();
      }
    }
  }

  private boolean hasBadResolve(final PsiReference ref) {
    if (ref instanceof PsiPolyVariantReference psiPolyVariantReference) {
      return psiPolyVariantReference.multiResolve(false).length == 0;
    }
    return ref.resolve() == null;
  }

  @Override
  public void visitJSCallExpression(final JSCallExpression node) {
    final JSExpression methodExpression = node.getMethodExpression();

    if (methodExpression instanceof JSLiteralExpression) {
      myHolder.createErrorAnnotation(methodExpression, JavaScriptLocalize.javascriptParserMessageExpectedFunctionName().get());
    }
  }

  @Override
  public void visitJSDocTagValue(final JSDocTagValue tagValue) {
    checkReferences(tagValue, HighlightSeverity.WARNING);
  }

  @Override
  public void visitJSDocTag(final JSDocTag tagValue) {
    checkReferences(tagValue, HighlightSeverity.WARNING);
  }

  @Override
  public void visitJSReferenceList(final JSReferenceList referenceList) {
    final JSClass jsClass = (JSClass)referenceList.getParent();
    if (JSResolveUtil.isArtificialClassUsedForReferenceList(jsClass)) {
      return; // implements="MyInterface" in mxml has artificial class created
    }

    final boolean withinExtends = jsClass.getExtendsList() == referenceList;
    final boolean withinImplements = jsClass.getImplementsList() == referenceList;

    if (withinImplements && jsClass.isInterface()) {
      myHolder.createErrorAnnotation(referenceList,
        JavaScriptLocalize.javascriptValidationMessageImplementsForInterfaceNotAllowed().get()
      );
      return;
    }

    final Map<String, JSReferenceExpression> nameToExprMap = new HashMap<String, JSReferenceExpression>();

    final JSReferenceExpression[] referenceExpressions = referenceList.getExpressions();
    if (referenceExpressions != null) {
      for (JSReferenceExpression expr : referenceExpressions) {
        final String s = expr.getReferencedName();
        if (s != null) {
          nameToExprMap.put(s, expr);
        }
      }
    }

    for (JSClass clazz : referenceList.getReferencedClasses()) {
      final boolean b = clazz.isInterface();
      final JSReferenceExpression expr = nameToExprMap.get(clazz.getName());

      if (!b && withinImplements) {
        myHolder.createErrorAnnotation(expr, JavaScriptLocalize.javascriptValidationMessageInterfaceNameExpectedHere().get());
      }
      else if (withinExtends && b != jsClass.isInterface()) {
        myHolder.createErrorAnnotation(
          expr,
          !b
            ? JavaScriptLocalize.javascriptValidationMessageInterfaceNameExpectedHere().get()
            : JavaScriptLocalize.javascriptValidationMessageClassNameExpectedHere().get()
        );
      }
      if (clazz == jsClass) {
        myHolder.createErrorAnnotation(expr, JavaScriptLocalize.javascriptValidationMessageCircularDependency().get())
          .registerFix(new RemoveASTNodeFix(referenceList.getNode(), JavaScriptLocalize.javascriptFixRemoveCircularDependency()));
      }
    }

    if (withinImplements) {
      checkImplementedMethods(jsClass, new SimpleErrorReportingClient());
    }
  }

  public interface ErrorReportingClient {
    enum ProblemKind {
      ERROR,
      WARNING
    }

    void reportError(final ASTNode nameIdentifier, final String s, ProblemKind kind, final IntentionAction implementMethodsFix);

    void reportError(final PsiElement nameIdentifier, final String s, ProblemKind kind, final IntentionAction implementMethodsFix);
  }

  public static void checkImplementedMethods(final JSClass jsClass, final ErrorReportingClient reportingClient) {
    final JSResolveUtil.CollectMethodsToImplementProcessor implementedMethodProcessor = new ImplementedMethodProcessor(jsClass) {
      ImplementMethodsFix implementMethodsFix = null;

      @Override
      protected void addNonimplementedFunction(final JSFunction function) {
        final PsiElement node = myJsClass.getNameIdentifier();
        if (node == null) {
          return;
        }
        if (implementMethodsFix == null) {
          implementMethodsFix = new ImplementMethodsFix(myJsClass);
        }
        implementMethodsFix.addElementToProcess(function);
        reportingClient.reportError(
          node,
          JavaScriptLocalize.javascriptValidationMessageInterfaceMethodNotImplemented(
            function.getName(),
            ((JSClass)JSResolveUtil.findParent(function)).getQualifiedName()
          ).get(),
          ErrorReportingClient.ProblemKind.ERROR,
          implementMethodsFix
        );
      }

      @Override
      protected void addImplementedFunction(final JSFunction interfaceFunction, final JSFunction implementationFunction) {
        final JSAttributeList attributeList = implementationFunction.getAttributeList();
        if (attributeList == null || attributeList.getAccessType() != JSAttributeList.AccessType.PUBLIC) {
          final ASTNode node = findElementForAccessModifierError(implementationFunction, attributeList);
          reportingClient.reportError(
            node,
            JavaScriptLocalize.javascriptValidationMessageInterfaceMethodInvalidAccessModifier().get(),
            ErrorReportingClient.ProblemKind.ERROR,
            null
            // TODO: quickfix
          );
        }

        final SignatureMatchResult incompatibleSignature = checkCompatibleSignature(implementationFunction, interfaceFunction);

        if (incompatibleSignature != SignatureMatchResult.COMPATIBLE_SIGNATURE) {
          PsiElement parent = JSResolveUtil.findParent(implementationFunction);
          if (parent instanceof JSFile) {
            parent = JSResolveUtil.getClassReferenceForXmlFromContext(parent);
          }

          if (parent != myJsClass) {
            // some parent incorrectly implements method from our interface
            addNonimplementedFunction(interfaceFunction);
            return;
          }

          if (incompatibleSignature == SignatureMatchResult.PARAMETERS_DIFFERS) {
            final JSParameterList parameterList = implementationFunction.getParameterList();
            final JSParameterList expectedParameterList = interfaceFunction.getParameterList();
            reportingClient.reportError(
              parameterList.getNode(),
              JavaScriptLocalize.javascriptValidationMessageInterfaceMethodInvalidSignature(
                expectedParameterList != null ? expectedParameterList.getText() : "()"
              ).get(),
              ErrorReportingClient.ProblemKind.ERROR,
              null
            );  // TODO: quickfix
          }
          else if (incompatibleSignature == SignatureMatchResult.RETURN_TYPE_DIFFERS) {
            PsiElement implementationReturnTypeExpr = implementationFunction.getReturnTypeElement();
            PsiElement interfaceReturnTypeExpr = interfaceFunction.getReturnTypeElement();
            reportingClient.reportError(
              implementationReturnTypeExpr != null ? implementationReturnTypeExpr : implementationFunction.getNameIdentifier(),
              JavaScriptLocalize.javascriptValidationMessageInterfaceMethodInvalidSignature2(
                interfaceReturnTypeExpr != null ? interfaceReturnTypeExpr.getText() : "*"
              ).get(),
              ErrorReportingClient.ProblemKind.ERROR,
              null
            );  // TODO: quickfix
          }
        }
      }
    };
    JSResolveUtil.processInterfaceMethods(jsClass, implementedMethodProcessor);
  }

  private static ASTNode findElementForAccessModifierError(final @Nonnull JSFunction o, final JSAttributeList attributeList) {
    if (attributeList != null) {
      final PsiElement accessTypeElement = attributeList.findAccessTypeElement();
      if (accessTypeElement != null) {
        return accessTypeElement.getNode();
      }
    }
    PsiElement nameIdentifier = o.getNameIdentifier();
    return nameIdentifier == null ? null : nameIdentifier.getNode();
  }

  @Override
  public void visitJSAttributeList(JSAttributeList attributeList) {
    PsiElement parentForCheckingNsOrAccessModifier = null;

    PsiElement namespaceElement = attributeList.getNamespaceElement();
    PsiElement accessTypeElement = attributeList.findAccessTypeElement();
    PsiElement namespaceOrAccessModifierElement = namespaceElement;

    if (namespaceOrAccessModifierElement == null) {
      namespaceOrAccessModifierElement = accessTypeElement;
    }
    else if (accessTypeElement != null) {
      myHolder.createErrorAnnotation(
          namespaceOrAccessModifierElement,
          JavaScriptLocalize.javascriptValidationMessageUseNamespaceReferenceOrAccessModifier().get()
        )
        .registerFix(new RemoveASTNodeFix(
          namespaceOrAccessModifierElement.getNode(),
          JavaScriptLocalize.javascriptFixRemoveNamespaceReference()
        ));

      myHolder.createErrorAnnotation(
          accessTypeElement,
          JavaScriptLocalize.javascriptValidationMessageUseNamespaceReferenceOrAccessModifier().get()
        )
        .registerFix(new RemoveASTNodeFix(accessTypeElement.getNode(), JavaScriptLocalize.javascriptFixRemoveNamespaceReference()));
    }

    if (namespaceOrAccessModifierElement != null) {
      parentForCheckingNsOrAccessModifier = JSResolveUtil.findParent(attributeList.getParent());
      if (!(parentForCheckingNsOrAccessModifier instanceof JSClass)) {
        String typeElementText;
        boolean nodeUnderPackage;

        if (((!(nodeUnderPackage = (parentForCheckingNsOrAccessModifier instanceof JSPackageStatement))
          && (!(parentForCheckingNsOrAccessModifier instanceof JSFile)
          || attributeList.getAccessType() != JSAttributeList.AccessType.PACKAGE_LOCAL))
          || (!"public".equals(typeElementText = namespaceOrAccessModifierElement.getText()))
          && !"internal".equals(typeElementText))) {
          boolean nsRef = namespaceOrAccessModifierElement instanceof JSReferenceExpression;
          myHolder.createErrorAnnotation(
              namespaceOrAccessModifierElement,
              nodeUnderPackage ?
                JavaScriptLocalize.javascriptValidationMessageAccessModifierAllowedOnlyForPackageMembers().get()
                : nsRef
                ? JavaScriptLocalize.javascriptValidationMessageNamespaceAllowedOnlyForClassMembers().get()
                : JavaScriptLocalize.javascriptValidationMessageAccessModifierAllowedOnlyForClassMembers().get()
            )
            .registerFix(new RemoveASTNodeFix(
              namespaceOrAccessModifierElement.getNode(),
              nsRef ? JavaScriptLocalize.javascriptFixRemoveNamespaceReference() : JavaScriptLocalize.javascriptFixRemoveAccessModifier()
            ));
        }
      }
      else if (((JSClass)parentForCheckingNsOrAccessModifier).isInterface()) {
        if (attributeList.getAccessType() != JSAttributeList.AccessType.PACKAGE_LOCAL
          || attributeList.getNode().findChildByType(JSTokenTypes.INTERNAL_KEYWORD) != null
        ) {
          final ASTNode astNode = attributeList.getNode().findChildByType(JSTokenTypes.ACCESS_MODIFIERS);
          final Annotation annotation = myHolder.createErrorAnnotation(
            astNode,
            JavaScriptLocalize.javascriptValidationMessageInterfaceMembersCannotHaveAccessModifiers().get()
          );

          annotation.registerFix(new RemoveASTNodeFix(astNode, JavaScriptLocalize.javascriptFixRemoveAccessModifier()));
        }
      }
    }
  }

  @Override
  public void visitJSReferenceExpression(final JSReferenceExpression node) {
    final PsiElement parent = node.getParent();

    if (parent instanceof JSNamedElement namedElement) {
      final PsiElement nameIdentifier = namedElement.getNameIdentifier();

      if (nameIdentifier != null && nameIdentifier == node) {
        if (parent instanceof JSPackageStatement packageStatement) {
          checkPackageStatement(packageStatement);
        }
        else if (!(parent instanceof JSImportStatement) && parent.getParent() instanceof JSPackageStatement) {
          checkNamedObjectIsInCorrespondingFile(namedElement);
        }
        else if (parent instanceof JSFunction function) {
          if (function.isConstructor()) {
            final JSClass clazz;
            if (parent.getParent() instanceof JSClass jsClass) {
              clazz = jsClass;
            }
            else {
              assert parent.getParent() instanceof JSFile;
              clazz = JSResolveUtil.getXmlBackedClass((JSFile)parent.getParent());
              assert clazz != null;
            }

            checkMissedSuperCall(node, function, clazz);
          }
          else if (function.isSetProperty()) {
            String typeString = function.getReturnTypeString();

            if (typeString != null && !"void".equals(typeString)) {
              // TODO: fix!
              myHolder.createErrorAnnotation(
                function.getReturnTypeElement(),
                JavaScriptLocalize.javascriptValidationMessageSetMethodShouldBeVoidOrWithoutType().get()
              );
            }

            JSParameterList parameterList = function.getParameterList();
            if (parameterList != null && parameterList.getParameters().length != 1) {
              // TODO: fix!
              myHolder.createErrorAnnotation(
                parameterList,
                JavaScriptLocalize.javascriptValidationMessageSetMethodShouldHaveOneParameter().get()
              );
            }
          }
          else if (function.isGetProperty()) {
            String typeString = function.getReturnTypeString();

            if (typeString == null || "void".equals(typeString)) {
              // TODO: fix!
              myHolder.createErrorAnnotation(
                typeString != null ? function.getReturnTypeElement() : nameIdentifier,
                JavaScriptLocalize.javascriptValidationMessageGetMethodShouldBeValidType(typeString != null ? typeString : "empty").get()
              );
            }

            JSParameterList parameterList = function.getParameterList();
            if (parameterList != null && parameterList.getParameters().length != 0) {
              // TODO: fix!
              myHolder.createErrorAnnotation(
                parameterList,
                JavaScriptLocalize.javascriptValidationMessageGetMethodShouldHaveNoParameter().get()
              );
            }
          }
        }

        if (parent instanceof JSClass jsClass) {
          final JSFunction constructor = jsClass.findFunctionByName(jsClass.getName());
          if (constructor == null) {
            checkMissedSuperCall(node, constructor, jsClass);
          }

          PsiElement clazzParent = jsClass.getParent();
          if (!(clazzParent instanceof JSPackageStatement || clazzParent instanceof JSFile)) {
            myHolder.createErrorAnnotation(node, JavaScriptLocalize.javascriptValidationMessageNestedClassesAreNotAllowed().get());
          }
        }
      }
    }

    if (node.getQualifier() == null && !(parent instanceof JSCallExpression) && "arguments".equals(node.getText())) {
      JSFunction fun = PsiTreeUtil.getParentOfType(node, JSFunction.class);
      if (fun == null) {
        myHolder.createErrorAnnotation(node, JavaScriptLocalize.javascriptValidationMessageArgumentsOutOfFunction().get());
      }
      else {
        JSParameterList parameterList = fun.getParameterList();
        if (parameterList != null) {
          for (JSParameter p : parameterList.getParameters()) {
            if (p.isRest()) {
              myHolder.createErrorAnnotation(
                node,
                JavaScriptLocalize.javascriptValidationMessageArgumentsWithRestParameter().get()
              );
            }
          }
        }
      }
    }
  }

  private void checkMissedSuperCall(JSReferenceExpression node, JSFunction constructor, JSClass jsClass) {
    if (jsClass.isInterface()) {
      return;
    }
    JSFunction nontrivialSuperClassConstructor = getNontrivialSuperClassConstructor(jsClass);

    if (nontrivialSuperClassConstructor != null) {
      Annotation annotation = null;

      if (!hasSuperConstructorCall(constructor)) {
        annotation =
          myHolder.createErrorAnnotation(node, JavaScriptLocalize.javascriptValidationMessageMissedSuperConstructorCall().get());
      }

      if (annotation != null) {
        if (constructor == null) {
          annotation.registerFix(new AddConstructorAndSuperInvokationFix(node, nontrivialSuperClassConstructor));
        }
        else {
          annotation.registerFix(new AddSuperInvokationFix(node, nontrivialSuperClassConstructor));
        }
      }
    }
  }

  private boolean hasSuperConstructorCall(JSFunction jsFunction) {
    if (jsFunction == null) {
      return false;
    }
    final JSSourceElement[] body = (jsFunction).getBody();
    final JSStatement[] statements = body.length > 0 ? ((JSBlockStatement)body[0]).getStatements() : JSStatement.EMPTY;

    for (JSStatement st : statements) {
      if (st instanceof JSExpressionStatement expressionStatement
        && expressionStatement.getExpression() instanceof JSCallExpression callExpression
        && callExpression.getMethodExpression() instanceof JSSuperExpression) {
        return true;
      }
    }

    return false;
  }

  public static JSFunction getNontrivialSuperClassConstructor(JSClass clazz) {
    final JSClass[] classes = clazz.getSuperClasses();

    if (classes.length > 0) {
      final JSFunction constructor = classes[0].findFunctionByName(classes[0].getName());

      if (constructor != null) {
        final JSParameter[] jsParameters = constructor.getParameterList().getParameters();
        boolean hasRequiredParameters = false;
        for (JSParameter p : jsParameters) {
          if (!p.isRest() && !p.hasInitializer()) {
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
  public void visitJSParameterList(JSParameterList node) {
    boolean foundRest = false;
    boolean initializerPresent = false;

    for (JSParameter parameter : node.getParameters()) {
      JSExpression initializer = parameter.getInitializer();
      boolean hasInitializer = initializer != null;

      if (hasInitializer && !initializerPresent) {
        initializerPresent = true;
      }
      else if (!hasInitializer && initializerPresent && !parameter.isRest()) {
        myHolder.createErrorAnnotation(parameter, JavaScriptLocalize.javascriptValidationMessageParameterShouldBeInitialized().get())
          .registerFix(new RemoveASTNodeFix(parameter.getNode(), JavaScriptLocalize.javascriptFixRemoveParameter()));
      }
      else if (hasInitializer && parameter.isRest()) {
        myHolder.createErrorAnnotation(
          parameter,
            JavaScriptLocalize.javascriptValidationMessageRestParameterShouldNotBeInitialized().get()
          )
          .registerFix(new RemoveASTNodeFix(
            JavaScriptLocalize.javascriptFixRemoveInitializer(),
            getNodesBefore(initializer, JSTokenTypes.EQ)
          ));
      }

      if (parameter.isRest() && !foundRest) {
        foundRest = true;
        PsiElement typeElement = parameter.getTypeElement();
        if (typeElement != null && !"Array".equals(typeElement.getText())) {
          myHolder.createErrorAnnotation(
              typeElement,
              JavaScriptLocalize.javascriptValidationMessageUnexpectedTypeForRestParameter().get()
            )
            .registerFix(new RemoveASTNodeFix(
              JavaScriptLocalize.javascriptFixRemoveTypeReference(),
              getNodesBefore(typeElement, JSTokenTypes.COLON)
            ));
        }
      }
      else if (foundRest) {
        myHolder.createErrorAnnotation(
            parameter,
            JavaScriptLocalize.javascriptValidationMessageParameterIsNotAllowedAfterRestParameter().get()
          )
          .registerFix(new RemoveASTNodeFix(parameter.getNode(), JavaScriptLocalize.javascriptFixRemoveParameter()));
      }
    }
  }

  private static ASTNode[] getNodesBefore(PsiElement initializer, IElementType eq) {
    List<ASTNode> nodes = new ArrayList<>();
    PsiElement element = initializer.getPrevSibling();
    PsiElement lastElement = element;

    if (element instanceof PsiWhiteSpace) {
      nodes.add(element.getNode());
      lastElement = element.getPrevSibling();
    }

    if (lastElement != null && lastElement.getNode().getElementType() == eq) {
      nodes.add(lastElement.getNode());
    }

    nodes.add(initializer.getNode());
    return nodes.toArray(new ASTNode[nodes.size()]);
  }

  @Override
  public void visitJSPackageStatement(final JSPackageStatement packageStatement) {
    for (PsiElement el = packageStatement.getPrevSibling(); el != null; el = el.getPrevSibling()) {
      if (!(el instanceof PsiWhiteSpace || el instanceof PsiComment)) {
        myHolder.createErrorAnnotation(
          packageStatement.getFirstChild().getNode(),
          JavaScriptLocalize.javascriptValidationMessagePackageShouldbeFirstStatement().get()
        );
        break;
      }
    }
    final PsiElement node = packageStatement.getNameIdentifier();
    if (node == null) {
      checkPackageStatement(packageStatement);
    }
  }

  @Override
  public void visitJSAssignmentExpression(final JSAssignmentExpression expression) {
    JSExpression lExpr = expression.getLOperand();
    if (lExpr == null) {
      return;
    }
    if (lExpr instanceof JSDefinitionExpression definitionExpression) {
      lExpr = definitionExpression.getExpression();
    }

    if (lExpr instanceof JSReferenceExpression lRefExpr) {
      PsiElement resolved = lRefExpr.resolve();
      if (resolved instanceof JSVariable variable && variable.isConst()) {
        myHolder.createErrorAnnotation(lExpr, JavaScriptLocalize.javascriptValidationMessageAssignmentToConst().get());
      }
    }

    if (!JSUtils.isLHSExpression(lExpr)) {
      myHolder.createErrorAnnotation(lExpr, JavaScriptLocalize.javascriptValidationMessageMustBeLvalue().get());
    }
  }

  @Override
  public void visitJSArrayLiteralExpression(final JSArrayLiteralExpression node) {
    final PsiElement lastChild = node.getLastChild();
    PsiElement child = lastChild != null ? lastChild.getPrevSibling() : null;
    if (child instanceof PsiWhiteSpace) {
      child = child.getPrevSibling();
    }
    ASTNode childNode;

    if (child != null && (childNode = child.getNode()) != null && childNode.getElementType() == JSTokenTypes.COMMA) {
      final Annotation annotation =
        myHolder.createWarningAnnotation(child, JavaScriptLocalize.javascriptValidationMessageUnneededComma().get());
      annotation.registerFix(new RemoveASTNodeFix(childNode, JavaScriptLocalize.javascriptValidationMessageRemoveUnneededCommaFix()));
    }
  }

  @Override
  public void visitJSTryStatement(final JSTryStatement node) {
    final JSCatchBlock[] blocks = node.getAllCatchBlocks();

    if (blocks.length > 1) {
      final Set<String> typeToCatch = new HashSet<>();

      for (JSCatchBlock block : blocks) {
        final JSParameter p = block.getParameter();
        if (p == null) {
          continue;
        }

        String s = p.getTypeString();
        if (s == null) {
          s = "";
        }

        if (typeToCatch.contains(s)) {
          final Annotation annotation =
            myHolder.createErrorAnnotation(block, JavaScriptLocalize.javascriptValidationMessageDuplicatedCatchBlock().get());
          annotation.registerFix(new RemoveASTNodeFix(
            block.getNode(),
            JavaScriptLocalize.javascriptValidationMessageDuplicatedCatchBlockFix()
          ));
        }
        else {
          typeToCatch.add(s);
        }
      }
    }
  }

  @Override
  public void visitJSVariable(final JSVariable var) {
    if (var.isConst() && var.getInitializer() == null) {
      if (var.getParent() instanceof JSVarStatement varStatement && varStatement.getParent() instanceof JSForInStatement) {
        return;
      }

      JSAttributeList attributeList = var.getAttributeList();
      if (attributeList == null || attributeList.getAttributesByName("Embed").length == 0) {
        myHolder.createWarningAnnotation(
          var,
          JavaScriptLocalize.javascriptValidationMessageConstVariableWithoutInitializer().get()
        );
      }
    }

    if (var.getParent().getParent() instanceof JSPackageStatement) {
      checkNamedObjectIsInCorrespondingFile(var);
    }
  }

  @Override
  public void visitJSContinueStatement(final JSContinueStatement node) {
    if (node.getStatementToContinue() == null) {
      myHolder.createErrorAnnotation(node, JavaScriptLocalize.javascriptValidationMessageContinueWithoutTarget().get());
    }
  }

  @Override
  public void visitJSBreakStatement(final JSBreakStatement node) {
    if (node.getStatementToBreak() == null) {
      myHolder.createErrorAnnotation(node, JavaScriptLocalize.javascriptValidationMessageBreakWithoutTarget().get());
    }
  }

  @Override
  public void visitJSThisExpression(final JSThisExpression node) {
    checkClassReferenceInStaticContext(node, JavaScriptLocalize.javascriptValidationMessageThisReferencedFromStaticContext());
  }

  private void checkClassReferenceInStaticContext(
    final JSExpression node,
    LocalizeValue message
  ) {
    PsiElement element = PsiTreeUtil.getParentOfType(
      node,
      JSFunction.class,
      JSFile.class,
      JSClass.class,
      JSObjectLiteralExpression.class,
      XmlTagChild.class
    );

    if (element instanceof JSFunction function) {
      final JSAttributeList attributeList = function.getAttributeList();
      if (attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)) {
        myHolder.createErrorAnnotation(node, message.get());
        return;
      }
    }

    if (!(node instanceof JSSuperExpression)) {
      return;
    }

    PsiElement elementParent = element != null ? element.getParent() : null;
    if (element == null || !(elementParent instanceof JSClass || elementParent instanceof JSFile && elementParent.getContext() != null)) {
      myHolder.createErrorAnnotation(node, JavaScriptLocalize.javascriptValidationMessageSuperReferencedWithoutClassInstanceContext().get());
    }
  }

  @Override
  public void visitJSSuperExpression(final JSSuperExpression node) {
    checkClassReferenceInStaticContext(node, JavaScriptLocalize.javascriptValidationMessageSuperReferencedFromStaticContext());
  }

  @Override
  public void visitJSFunctionDeclaration(final JSFunction node) {
    final PsiElement nameIdentifier = node.getNameIdentifier();
    if (nameIdentifier == null) {
      return;
    }
    PsiElement parent = node.getParent();

    if (parent instanceof JSFile file) {
      parent = JSResolveUtil.getClassReferenceForXmlFromContext(file);

      if (parent instanceof JSClass jsClass && node.getName().equals(jsClass.getName())
        && JavaScriptSupportLoader.isFlexMxmFile(parent.getContainingFile())) {
        final Annotation annotation = myHolder.createErrorAnnotation(
          nameIdentifier,
          JavaScriptLocalize.javascriptValidationMessageConstructorInMxmlIsNotAllowed().get()
        );

        annotation.registerFix(new RemoveASTNodeFix(node.getNode(), JavaScriptLocalize.javascriptFixRemoveConstructor()));
      }
    }

    if (parent instanceof JSPackageStatement) {
      checkNamedObjectIsInCorrespondingFile(node);
    }

    if (parent instanceof JSClass clazz && !node.isConstructor()) {
      final JSAttributeList attributeList = node.getAttributeList();

      if (attributeList == null || (!attributeList.hasModifier(JSAttributeList.ModifierType.STATIC) && (attributeList.getAccessType() != JSAttributeList
        .AccessType.PRIVATE || attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE)))) {
        final String qName = clazz.getQualifiedName();
        final boolean hasOverride = attributeList != null ? attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE) : false;

        final Ref<JSFunction> set = new Ref<>();
        boolean b = JSResolveUtil.iterateType(node, parent, qName, (processor, scope, className) -> {
            if (qName == className || (qName != null && qName.equals(className))) {
              return true;
            }
            set.set((JSFunction)processor.getResult());
            if ("Object".equals(className)) {
              if (hasOverride && !attributeList.hasModifier(JSAttributeList.ModifierType.NATIVE)) { /*native modifier is written always*/
                final ASTNode astNode = attributeList.getNode().findChildByType(JSTokenTypes.OVERRIDE_KEYWORD);
                final Annotation annotation = myHolder.createErrorAnnotation(
                  astNode,
                  JavaScriptLocalize.javascriptValidationMessageFunctionOverrideForObjectMethod().get()
                );

                annotation.registerFix(new RemoveASTNodeFix(astNode, JavaScriptLocalize.javascriptFixRemoveOverrideModifier()));
              }
              return false;
            }
            else if (!hasOverride) {
              final Annotation annotation = myHolder.createErrorAnnotation(
                nameIdentifier,
                JavaScriptLocalize.javascriptValidationMessageFunctionOverrideWithoutOverrideModifier(className).get()
              );

              annotation.registerFix(new AddOverrideIntentionAction(node));
            }
            return false;
          });

        if (b && hasOverride) {
          final ASTNode astNode = attributeList.getNode().findChildByType(JSTokenTypes.OVERRIDE_KEYWORD);
          final Annotation annotation = myHolder.createErrorAnnotation(
            astNode,
            JavaScriptLocalize.javascriptValidationMessageFunctionOverrideWithoutParentMethod().get()
          );

          annotation.registerFix(new RemoveASTNodeFix(astNode, JavaScriptLocalize.javascriptFixRemoveOverrideModifier()));
        }

        if (!b && hasOverride) {
          final JSFunction override = set.get();
          final JSAttributeList overrideAttrList = override.getAttributeList();
          String overrideNs = null;

          if ((overrideAttrList == null && (attributeList.getAccessType() != JSAttributeList.AccessType.PACKAGE_LOCAL)) ||
            (overrideAttrList != null && attributeList.getAccessType() != overrideAttrList.getAccessType()) ||
            overrideAttrList != null && (overrideNs =
              overrideAttrList.getNamespace()) != null && !overrideNs.equals(attributeList.getNamespace())) {
            final Annotation annotation1 = myHolder.createErrorAnnotation(
              findElementForAccessModifierError(node, attributeList),
              JavaScriptLocalize.javascriptValidationMessageFunctionOverrideIncompatibleAccessModifier(
                overrideNs != null ? overrideNs : (
                  overrideAttrList != null
                    ? overrideAttrList.getAccessType().toString()
                    : JSAttributeList.AccessType.PACKAGE_LOCAL.toString()
                ).toLowerCase()
              ).get()
            );

            // TODO: quickfix
            //annotation.registerFix(
            //    new RemoveASTNodeFix(astNode,"javascript.fix.remove.override.modifier")
            //);
          }

          final SignatureMatchResult incompatibleSignature = checkCompatibleSignature(node, override);

          if (incompatibleSignature == SignatureMatchResult.PARAMETERS_DIFFERS) {
            final JSParameterList nodeParameterList = node.getParameterList();
            final JSParameterList overrideParameterList = override.getParameterList();

            final Annotation annotation = myHolder.createErrorAnnotation(
              nodeParameterList != null ? nodeParameterList : node.getNameIdentifier(),
              JavaScriptLocalize.javascriptValidationMessageFunctionOverrideIncompatibleSignature(
                overrideParameterList != null ? overrideParameterList.getText() : "()"
              ).get()
            );

            // TODO: quickfix
            //annotation.registerFix(
            //    new RemoveASTNodeFix(astNode,"javascript.fix.remove.override.modifier")
            //);
          }
          else if (incompatibleSignature == SignatureMatchResult.RETURN_TYPE_DIFFERS) {
            PsiElement returnTypeExpr = node.getReturnTypeElement();
            PsiElement overrideReturnTypeExpr = override.getReturnTypeElement();
            final Annotation annotation = myHolder.createErrorAnnotation(
              returnTypeExpr != null ? returnTypeExpr : node.getNameIdentifier(),
              JavaScriptLocalize.javascriptValidationMessageFunctionOverrideIncompatibleSignature2(
                overrideReturnTypeExpr != null ? overrideReturnTypeExpr.getText() : "*"
              ).get()
            );

            // TODO: quickfix
            //annotation.registerFix(
            //    new RemoveASTNodeFix(astNode,"javascript.fix.remove.override.modifier")
            //);
          }
        }
      }
    }
  }

  enum SignatureMatchResult {
    PARAMETERS_DIFFERS,
    RETURN_TYPE_DIFFERS,
    COMPATIBLE_SIGNATURE
  }

  private static SignatureMatchResult checkCompatibleSignature(final JSFunction fun, final JSFunction override) {
    JSParameterList nodeParameterList = fun.getParameterList();
    JSParameterList overrideParameterList = override.getParameterList();
    final JSParameter[] parameters = nodeParameterList != null ? nodeParameterList.getParameters() : JSParameter.EMPTY_ARRAY;
    final JSParameter[] overrideParameters =
      overrideParameterList != null ? overrideParameterList.getParameters() : JSParameter.EMPTY_ARRAY;

    SignatureMatchResult result =
      parameters.length != overrideParameters.length ? SignatureMatchResult.PARAMETERS_DIFFERS : SignatureMatchResult
        .COMPATIBLE_SIGNATURE;

    if (result == SignatureMatchResult.COMPATIBLE_SIGNATURE) {
      for (int i = 0; i < parameters.length; ++i) {
        if (!compatibleType(overrideParameters[i].getTypeString(), parameters[i].getTypeString(), overrideParameterList,
                            nodeParameterList) || overrideParameters[i].hasInitializer() != parameters[i].hasInitializer()) {
          result = SignatureMatchResult.PARAMETERS_DIFFERS;
          break;
        }
      }
    }

    if (result == SignatureMatchResult.COMPATIBLE_SIGNATURE) {
      if (!compatibleType(override.getReturnTypeString(), fun.getReturnTypeString(), override, fun)) {
        result = SignatureMatchResult.RETURN_TYPE_DIFFERS;
      }
    }
    return result;
  }

  private static boolean compatibleType(String overrideParameterType,
                                        String parameterType,
                                        PsiElement overrideContext,
                                        PsiElement funContext) {
    // TODO: This should be more accurate
    if (overrideParameterType != null && !overrideParameterType.equals(parameterType)) {
      parameterType = JSImportHandlingUtil.resolveTypeName(parameterType, funContext);
      overrideParameterType = JSImportHandlingUtil.resolveTypeName(overrideParameterType, overrideContext);

      return overrideParameterType.equals(parameterType);
    }
    else if (overrideParameterType == null && parameterType != null && !"*".equals(parameterType)) {
      return false;
    }

    return true;
  }

  @Override
  public void visitJSReturnStatement(final JSReturnStatement node) {
    final PsiElement element = PsiTreeUtil.getParentOfType(
      node,
      JSFunction.class,
      XmlTagChild.class,
      XmlAttributeValue.class,
      JSFile.class,
      JavaScriptLambdaExpression.class
    );
    if ((element instanceof JSFile && !(element.getContext() instanceof PsiLanguageInjectionHost))
      || (element instanceof XmlTagChild && !(element.getParent() instanceof XmlAttributeValue))) {
      myHolder.createErrorAnnotation(node, JavaScriptLocalize.javascriptValidationMessageReturnOutsideFunctionDefinition().get());
    }

    if (element instanceof JSFunction function) {
      final @NonNls String typeString = function.getReturnTypeString();
      if (typeString != null && !"void".equals(typeString) && node.getExpression() == null) {
        myHolder.createErrorAnnotation(node, JavaScriptLocalize.javascriptValidationMessageReturnValueOfTypeIsRequired(typeString).get());
      }
    }
  }

  @Override
  public void visitJSLabeledStatement(final JSLabeledStatement node) {
    final String label = node.getLabel();
    if (label != null) {
      PsiElement run = node.getParent();
      while (run != null) {
        if (run instanceof JSLabeledStatement labeledStatement) {
          if (label.equals(labeledStatement.getLabel())) {
            myHolder.createErrorAnnotation(node.getLabelIdentifier(), JavaScriptLocalize.javascriptValidationMessageDuplicateLabel().get());
            break;
          }
        }

        if (run instanceof JSFunction) {
          break;
        }
        run = run.getParent();
      }
    }
  }

  private void checkNamedObjectIsInCorrespondingFile(final JSNamedElement aClass) {
    final PsiFile containingFile = aClass.getContainingFile();

    if (containingFile.getContext() != null) {
      return;
    }
    final VirtualFile file = containingFile.getVirtualFile();

    if (file != null && !file.getNameWithoutExtension().equals(aClass.getName())) {
      final PsiElement node = aClass.getNameIdentifier();

      if (node != null) {
        final String name = aClass.getName();
        String nameWithExtension = name + "." + file.getExtension();
        final LocalizeValue message = aClass instanceof JSClass
          ? JavaScriptLocalize.javascriptValidationMessageClassShouldBeInFile(name, nameWithExtension)
          : aClass instanceof JSNamespaceDeclaration
          ? JavaScriptLocalize.javascriptValidationMessageNamespaceShouldBeInFile(name, nameWithExtension)
          : aClass instanceof JSVariable
          ? JavaScriptLocalize.javascriptValidationMessageVariableShouldBeInFile(name, nameWithExtension)
          : JavaScriptLocalize.javascriptValidationMessageFunctionShouldBeInFile(name, nameWithExtension);
        final Annotation annotation = myHolder.createErrorAnnotation(node, message.get());

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

  private String getTerm(String message) {
    String term = message.substring(0, message.indexOf(' '));
    return term;
  }

  public static void checkFileUnderSourceRoot(final JSNamedElement aClass, ErrorReportingClient client) {
    PsiElement nameIdentifier = aClass.getNameIdentifier();
    if (nameIdentifier == null) {
      nameIdentifier = aClass.getFirstChild();
    }

    final PsiFile containingFile = aClass.getContainingFile();
    final VirtualFile file = containingFile.getVirtualFile();
    if (file == null) {
      return;
    }
    final VirtualFile rootForFile = ProjectRootManager.getInstance(containingFile.getProject()).getFileIndex().getSourceRootForFile(file);

    if (rootForFile == null) {
      client.reportError(
        nameIdentifier.getNode(),
        JavaScriptLocalize.javascriptValidationMessageFileShouldBeUnderSourceRoot().get(),
        ErrorReportingClient.ProblemKind.WARNING,
        null
      );
    }
  }

  private void checkPackageStatement(final JSPackageStatement packageStatement) {
    final String s = packageStatement.getQualifiedName();

    final PsiFile containingFile = packageStatement.getContainingFile();
    final String expected = JSResolveUtil.getExpectedPackageNameFromFile(
      containingFile.getVirtualFile(),
      containingFile.getProject(),
      true
    );

    if (expected != null && ((s == null && expected.length() != 0) || (s != null && !expected.equals(s)))) {
      final PsiElement nameIdentifier = packageStatement.getNameIdentifier();
      final Annotation annotation = myHolder.createErrorAnnotation(
        nameIdentifier != null ? nameIdentifier : packageStatement.getFirstChild(),
        JavaScriptLocalize.javascriptValidationMessageIncorrectPackageName(s, expected).get()
      );
      annotation.registerFix(new SyntheticIntentionAction() {
        @Override
        @Nonnull
        public String getText() {
          return JavaScriptLocalize.javascriptFixPackageName(expected).get();
        }

        @Override
        public boolean isAvailable(@Nonnull final Project project, final Editor editor, final PsiFile file) {
          return packageStatement.isValid();
        }

        @Override
        public void invoke(@Nonnull final Project project, final Editor editor, final PsiFile file) throws IncorrectOperationException {
          JSPackageStatementImpl.doChangeName(project, packageStatement, expected);
        }

        @Override
        public boolean startInWriteAction() {
          return true;
        }
      });
    }

    final Set<JSNamedElement> elements = new HashSet<>();

    for (JSSourceElement statement : packageStatement.getStatements()) {
      if (statement instanceof JSNamedElement namedElement && !(statement instanceof JSImportStatement)) {
        elements.add(namedElement);
      }
    }

    if (elements.size() > 1) {
      for (JSNamedElement el : elements) {
        final PsiElement nameIdentifier = el.getNameIdentifier();
        myHolder.createErrorAnnotation(
            nameIdentifier != null ? nameIdentifier : el.getFirstChild(),
            JavaScriptLocalize.javascriptValidationMessageMoreThanOneExternallyVisibleSymbol().get()
          )
          .registerFix(new RemoveASTNodeFix(el.getNode(), JavaScriptLocalize.javascriptFixRemoveExternallyVisibleSymbol()));
      }
    }

    checkFileUnderSourceRoot(packageStatement, new SimpleErrorReportingClient());
  }

  public static class RemoveASTNodeFix implements SyntheticIntentionAction, LocalQuickFix {
    private final ASTNode[] myAstNodes;
    private final LocalizeValue myProp;

    public RemoveASTNodeFix(final ASTNode astNode, LocalizeValue prop) {
      this(prop, astNode);
    }

    public RemoveASTNodeFix(LocalizeValue prop, final ASTNode... astNodes) {
      myProp = prop;
      myAstNodes = astNodes;
    }

    @Override
    @Nonnull
    public String getText() {
      return myProp.get();
    }

    @Override
    @Nonnull
    public String getName() {
      return getText();
    }

    @Override
    @Nonnull
    public String getFamilyName() {
      return getText();
    }

    @Override
    public void applyFix(@Nonnull Project project, @Nonnull ProblemDescriptor descriptor) {
      invoke(project, null, descriptor.getPsiElement().getContainingFile());
    }

    @Override
    public boolean isAvailable(@Nonnull final Project project, final Editor editor, final PsiFile file) {
      for (ASTNode astNode : myAstNodes) {
        if (!astNode.getPsi().isValid()) {
          return false;
        }
      }

      return true;
    }

    @Override
    public void invoke(@Nonnull final Project project, final Editor editor, final PsiFile file) throws IncorrectOperationException {
      if (!FileModificationService.getInstance().prepareFileForWrite(file)) {
        return;
      }
      for (ASTNode astNode : myAstNodes) {
        if (astNode.getPsi().isValid()) {
          astNode.getPsi().delete();
        }
      }
    }

    @Override
    public boolean startInWriteAction() {
      return true;
    }
  }

  private static class AddOverrideIntentionAction implements SyntheticIntentionAction {
    private final JSFunction myNode;

    public AddOverrideIntentionAction(final JSFunction node) {
      myNode = node;
    }

    @Override
    @Nonnull
    public String getText() {
      return JavaScriptLocalize.javascriptFixAddOverrideModifier().get();
    }

    @Override
    public boolean isAvailable(@Nonnull final Project project, final Editor editor, final PsiFile file) {
      return myNode.isValid();
    }

    @Override
    public void invoke(@Nonnull final Project project, final Editor editor, final PsiFile file) throws IncorrectOperationException {
      if (!FileModificationService.getInstance().prepareFileForWrite(file)) {
        return;
      }
      final ASTNode fromText = JSChangeUtil.createJSTreeFromText(project, "override class A {}");
      final JSAttributeList jsAttributeList = myNode.getAttributeList();
      final JSAttributeList createdAttrList = ((JSClass)fromText.getPsi()).getAttributeList();

      if (jsAttributeList != null) {
        jsAttributeList.add(createdAttrList.getFirstChild());
      }
      else {
        myNode.addBefore(createdAttrList, myNode.getFirstChild());
      }
    }

    @Override
    public boolean startInWriteAction() {
      return true;
    }
  }

  private class SimpleErrorReportingClient implements ErrorReportingClient {
    @Override
    public void reportError(final ASTNode nameIdentifier, final String s, ProblemKind kind, final IntentionAction implementMethodsFix) {
      final Annotation annotation = kind == ProblemKind.ERROR
        ? myHolder.createErrorAnnotation(nameIdentifier, s)
        : myHolder.createWarningAnnotation(nameIdentifier, s);

      if (implementMethodsFix != null) {
        annotation.registerFix(implementMethodsFix);
      }
    }

    @Override
    public void reportError(PsiElement nameIdentifier, String s, ProblemKind kind, IntentionAction implementMethodsFix) {
      final Annotation annotation = kind == ProblemKind.ERROR
        ? myHolder.createErrorAnnotation(nameIdentifier, s)
        : myHolder.createWarningAnnotation(nameIdentifier, s);
      if (implementMethodsFix != null) {
        annotation.registerFix(implementMethodsFix);
      }
    }
  }

  private static class AddSuperInvokationFix implements SyntheticIntentionAction {
    private final JSReferenceExpression node;
    private final JSFunction superConstructor;

    public AddSuperInvokationFix(JSReferenceExpression node, JSFunction superConstructor) {
      this.node = node;
      this.superConstructor = superConstructor;
    }

    @Override
    @Nonnull
    public String getText() {
      return JavaScriptLocalize.javascriptFixCreateInvokeSuper().get();
    }

    @Override
    public boolean isAvailable(@Nonnull Project project, Editor editor, PsiFile file) {
      return superConstructor.isValid() && node.isValid();
    }

    @Override
    public void invoke(@Nonnull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
      if (!FileModificationService.getInstance().prepareFileForWrite(file)) {
        return;
      }
      Template t = TemplateManager.getInstance(project).createTemplate("", "");
      t.setToReformat(true);

      t.addTextSegment("super(");
      boolean first = true;
      for (JSParameter p : superConstructor.getParameterList().getParameters()) {
        if (p.isRest()) {
          break;
        }
        if (!first) {
          t.addTextSegment(", ");
        }
        first = false;
        MacroCallNode node = new MacroCallNode(new CompleteMacro());
        t.addVariable(p.getName(), node, node, true);
      }
      t.addTextSegment(")");
      String s = JSChangeUtil.getSemicolon(project);
      if (s.length() > 0) {
        t.addTextSegment(s);
      }

      JSFunction fun = PsiTreeUtil.getParentOfType(node, JSFunction.class);
      JSSourceElement[] body = fun != null ? fun.getBody() : JSSourceElement.EMPTY_ARRAY;

      if (body.length > 0 && body[0] instanceof JSBlockStatement blockStatement) {
        PsiElement firstChild = blockStatement.getFirstChild();
        editor.getCaretModel().moveToOffset(firstChild.getTextRange().getEndOffset());
        TemplateManager.getInstance(project).startTemplate(editor, t);
      }
    }

    @Override
    public boolean startInWriteAction() {
      return false;
    }
  }

  private static class AddConstructorAndSuperInvokationFix implements SyntheticIntentionAction {
    private final JSReferenceExpression node;
    private final JSFunction superConstructor;

    AddConstructorAndSuperInvokationFix(JSReferenceExpression _node, JSFunction _superCall) {
      node = _node;
      superConstructor = _superCall;
    }

    @Override
    @Nonnull
    public String getText() {
      return JavaScriptLocalize.javascriptFixCreateConstructorInvokeSuper().get();
    }

    @Override
    public boolean isAvailable(@Nonnull Project project, Editor editor, PsiFile file) {
      return node.isValid() && superConstructor.isValid();
    }

    @Override
    public void invoke(@Nonnull Project project, Editor editor, PsiFile file) throws IncorrectOperationException {
      if (!FileModificationService.getInstance().prepareFileForWrite(file)) {
        return;
      }
      final JSClass jsClass = PsiTreeUtil.getParentOfType(node, JSClass.class);
      if (jsClass == null) {
        return;
      }
      final JSAttributeList attributeList = jsClass.getAttributeList();
      String fun = "";

      if (attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PUBLIC) {
        fun += "public ";
      }

      fun += "function ";

      final JSParameterList parameterList = superConstructor.getParameterList();
      fun += jsClass.getName() + parameterList.getText() + "{\n";
      fun += "super(";
      int i = 0;

      for (JSParameter p : parameterList.getParameters()) {
        if (i != 0) {
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
    public boolean startInWriteAction() {
      return true;
    }
  }
}
