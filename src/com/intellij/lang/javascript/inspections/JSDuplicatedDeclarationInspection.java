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

import com.intellij.codeHighlighting.HighlightDisplayLevel;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JSElementType;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.ResolveState;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

/**
 * @author Maxim.Mossienko
 */
public class JSDuplicatedDeclarationInspection extends JSInspection {
  @NonNls private static final String SHORT_NAME = "JSDuplicatedDeclaration";

  @NotNull
  public String getGroupDisplayName() {
    return JSBundle.message("js.inspection.group.name");
  }

  @NotNull
  public String getDisplayName() {
    return JSBundle.message("js.duplicated.declaration.inspection.name");
  }

  @NotNull
  @NonNls
  public String getShortName() {
    return SHORT_NAME;
  }

  protected JSElementVisitor createVisitor(final ProblemsHolder holder) {
    return new JSElementVisitor() {
      @Override public void visitJSClass(final JSClass node) {
        final String name = node.getName();
        if (name == null) return;
        final ASTNode nameIdentifier = node.findNameIdentifier();

        checkForDuplicateDeclaration(name, node, nameIdentifier);
      }

      @Override public void visitJSFunctionDeclaration(final JSFunction node) {
        final String name = node.getName();
        if (name == null) return;
        final ASTNode nameIdentifier = node.findNameIdentifier();

        checkForDuplicateDeclaration(name, node, nameIdentifier);
      }

      private void checkForDuplicateDeclaration(final String name, final PsiElement decl, final ASTNode nameIdentifier) {
        PsiElement scope = PsiTreeUtil.getParentOfType(decl, JSFunction.class, JSFile.class, JSEmbeddedContentImpl.class, JSClass.class,
                                                             JSObjectLiteralExpression.class, JSPackageStatement.class, PsiFile.class);
        if (scope instanceof JSPackageStatement) return; // dedicated inspection
        final PsiElement originalScope = scope;
        if (scope instanceof JSFile && scope.getContext() != null) {
          scope = scope.getContext().getContainingFile();
        }

        final ResolveProcessor processor = new ResolveProcessor(name, scope) {
          public boolean execute(PsiElement element, ResolveState state) {
            if (element == decl) return true;
            //if (!decl.getClass().isInstance(element)) return true;
            if (decl instanceof JSParameter && decl.getParent() != element.getParent()) return false;

            if (element instanceof JSFunction && decl instanceof JSFunction) {
              final JSFunction declFunction = (JSFunction)decl;
              final JSFunction elementFunction = (JSFunction)element;
              if ( (declFunction.isGetProperty() && elementFunction.isSetProperty()) ||
                   (declFunction.isSetProperty() && elementFunction.isGetProperty())
                 ) {
                return true;
              }
            } else if (element instanceof JSProperty && decl instanceof JSProperty) {
              final JSProperty elementProperty = (JSProperty)element;
              final JSProperty declProperty = (JSProperty)decl;

              if ( (declProperty.isGetProperty() && elementProperty.isSetProperty()) ||
                   (declProperty.isSetProperty() && elementProperty.isGetProperty())
                 ) {
                return true;
              }
            }

            if (element instanceof JSFunction &&
                decl instanceof JSClass && element.getParent() == decl) {
              return true;
            }

            if (element instanceof JSAttributeListOwner && decl instanceof JSAttributeListOwner) {
              JSAttributeList attrList = ((JSAttributeListOwner)element).getAttributeList();
              JSAttributeList attrList2 = ((JSAttributeListOwner)decl).getAttributeList();

              if (attrList != null && attrList2 != null) {
                final String ns = attrList.getNamespace();
                final String ns2 = attrList2.getNamespace();

                if ((ns != null && !ns.equals(ns2)) ||
                    ns2 != null && !ns2.equals(ns) ||
                    (ns != null && ns2 != null)
                   ) {
                  return true;
                }
              } else if ((attrList != null && attrList.getNamespace() != null) ||
                         (attrList2 != null && attrList2.getNamespace() != null)
                        ) {
                return true;
              }

              final boolean notStatic2 = attrList2 == null || !attrList2.hasModifier(JSAttributeList.ModifierType.STATIC);
              final boolean notStatic = attrList == null || !attrList.hasModifier(JSAttributeList.ModifierType.STATIC);
              if ((notStatic2 && !notStatic) || (notStatic && !notStatic2)) {
                return true;
              }
            }
            return super.execute(element, state);
          }
        };

        PsiElement parent = JSResolveUtil.findParent(decl);
        if (parent instanceof JSClass) {
          processor.configureClassScope((JSClass)parent);
        }

        if (decl instanceof JSFunction || decl instanceof JSVariable) {
          JSAttributeList attrList = ((JSAttributeListOwner)decl).getAttributeList();
          processor.setProcessStatics(attrList != null && attrList.hasModifier(JSAttributeList.ModifierType.STATIC));
        }

        processor.setLocalResolve(true);
        JSResolveUtil.treeWalkUp(processor, decl, null, decl, scope);

        if (processor.getResult() != null && processor.getResult() != scope) {
          holder.registerProblem(
              nameIdentifier.getPsi(),
              JSBundle.message("javascript.validation.message.duplicate.declaration"),
              originalScope.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4? ProblemHighlightType.ERROR : ProblemHighlightType.GENERIC_ERROR_OR_WARNING
          );
        }
      }

      @Override public void visitJSProperty(final JSProperty node) {
        final String name = node.getName();
        if (name == null) return;
        checkForDuplicateDeclaration(name, node, node.findNameIdentifier());
      }

      @Override public void visitJSVariable(final JSVariable var) {
        final ASTNode nameIdentifier = var.findNameIdentifier();
        final ASTNode next = nameIdentifier != null ? nameIdentifier.getTreeNext():null;
        final String name = nameIdentifier != null ? nameIdentifier.getText():null;

        // Actully skip outer language elements
        if (name != null &&
            ( next == null ||
              next.getElementType() instanceof JSElementType ||
              next.getPsi() instanceof PsiWhiteSpace)
           ) {
          checkForDuplicateDeclaration(name, var, nameIdentifier);
        }
      }

    };
  }

  @NotNull
  public HighlightDisplayLevel getDefaultLevel() {
    return HighlightDisplayLevel.WARNING;
  }
}