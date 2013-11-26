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

import com.intellij.codeInsight.CodeInsightUtilBase;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.util.JSUtils;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import java.util.LinkedList;
import java.util.List;

/**
 * @author Maxim.Mossienko
 */
public class JSUndeclaredVariableInspection extends JSInspection {
  @NonNls public static final String SHORT_NAME = "JSUndeclaredVariable";

  @NotNull
  public String getGroupDisplayName() {
    return JSBundle.message("js.inspection.group.name");
  }

  @NotNull
  public String getDisplayName() {
    return JSBundle.message("js.undeclared.variable.inspection.name");
  }

  @NotNull
  @NonNls
  public String getShortName() {
    return SHORT_NAME;
  }

  protected JSElementVisitor createVisitor(final ProblemsHolder holder) {
    return new JSElementVisitor() {
      @Override public void visitJSReferenceExpression(final JSReferenceExpression node) {
        final PsiElement parentElement = node.getParent();

        if (!(parentElement instanceof JSCallExpression) && node.shouldCheckReferences()
            && node.getQualifier() == null && parentElement instanceof JSDefinitionExpression) {
          final JSSourceElement element = PsiTreeUtil.getParentOfType(node, JSWithStatement.class, JSFunction.class);

          if (!(element instanceof JSWithStatement)) {
            boolean varReferenceWithoutVar = true;
            final ResolveResult[] resolveResults = node.multiResolve(false);

            JSNamedElementProxy proxy;

            for(ResolveResult r:resolveResults) {
              final PsiElement resolveResult = r.getElement();
              if (resolveResult instanceof JSVariable ||
                  resolveResult instanceof JSFunction ||
                  ( resolveResult instanceof JSNamedElementProxy &&
                    ( (proxy = ((JSNamedElementProxy)resolveResult)).getType() == JSNamedElementProxy.NamedItemType.MemberVariable ||
                      proxy.getType() == JSNamedElementProxy.NamedItemType.MemberFunction &&
                      ( proxy.hasProperty(JSNamedElementProxy.Property.GetFunction) ||
                        proxy.hasProperty(JSNamedElementProxy.Property.SetFunction)
                      )
                    )
                  )
                 ) {
                varReferenceWithoutVar = false;
                break;
              }
            }

            if (varReferenceWithoutVar) {
              final PsiElement nameIdentifier = node.getReferenceNameElement();

              if (nameIdentifier != null) {
                final List<LocalQuickFix> fixes = new LinkedList<LocalQuickFix>();

                if (myOnTheFly) {
                  fixes.add( new DeclareJSVariableIntentionAction(node) );
                }

                holder.registerProblem(
                  nameIdentifier,
                  JSBundle.message("javascript.undeclared.variable.name.message",node.getReferencedName()),
                  ProblemHighlightType.GENERIC_ERROR_OR_WARNING, !fixes.isEmpty() ? fixes.toArray(new LocalQuickFix[fixes.size()]):null
                );
              }
            }
          }
        }
        super.visitJSReferenceExpression(node);
      }
    };
  }

  private static boolean isImplicitlyDeclared(final JSReferenceExpression node, final PsiElement parentElement) {
    if (parentElement instanceof JSForInStatement) {
      final JSExpression varExpression = ((JSForInStatement)parentElement).getVariableExpression();

      return PsiTreeUtil.findCommonParent(varExpression, node) == varExpression;
    } else if (parentElement instanceof JSForStatement) {
      final JSExpression varExpression = ((JSForStatement)parentElement).getInitialization();

      return PsiTreeUtil.findCommonParent(varExpression, node) == varExpression;
    }
    return false;
  }

  private static class DeclareJSVariableIntentionAction implements LocalQuickFix {
    private final JSReferenceExpression myReferenceExpression;
    @NonNls private static final String VAR_STATEMENT_START = "var ";
    private final PsiFile myFile;

    DeclareJSVariableIntentionAction(JSReferenceExpression expression) {
      myReferenceExpression = expression;
      myFile = expression.getContainingFile();
    }

    @NotNull
    public String getName() {
      return JSBundle.message(
        "javascript.declare.variable.intention.name",
        myReferenceExpression.getReferencedName()
      );
    }

    @NotNull
    public String getFamilyName() {
      return JSBundle.message("javascript.create.variable.intention.family");
    }

    public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
      if ( !CodeInsightUtilBase.prepareFileForWrite(myFile)) return;

      PsiElement anchor = JSUtils.findStatementAnchor(myReferenceExpression, myFile);
      boolean implicitlyDeclared = isImplicitlyDeclared(myReferenceExpression, anchor);

      if (implicitlyDeclared) {
        anchor = myReferenceExpression;
        final JSStatement statement = PsiTreeUtil.getParentOfType(anchor, JSForStatement.class,JSStatement.class);

        if (statement instanceof JSForStatement) {
          final JSExpression initialization = ((JSForStatement)statement).getInitialization();

          if (initialization instanceof JSBinaryExpression &&
              ((JSBinaryExpression)initialization).getOperationSign() == JSTokenTypes.COMMA) {
            anchor = ((JSAssignmentExpression)((JSBinaryExpression)initialization).getLOperand()).getLOperand();
          }
        }
      }

      if (anchor != null) {
        boolean anchorChanged = false;

        if (!implicitlyDeclared) {
          PsiElement parent = anchor.getParent();

          while (parent instanceof JSBlockStatement || parent instanceof JSIfStatement || parent instanceof JSLoopStatement) {
            PsiElement newAnchor = parent.getParent();

            if (newAnchor instanceof JSIfStatement || newAnchor instanceof JSWithStatement || newAnchor instanceof JSLoopStatement ||
                newAnchor instanceof JSTryStatement || newAnchor instanceof JSSwitchStatement) {
              anchor = newAnchor;
              parent = anchor.getParent();
              anchorChanged = true;
            }
            else if (newAnchor instanceof JSFile) {
              anchor = parent;
              anchorChanged = true;
              break;
            }
            else {
              break;
            }
          }
        }

        final TextRange textRange = anchor.getTextRange();
        final int startOffset = textRange.getStartOffset();
        @NonNls StringBuilder builder = new StringBuilder();
        builder.append(VAR_STATEMENT_START);

        if (anchor instanceof JSExpressionStatement) {
          JSExpression expr = ((JSExpressionStatement)anchor).getExpression();
          if (expr instanceof JSAssignmentExpression && ((JSAssignmentExpression)expr).getOperationSign() != JSTokenTypes.EQ) {
            anchorChanged = true;
          }
        }
        
        if ((anchorChanged || !(anchor instanceof JSExpressionStatement)) && !implicitlyDeclared) {
          // var statement should be inserted completely
          builder.append(myReferenceExpression.getReferencedName()).append(";\n");
        }

        Document document = PsiDocumentManager.getInstance(project).getDocument(myFile);
        document.replaceString(startOffset, startOffset, builder);
        PsiDocumentManager.getInstance(project).commitDocument(document);

        try {
          CodeStyleManager.getInstance(project).reformatText(myFile, startOffset, textRange.getEndOffset() + builder.length());
        }
        catch (IncorrectOperationException e) {
          e.printStackTrace();
        }
        myFile.navigate(true);
      }
    }
  }
}
