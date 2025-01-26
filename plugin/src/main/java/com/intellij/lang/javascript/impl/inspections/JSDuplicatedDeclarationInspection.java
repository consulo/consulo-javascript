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

package com.intellij.lang.javascript.impl.inspections;

import com.intellij.lang.javascript.JSElementType;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.editor.inspection.ProblemsHolder;
import consulo.language.editor.rawHighlight.HighlightDisplayLevel;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.util.PsiTreeUtil;

import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 */
@ExtensionImpl
public class JSDuplicatedDeclarationInspection extends JSInspection {
    private static final String SHORT_NAME = "JSDuplicatedDeclaration";

    @Nonnull
    @Override
    public String getGroupDisplayName() {
        return "General";
    }

    @Nonnull
    @Override
    public String getDisplayName() {
        return JavaScriptLocalize.jsDuplicatedDeclarationInspectionName().get();
    }

    @Nonnull
    @Override
    public String getShortName() {
        return SHORT_NAME;
    }

    @Override
    protected JSElementVisitor createVisitor(ProblemsHolder holder) {
        return new JSElementVisitor() {
            @Override
            @RequiredReadAction
            public void visitJSClass(@Nonnull JSClass node) {
                String name = node.getName();
                if (name == null) {
                    return;
                }
                PsiElement nameIdentifier = node.getNameIdentifier();

                checkForDuplicateDeclaration(name, node, nameIdentifier);
            }

            @Override
            @RequiredReadAction
            public void visitJSFunctionDeclaration(@Nonnull JSFunction node) {
                String name = node.getName();
                if (name == null) {
                    return;
                }
                PsiElement nameIdentifier = node.getNameIdentifier();

                checkForDuplicateDeclaration(name, node, nameIdentifier);
            }

            @RequiredReadAction
            private void checkForDuplicateDeclaration(String name, PsiElement decl, PsiElement nameIdentifier) {
                PsiElement scope = PsiTreeUtil.getParentOfType(
                    decl,
                    JSFunction.class,
                    JSFile.class,
                    JSEmbeddedContentImpl.class,
                    JSClass.class,
                    JSObjectLiteralExpression.class,
                    JSPackageStatement.class,
                    PsiFile.class
                );
                if (scope instanceof JSPackageStatement) {
                    return; // dedicated inspection
                }
                PsiElement originalScope = scope;
                if (scope instanceof JSFile && scope.getContext() != null) {
                    scope = scope.getContext().getContainingFile();
                }

                ResolveProcessor processor = new ResolveProcessor(name, scope) {
                    @Override
                    @RequiredReadAction
                    public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                        if (element == decl) {
                            return true;
                        }
                        //if (!decl.getClass().isInstance(element)) return true;
                        if (decl instanceof JSParameter && decl.getParent() != element.getParent()) {
                            return false;
                        }

                        if (element instanceof JSFunction elementFunction && decl instanceof JSFunction declFunction) {
                            if ((declFunction.isGetProperty() && elementFunction.isSetProperty())
                                || (declFunction.isSetProperty() && elementFunction.isGetProperty())) {
                                return true;
                            }
                        }
                        if (element instanceof JSFunction function && decl instanceof JSClass jsClass && function.getParent() == jsClass) {
                            return true;
                        }

                        if (element instanceof JSAttributeListOwner elementAttrListOwner
                            && decl instanceof JSAttributeListOwner declAttrListOwner) {
                            JSAttributeList attrList = elementAttrListOwner.getAttributeList();
                            JSAttributeList attrList2 = declAttrListOwner.getAttributeList();

                            if (attrList != null && attrList2 != null) {
                                String ns = attrList.getNamespace();
                                String ns2 = attrList2.getNamespace();

                                if ((ns != null && !ns.equals(ns2)) ||
                                    ns2 != null && !ns2.equals(ns) ||
                                    (ns != null && ns2 != null)) {
                                    return true;
                                }
                            }
                            else if ((attrList != null && attrList.getNamespace() != null)
                                || (attrList2 != null && attrList2.getNamespace() != null)) {
                                return true;
                            }

                            boolean notStatic2 = attrList2 == null || !attrList2.hasModifier(JSAttributeList.ModifierType.STATIC);
                            boolean notStatic = attrList == null || !attrList.hasModifier(JSAttributeList.ModifierType.STATIC);
                            if ((notStatic2 && !notStatic) || (notStatic && !notStatic2)) {
                                return true;
                            }
                        }
                        return super.execute(element, state);
                    }
                };

                PsiElement parent = JSResolveUtil.findParent(decl);
                if (parent instanceof JSClass jsClass) {
                    processor.configureClassScope(jsClass);
                }

                if (decl instanceof JSFunction function || decl instanceof JSVariable) {
                    JSAttributeList attrList = ((JSAttributeListOwner)decl).getAttributeList();
                    processor.setProcessStatics(attrList != null && attrList.hasModifier(JSAttributeList.ModifierType.STATIC));
                }

                processor.setLocalResolve(true);
                JSResolveUtil.treeWalkUp(processor, decl, null, decl, scope);

                if (processor.getResult() != null && processor.getResult() != scope) {
                    holder.newProblem(JavaScriptLocalize.javascriptValidationMessageDuplicateDeclaration())
                        .range(nameIdentifier)
                        .highlightType(
                            originalScope.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4
                                ? ProblemHighlightType.ERROR
                                : ProblemHighlightType.GENERIC_ERROR_OR_WARNING
                        )
                        .create();
                }
            }

            @Override
            @RequiredReadAction
            public void visitJSProperty(@Nonnull JSProperty node) {
                String name = node.getName();
                if (name == null) {
                    return;
                }
                checkForDuplicateDeclaration(name, node, node.getNameIdentifier());
            }

            @Override
            @RequiredReadAction
            public void visitJSVariable(@Nonnull JSVariable var) {
                PsiElement nameIdentifier = var.getNameIdentifier();
                PsiElement next = nameIdentifier != null ? nameIdentifier.getNextSibling() : null;
                String name = nameIdentifier != null ? nameIdentifier.getText() : null;

                // Actully skip outer language elements
                if (name != null && (next == null
                    || PsiUtilCore.getElementType(next) instanceof JSElementType
                    || next instanceof PsiWhiteSpace)) {
                    checkForDuplicateDeclaration(name, var, nameIdentifier);
                }
            }
        };
    }

    @Override
    @Nonnull
    public HighlightDisplayLevel getDefaultLevel() {
        return HighlightDisplayLevel.WARNING;
    }
}