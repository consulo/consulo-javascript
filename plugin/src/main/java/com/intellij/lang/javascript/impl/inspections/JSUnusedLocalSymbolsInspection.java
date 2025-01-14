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

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.impl.highlighting.JavaScriptLineMarkerProvider;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSClassImpl;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.inspection.*;
import consulo.language.editor.rawHighlight.HighlightDisplayLevel;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import consulo.logging.Logger;
import consulo.project.Project;
import consulo.util.collection.primitive.objects.ObjectIntMap;
import consulo.util.collection.primitive.objects.ObjectMaps;
import consulo.util.dataholder.Key;
import jakarta.annotation.Nonnull;

import java.util.BitSet;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * @author Maxim.Mossienko
 */
@ExtensionImpl
public class JSUnusedLocalSymbolsInspection extends JSInspection {
    private static final Logger LOG = Logger.getInstance("JSUnusedLocalSymbols");
    public static final String SHORT_NAME = "JSUnusedLocalSymbols";

    @Nonnull
    @Override
    public String getGroupDisplayName() {
        return "General";
    }

    @Nonnull
    @Override
    public String getDisplayName() {
        return JavaScriptLocalize.jsUnusedLocalSymbolInspectionName().get();
    }

    @Nonnull
    @Override
    public String getShortName() {
        return SHORT_NAME;
    }

    private static final Key<Set<PsiElement>> UNUSED_LOCAL_DECLARATIONS_SET_KEY = Key.create("js unused local dcls key");
    private static final Key<Set<PsiElement>> USED_LOCAL_DECLARATIONS_SET_KEY = Key.create("js used local functions key");

    @Override
    protected JSElementVisitor createVisitor(final ProblemsHolder holder) {
        return new JSElementVisitor() {
            @Override
            public void visitJSVariable(@Nonnull JSVariable node) {
                handleLocalDeclaration(node);
            }

            @Override
            public void visitJSParameterList(@Nonnull JSParameterList node) {
                PsiElement parent = node.getParent();
                Set<PsiElement> set = parent.getUserData(UNUSED_LOCAL_DECLARATIONS_SET_KEY);

                if (set == null) {
                    parent.putUserData(UNUSED_LOCAL_DECLARATIONS_SET_KEY, Collections.synchronizedSet(new HashSet<>(3)));
                }
                else if (node.getParameters().length == 0) {
                    set.clear();
                }
            }

            @Override
            public void visitFile(PsiFile file) {
                Set<PsiElement> set = file.getUserData(UNUSED_LOCAL_DECLARATIONS_SET_KEY);
                if (set != null) {
                    set.clear();
                }
            }

            @Override
            @RequiredReadAction
            public void visitJSParameter(@Nonnull JSParameter node) {
                PsiElement scopeNode = PsiTreeUtil.getParentOfType(node, JSFunction.class, JSCatchBlock.class);

                if (scopeNode == null || scopeNode instanceof JSCatchBlock) {
                    return;
                }
                // TODO: calculate more accurately right here (we depend in this place for slow marker pass to finish before)
                if (scopeNode.getUserData(JavaScriptLineMarkerProvider.ourParticipatesInHierarchyKey) != null) {
                    return;
                }

                if (scopeNode instanceof JSFunction function) {
                    JSAttributeList attributeList = function.getAttributeList();
                    if (attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE)) {
                        return;
                    }
                }

                Set<PsiElement> unusedParametersSet;
                PsiElement parameterList = node.getParent();

                if (parameterList.getNode().findChildByType(JSElementTypes.FORMAL_PARAMETER) == node.getNode()) {
                    unusedParametersSet = Collections.synchronizedSet(new HashSet<PsiElement>(3));
                    scopeNode.putUserData(UNUSED_LOCAL_DECLARATIONS_SET_KEY, unusedParametersSet);
                }
                else {
                    unusedParametersSet = scopeNode.getUserData(UNUSED_LOCAL_DECLARATIONS_SET_KEY);
                    if (unusedParametersSet == null) {
                        return;
                    }
                }
                unusedParametersSet.add(node);
            }

            @Override
            @RequiredReadAction
            public void visitJSReferenceExpression(@Nonnull JSReferenceExpression node) {
                if (node.getParent() instanceof JSFunction) {
                    return;
                }
                if (node.getQualifier() == null) {
                    if ("arguments".equals(node.getText())) {
                        JSFunction function = PsiTreeUtil.getParentOfType(node, JSFunction.class);
                        if (function == null) {
                            return;
                        }
                        Set<PsiElement> unusedParametersSet = function.getUserData(UNUSED_LOCAL_DECLARATIONS_SET_KEY);
                        if (unusedParametersSet == null) {
                            return;
                        }
                        for (JSParameter p : function.getParameterList().getParameters()) {
                            unusedParametersSet.remove(p);
                        }
                        return;
                    }

                    ResolveResult[] results = node.multiResolve(false);

                    for (ResolveResult r : results) {
                        PsiElement element = r.getElement();

                        if (element instanceof JSVariable || (element instanceof JSFunction function && isSupportedFunction(function))) {
                            assert !(element instanceof JSFunctionExpression);
                            PsiElement scopeHandler = PsiTreeUtil.getParentOfType(element, JSFunction.class);

                            if (scopeHandler != null) {
                                Set<PsiElement> unusedParametersSet = scopeHandler.getUserData(UNUSED_LOCAL_DECLARATIONS_SET_KEY);

                                if (unusedParametersSet != null) {
                                    boolean removed = unusedParametersSet.remove(element);

                                    if (!removed) {
                                        Set<PsiElement> set = scopeHandler.getUserData(USED_LOCAL_DECLARATIONS_SET_KEY);
                                        if (set == null) {
                                            set = new HashSet<>(3);
                                            scopeHandler.putUserData(USED_LOCAL_DECLARATIONS_SET_KEY, set);
                                        }
                                        set.add(element);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            @Override
            @RequiredReadAction
            public void visitJSFunctionExpression(@Nonnull JSFunctionExpression node) {
                visitJSFunctionDeclaration(node);
            }

            @Override
            @RequiredReadAction
            public void visitJSFunctionDeclaration(@Nonnull JSFunction node) {
                processDeclarationHost(node, holder);
                handleLocalDeclaration(node);
            }
        };
    }

    private static boolean isSupportedFunction(JSFunction element) {
        return !(element instanceof JSFunctionExpression || element.getParent() instanceof JSProperty);
    }

    @RequiredReadAction
    private static void processDeclarationHost(PsiElement node, ProblemsHolder holder) {
        Set<PsiElement> unusedDeclarationsSet = node.getUserData(UNUSED_LOCAL_DECLARATIONS_SET_KEY);
        if (unusedDeclarationsSet == null || node instanceof JSFunction function && function.getBody().length == 0) {
            return;
        }

        try {
            unusedDeclarationsSet = new HashSet<>(unusedDeclarationsSet);

            int nonCounted = -2;
            int lastUsedParameterIndex = nonCounted;
            ObjectIntMap<JSParameter> parameterIndexMap = null;

            for (PsiElement p : unusedDeclarationsSet) {
                if (!p.isValid()) {
                    continue;
                }
                LocalizeValue message;
                @Nonnull PsiElement highlightedElement;

                if (p instanceof JSParameter parameter) {
                    // There are cases of predefined sinatures for which we are not interested in reported unused parameters
                    boolean ecma = node.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;
                    if (ecma && node instanceof JSFunctionExpression) {
                        continue; // do not report unused parameters
                    }
                    else if (ecma && node instanceof JSFunction function) {
                        JSParameter[] params = function.getParameterList().getParameters();

                        if (params.length == 1) {
                            String type = parameter.getTypeString();
                            if (type != null) {
                                type = JSImportHandlingUtil.resolveTypeName(type, p);
                            }

                            if (type != null) {
                                String eventType = "flash.events.Event";
                                if (eventType.equals(type)) {
                                    continue;
                                }

                                if (JSClassImpl.findClassFromNamespace(type, node) instanceof JSClass jsClass) {
                                    ResolveProcessor processor = new ResolveProcessor(eventType) {
                                        {
                                            setTypeContext(true);
                                            setToProcessMembers(false);
                                            setToProcessHierarchy(true);
                                        }

                                        @Override
                                        @RequiredReadAction
                                        public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                                            return !(element instanceof JSClass jsClass && myName.equals(jsClass.getQualifiedName()));
                                        }
                                    };
                                    processor.setLocalResolve(true);
                                    boolean b = jsClass.processDeclarations(processor, ResolveState.initial(), jsClass, jsClass);
                                    if (!b) {
                                        continue;
                                    }
                                }
                            }
                        }
                    }

                    JSParameter[] params = ((JSFunction)node).getParameterList().getParameters();

                    if (lastUsedParameterIndex == nonCounted) {
                        BitSet unusedSet = new BitSet(params.length);
                        parameterIndexMap = ObjectMaps.newObjectIntHashMap();
                        for (int i = 0; i < params.length; ++i) {
                            parameterIndexMap.putInt(params[i], i);
                        }

                        for (PsiElement param : unusedDeclarationsSet) {
                            if (!(param instanceof JSParameter)) {
                                continue;
                            }
                            unusedSet.set(parameterIndexMap.getInt((JSParameter)param));
                        }

                        lastUsedParameterIndex = -1;

                        for (int i = params.length - 1; i >= 0; --i) {
                            if (!unusedSet.get(i)) {
                                lastUsedParameterIndex = i;
                                break;
                            }
                        }
                    }

                    if (parameterIndexMap.getInt(parameter) < lastUsedParameterIndex) {
                        continue; // no sense to report unused symbol before used since it will change signature
                    }

                    message = JavaScriptLocalize.jsUnusedParameter();
                    highlightedElement = parameter.getNameIdentifier();
                }
                else if (p instanceof JSFunction function) {
                    PsiElement nameIdentifier = function.getNameIdentifier();
                    if (nameIdentifier == null) {
                        continue;
                    }
                    highlightedElement = nameIdentifier;
                    message = JavaScriptLocalize.jsUnusedFunctionDeclaration();
                }
                else {
                    highlightedElement = ((JSVariable)p).getNameIdentifier();
                    message = JavaScriptLocalize.jsUnusedLocalVariable();
                }

                ProblemBuilder problemBuilder = holder.newProblem(message)
                    .range(highlightedElement)
                    .highlightType(ProblemHighlightType.LIKE_UNUSED_SYMBOL);
                if (!(p.getParent() instanceof JSCatchBlock)) {
                    problemBuilder.withFix(new RemoveElementLocalQuickFix());
                }
                problemBuilder.create();
            }
        }
        finally {
            node.putUserData(UNUSED_LOCAL_DECLARATIONS_SET_KEY, null);
        }
    }

    private static void handleLocalDeclaration(JSNamedElement node) {
        if (node instanceof JSFunction function && !isSupportedFunction(function)) {
            return;
        }
        PsiElement scopeNode = PsiTreeUtil.getParentOfType(node, JSFunction.class, JSCatchBlock.class);
        if (scopeNode == null) {
            return;
        }
        Set<PsiElement> unusedParametersSet = scopeNode.getUserData(UNUSED_LOCAL_DECLARATIONS_SET_KEY);
        Set<PsiElement> usedSet = scopeNode.getUserData(USED_LOCAL_DECLARATIONS_SET_KEY);
        if (usedSet != null && usedSet.contains(node)) {
            return;
        }
        if (unusedParametersSet == null) {
            return;
        }
        unusedParametersSet.add(node);
    }

    @Override
    @Nonnull
    public HighlightDisplayLevel getDefaultLevel() {
        return HighlightDisplayLevel.WARNING;
    }

    private static class RemoveElementLocalQuickFix implements LocalQuickFix {
        @Nonnull
        @Override
        public String getName() {
            return JavaScriptLocalize.jsUnusedSymbolRemove().get();
        }

        @Nonnull
        @Override
        public String getFamilyName() {
            return getName();
        }

        @Override
        @RequiredWriteAction
        public void applyFix(@Nonnull Project project, @Nonnull ProblemDescriptor descriptor) {
            try {
                PsiElement element = descriptor.getPsiElement();
                if (!(element instanceof JSNamedElement)) {
                    element = element.getParent();
                }
                element.delete();
            }
            catch (IncorrectOperationException e) {
                LOG.error(e);
            }
        }
    }
}