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

package com.intellij.javascript;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.ast.IElementType;
import consulo.language.editor.completion.lookup.LookupElement;
import consulo.language.editor.completion.lookup.MutableLookupElement;
import consulo.language.editor.localize.CodeInsightLocalize;
import consulo.language.editor.parameterInfo.*;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.search.DefinitionsScopedSearch;
import consulo.util.collection.ArrayUtil;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.*;

/**
 * @author Maxim.Mossienko
 */
@ExtensionImpl
public class JSParameterInfoHandler implements ParameterInfoHandlerWithTabActionSupport<JSArgumentList, JSFunction, JSExpression> {
    private static final Set<Class<?>> ourArgumentListAllowedParentClassesSet = Set.of(JSCallExpression.class);

    @Override
    public boolean couldShowInLookup() {
        return true;
    }

    @Override
    public Object[] getParametersForLookup(LookupElement item, ParameterInfoContext context) {
        if (!(item instanceof MutableLookupElement)) {
            return null;
        }

        PsiElement element = item.getPsiElement();
        if (element instanceof JSFunction) {
            JSFunction originalFunction = (JSFunction)element;
            List<JSFunction> lookupItems = new ArrayList<>();
            Set<String> availableSignatures = new HashSet<>();

            for (PsiElement el : DefinitionsScopedSearch.search(originalFunction)) {
                doAddSignature(lookupItems, availableSignatures, el);
            }

            if (lookupItems.size() == 0) {
                lookupItems.add(originalFunction);
            }

            return lookupItems.toArray(new Object[lookupItems.size()]);
        }

        return ArrayUtil.EMPTY_OBJECT_ARRAY;
    }

    private static void doAddSignature(List<JSFunction> lookupItems, Set<String> availableSignatures, PsiElement el) {
        if (el instanceof JSFunction function) {
            JSParameterList parameterList = function.getParameterList();

            if (parameterList != null) {
                String typedSignature = buildSignature(parameterList.getParameters(), false, -1).text;
                String untypedSignature = buildSignature(parameterList.getParameters(), true, -1).text;

                if (!availableSignatures.contains(typedSignature) && !availableSignatures.contains(untypedSignature)) {
                    lookupItems.add(function);
                    availableSignatures.add(typedSignature);
                    availableSignatures.add(untypedSignature);
                }
            }
        }
    }

    @Override
    public JSArgumentList findElementForParameterInfo(CreateParameterInfoContext context) {
        JSArgumentList argList = findArgumentList(context.getFile(), context.getOffset());

        if (argList != null) {
            return fillSignaturesForArgumentList(context, argList);
        }
        return argList;
    }

    @Nullable
    public static JSArgumentList findArgumentList(PsiFile file, int offset) {
        JSArgumentList argList = ParameterInfoUtils.findParentOfType(file, offset, JSArgumentList.class);
        if (argList == null) {
            JSCallExpression callExpression = ParameterInfoUtils.findParentOfType(file, offset, JSCallExpression.class);
            if (callExpression != null) {
                argList = callExpression.getArgumentList();
            }
        }
        return argList;
    }

    @Nullable
    private static JSArgumentList fillSignaturesForArgumentList(
        CreateParameterInfoContext context,
        @Nonnull JSArgumentList argList
    ) {
        PsiElement psiElement = argList.getParent();
        if (!(psiElement instanceof JSCallExpression)) {
            return null;
        }

        JSCallExpression parent = (JSCallExpression)psiElement;
        JSExpression methodExpression = parent.getMethodExpression();

        if (methodExpression instanceof JSReferenceExpression referenceExpression) {
            ResolveResult[] resolveResults = referenceExpression.multiResolve(true);

            if (resolveResults.length > 0) {
                List<JSFunction> items = new ArrayList<JSFunction>(resolveResults.length);
                Set<String> availableSignatures = new HashSet<String>();

                for (ResolveResult r : resolveResults) {
                    PsiElement element = r.getElement();
                    if (element instanceof JSProperty property) {
                        element = property.getValue();
                    }

                    doAddSignature(items, availableSignatures, element);
                }

                context.setItemsToShow(ArrayUtil.toObjectArray(items));
                return argList;
            }
        }
        else if (methodExpression instanceof JSSuperExpression) {
            PsiElement clazz = methodExpression.getReference().resolve();
            if (clazz instanceof JSFunction) {
                context.setItemsToShow(new Object[]{clazz});
                return argList;
            }
        }
        return null;
    }

    @Override
    public void showParameterInfo(@Nonnull JSArgumentList element, CreateParameterInfoContext context) {
        context.showHint(element, element.getTextOffset(), this);
    }

    @Override
    public JSArgumentList findElementForUpdatingParameterInfo(UpdateParameterInfoContext context) {
        return findArgumentList(context.getFile(), context.getOffset());
    }

    @Override
    public void updateParameterInfo(@Nonnull JSArgumentList o, UpdateParameterInfoContext context) {
        if (context.getParameterOwner() != o) {
            context.removeHint();
            return;
        }
        int currentParameterIndex = ParameterInfoUtils.getCurrentParameterIndex(o.getNode(), context.getOffset(), JSTokenTypes.COMMA);
        context.setCurrentParameter(currentParameterIndex);
    }

    @Override
    public void updateUI(JSFunction p, ParameterInfoUIContext context) {
        JSParameterList parameterList = p.getParameterList();
        JSParameter[] params = parameterList != null ? parameterList.getParameters() : new JSParameter[0];
        int currentParameterIndex = context.getCurrentParameterIndex() >= 0 ? context.getCurrentParameterIndex() : params.length;
        JSParameter parameter = currentParameterIndex < params.length ? params[currentParameterIndex] : null;

        SignatureInfo signatureInfo = buildSignature(params, false, currentParameterIndex);
        String name = signatureInfo.text;

        String currentParameterSignature = parameter != null ? getSignatureForParameter(parameter, false) : null;
        int highlightStart = parameter != null ? signatureInfo.selectedParameterStart : 0;
        int highlightEnd = parameter != null ? highlightStart + currentParameterSignature.length() : 0;
        context.setupUIComponentPresentation(name, highlightStart, highlightEnd, false, false, false, context.getDefaultParameterColor());
    }

    private static class SignatureInfo {
        String text;
        int selectedParameterStart = -1;
    }

    private static
    @Nonnull
    SignatureInfo buildSignature(JSParameter[] params, boolean skipType, int selectedParameterIndex) {
        SignatureInfo info = new SignatureInfo();
        if (params.length > 0) {
            StringBuilder result = new StringBuilder();
            for (int i = 0; i < params.length; ++i) {
                if (result.length() > 0) {
                    result.append(", ");
                }
                if (selectedParameterIndex == i) {
                    info.selectedParameterStart = result.length();
                }
                result.append(getSignatureForParameter(params[i], skipType));
            }

            info.text = result.toString();
        }
        else {
            info.text = CodeInsightLocalize.parameterInfoNoParameters().get();
        }
        return info;
    }

    public static String getSignatureForParameter(JSParameter p, boolean skipType) {
        String s = skipType ? null : p.getTypeString();

        if (s != null && s.length() > 0) {
            boolean ecmal4 = p.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;
            String result;

            if (ecmal4) {
                if (p.isRest()) {
                    result = "...";
                }
                else {
                    result = p.getName() + ":" + s;
                }
            }
            else {
                result = "[" + s + "] " + p.getName();
            }
            String initializerText = p.getInitializerText();
            if (initializerText != null) {
                result += " = " + initializerText;
            }
            return result;
        }
        return p.getName();
    }

    @Override
    @Nonnull
    public JSExpression[] getActualParameters(@Nonnull JSArgumentList jsArgumentList) {
        return jsArgumentList.getArguments();
    }

    @Override
    @Nonnull
    public IElementType getActualParameterDelimiterType() {
        return JSTokenTypes.COMMA;
    }

    @Override
    @Nonnull
    public IElementType getActualParametersRBraceType() {
        return JSTokenTypes.RBRACE;
    }

    @Override
    @Nonnull
    public Set<Class<?>> getArgumentListAllowedParentClasses() {
        return ourArgumentListAllowedParentClassesSet;
    }

    @Nonnull
    @Override
    public Set<? extends Class<?>> getArgListStopSearchClasses() {
        return Collections.emptySet();
    }

    @Override
    @Nonnull
    public Class<JSArgumentList> getArgumentListClass() {
        return JSArgumentList.class;
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
