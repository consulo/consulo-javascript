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

package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.psi.util.JSLookupUtil;
import com.intellij.lang.javascript.search.JSClassSearch;
import consulo.annotation.access.RequiredReadAction;
import consulo.application.util.query.Query;
import consulo.language.psi.PsiElement;
import consulo.language.psi.ResolveResult;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.project.Project;
import consulo.util.lang.StringUtil;
import consulo.util.lang.ref.Ref;
import consulo.util.lang.ref.SimpleReference;
import consulo.xml.psi.xml.XmlFile;
import consulo.xml.psi.xml.XmlTag;
import jakarta.annotation.Nonnull;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Maxim.Mossienko
 * @since 2008-01-25
 */
public class JSSmartCompletionVariantsHandler {
    @RequiredReadAction
    static Object[] getSmartVariants(@Nonnull PsiElement expr, boolean ecma) {
        PsiElement parent = expr.getParent();

        if (parent instanceof JSArgumentList argumentList && argumentList.getArguments()[0] == expr && ecma
            && ((JSReferenceExpression)expr).getQualifier() == null) {
            JSExpression calledExpr = ((JSCallExpression)parent.getParent()).getMethodExpression();

            if (calledExpr instanceof JSReferenceExpression expression) {
                String s = expression.getReferencedName();

                if ("addEventListener".equals(s) || "removeEventListener".equals(s)) {
                    List<Object> variants = new ArrayList<>();
                    MyEventSubclassesProcessor subclassesProcessor = new MyEventSubclassesProcessor(expr, variants);
                    subclassesProcessor.findAcceptableVariants(expression, parent.getProject());
                    if (variants.size() > 0) {
                        return variants.toArray(new Object[variants.size()]);
                    }
                }
            }
        }

        return null;
    }

    private static class MyEventSubclassesProcessor extends ResolveProcessor implements JSResolveUtil.MetaDataProcessor {
        private final PsiElement myExpr;
        private final List<Object> myVariants;
        private final ResolveState state = new ResolveState();
        private final Map<String, JSVariable> myCandidatesMap = new HashMap<>();
        private boolean findAcceptableEvents;

        public MyEventSubclassesProcessor(PsiElement expr, List<Object> variants) {
            super(null);
            myExpr = expr;
            myVariants = variants;

            setToProcessHierarchy(true);
        }

        public boolean process(JSClass clazz) {
            clazz.processDeclarations(this, state, clazz, clazz);

            return true;
        }

        @Override
        @RequiredReadAction
        public boolean execute(@Nonnull PsiElement element, ResolveState state) {
            if (element instanceof JSVariable variable) {
                JSAttributeList attributeList = variable.getAttributeList();

                if (attributeList != null
                    && attributeList.getAccessType() == JSAttributeList.AccessType.PUBLIC
                    && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)
                    && "String".equals(variable.getTypeString())) {
                    String s = variable.getInitializerText();
                    if (s != null && StringUtil.startsWith(s, "\"") && StringUtil.endsWith(s, "\"")) {
                        myCandidatesMap.put(StringUtil.stripQuotesAroundValue(s), variable);
                    }
                }
            }

            if (findAcceptableEvents && element instanceof JSClass jsClass) {
                JSResolveUtil.processMetaAttributesForClass(jsClass, this);
            }

            return true;
        }

        @RequiredReadAction
        public void findAcceptableVariants(JSReferenceExpression expression, Project project) {
            PsiElement clazz = JSResolveUtil.findClassByQName("flash.events.Event", expression.getResolveScope(), project);
            clazz = JSResolveUtil.unwrapProxy(clazz);
            if (!(clazz instanceof JSClass)) {
                return;
            }
            Query<JSClass> query = JSClassSearch.searchClassInheritors((JSClass)clazz, true);

            for (JSClass extendedClass : query.findAll()) {
                process(extendedClass);
            }

            JSExpression qualifier = expression.getQualifier();

            JSClass clazzToProcess = null;

            if (qualifier instanceof JSThisExpression || qualifier instanceof JSSuperExpression) {
                clazzToProcess = PsiTreeUtil.getParentOfType(qualifier, JSClass.class);
            }
            else if (qualifier instanceof JSReferenceExpression referenceExpression) {
                ResolveResult[] results = referenceExpression.multiResolve(false);
                if (results.length > 0 && results[0].getElement() instanceof JSClass jsClass) {
                    clazzToProcess = jsClass;
                }
            }

            if (clazzToProcess == null) {
                PsiElement context = expression.getContainingFile().getContext();
                clazzToProcess = JSResolveUtil.getClassFromTagNameInMxml(context);
                if (clazzToProcess == null && context != null) {
                    XmlFile file = PsiTreeUtil.getParentOfType(context, XmlFile.class);
                    if (file != null) {
                        XmlTag rootTag = file.getDocument().getRootTag();
                        XmlTag[] tags = rootTag != null
                            ? XmlBackedJSClassImpl.findMxmlSubTags(rootTag, "Metadata")
                            : XmlTag.EMPTY;
                        MyJSInjectedFilesVisitor injectedFilesVisitor = new MyJSInjectedFilesVisitor();

                        for (XmlTag tag : tags) {
                            JSResolveUtil.processInjectedFileForTag(tag, injectedFilesVisitor);
                        }
                    }
                }
            }

            if (clazzToProcess != null) {
                findAcceptableEvents = true;
                setToProcessMembers(false);
                setTypeContext(true);

                clazzToProcess.processDeclarations(this, ResolveState.initial(), clazz, clazz);
            }
        }

        @Override
        @RequiredReadAction
        public boolean process(@Nonnull JSAttribute jsAttribute) {
            if ("Event".equals(jsAttribute.getName())) {
                JSAttributeNameValuePair eventName = jsAttribute.getValueByName("name");

                if (eventName != null) {
                    String value = eventName.getSimpleValue();
                    JSVariable variable = myCandidatesMap.get(value);

                    if (variable != null) {
                        myCandidatesMap.remove(value);
                        myVariants.add(JSLookupUtil.createLookupItem(
                            variable,
                            ((JSClass)variable.getParent().getParent()).getName() + "." + variable
                                .getName(),
                            JSLookupUtil.LookupPriority.HIGHER
                        ));
                    }
                }
            }
            return true;
        }

        @Override
        public boolean handleOtherElement(PsiElement el, PsiElement context, SimpleReference<PsiElement> continuePassElement) {
            return true;
        }

        private class MyJSInjectedFilesVisitor extends JSResolveUtil.JSInjectedFilesVisitor {
            @Override
            @RequiredReadAction
            protected void process(JSFile file) {
                for (PsiElement element : file.getChildren()) {
                    if (element instanceof JSAttributeList attributeList) {
                        JSResolveUtil.processAttributeList(MyEventSubclassesProcessor.this, null, attributeList, true);
                    }
                }
            }
        }
    }
}
