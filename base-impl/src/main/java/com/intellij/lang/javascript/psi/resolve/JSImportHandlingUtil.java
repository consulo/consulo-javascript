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

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.FlexImportSupport;
import com.intellij.lang.javascript.flex.JSResolveHelper;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSClassBase;
import com.intellij.lang.javascript.psi.impl.JSStubElementImpl;
import com.intellij.xml.XmlElementDescriptor;
import consulo.application.util.CachedValue;
import consulo.application.util.CachedValueProvider;
import consulo.application.util.CachedValuesManager;
import consulo.application.util.UserDataCache;
import consulo.application.util.function.Processor;
import consulo.component.extension.Extensions;
import consulo.javascript.lang.psi.impl.resolve.JavaScriptVersionWithHelper;
import consulo.javascript.lang.psi.impl.resolve.ResolveHelper;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiModificationTracker;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.version.LanguageVersion;
import consulo.util.dataholder.Key;
import consulo.util.lang.ref.Ref;
import consulo.xml.psi.xml.XmlFile;
import consulo.xml.psi.xml.XmlTag;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author Maxim.Mossienko
 * @since 2008-07-29
 */
public class JSImportHandlingUtil {
    public static final Key<CachedValue<Map<String, Object>>> IMPORT_LIST_CACHE_KEY = Key.create("js.import.list.cache");
    public static final UserDataCache<CachedValue<Map<String, Object>>, PsiElement, Object> IMPORT_LIST_CACHE = new ImportListDataCache();
    public static final Key<CachedValue<Map<String, JSImportedElementResolveResult>>> IMPORT_RESOLVE_CACHE_KEY =
        Key.create("js.import.resolve");
    public static final UserDataCache<CachedValue<Map<String, JSImportedElementResolveResult>>, PsiElement, Object>
        IMPORT_RESOLVE_CACHE =
        new UserDataCache<>() {
            @Override
            protected CachedValue<Map<String, JSImportedElementResolveResult>> compute(final PsiElement psiElement, final Object p) {
                return CachedValuesManager.getManager(psiElement.getProject()).createCachedValue(
                    () -> new CachedValueProvider.Result<Map<String, JSImportedElementResolveResult>>(
                        new ConcurrentHashMap<>(),
                        PsiModificationTracker.MODIFICATION_COUNT
                    ),
                    false
                );
            }
        };

    public static String resolveTypeName(final String _str, @Nonnull PsiElement context) {
        final JSImportedElementResolveResult resolveResult = _resolveTypeName(_str, context);
        if (resolveResult == null) {
            return _str;
        }
        return resolveResult.qualifiedName;
    }

    // TODO _str should be JSReferenceExpression for caching!
    private static JSImportedElementResolveResult _resolveTypeName(final String _name, @Nonnull PsiElement context) {
        String name = _name;
        if (name == null) {
            return null;
        }
        final int i = name.indexOf('<');
        String signature = null;

        if (i != -1) {
            final int index = name.lastIndexOf('.', i);
            if (index == -1) {
                return null;
            }
            signature = name.substring(index);
            name = name.substring(0, index);
        }

        if (name.indexOf('.') != -1) {
            return null;
        }

        final Ref<JSImportedElementResolveResult> resultRef = new Ref<JSImportedElementResolveResult>();

        final String name1 = name;
        JSResolveUtil.walkOverStructure(
            context,
            context1 -> {
                JSImportedElementResolveResult resolved = null;

                if (context1 instanceof XmlBackedJSClassImpl xmlBackedJSClass) { // reference list in mxml
                    XmlTag rootTag = xmlBackedJSClass.getParent();
                    final XmlElementDescriptor descriptor = rootTag != null ? rootTag.getDescriptor() : null;
                    PsiElement element = descriptor != null ? descriptor.getDeclaration() : null;
                    if (element instanceof XmlFile xmlFile) {
                        element = XmlBackedJSClassImpl.getXmlBackedClass(xmlFile);
                    }

                    final String s = element instanceof JSClass jsClass ? jsClass.getQualifiedName() : rootTag.getLocalName();
                    resolved = new JSImportedElementResolveResult(s);
                }
                else if (context1 instanceof JSQualifiedNamedElement qualifiedNamedElement) {
                    if (context1 instanceof JSClass && name1.equals(context1.getName())) {
                        resolved = new JSImportedElementResolveResult(qualifiedNamedElement.getQualifiedName());
                    }
                    else {
                        resolved = resolveTypeNameUsingImports(name1, qualifiedNamedElement);

                        if (resolved == null) {
                            final String qName = qualifiedNamedElement.getQualifiedName();
                            final String packageName = qName != null ? context1 instanceof JSPackageStatement
                                ? qName + "."
                                : qName.substring(0, qName.lastIndexOf('.') + 1) : "";

                            if (packageName.length() != 0) {
                                final PsiElement byQName = JSClassBase.findClassFromNamespace(packageName + name1, context1);

                                if (byQName instanceof JSQualifiedNamedElement byQNameNamed) {
                                    resolved = new JSImportedElementResolveResult(byQNameNamed.getQualifiedName());
                                }
                            }
                        }
                    }
                }
                else {
                    resolved = resolveTypeNameUsingImports(name1, context1);
                    PsiElement contextOfContext;

                    if (resolved == null && context1 instanceof JSFile && (contextOfContext = context1.getContext()) != null) {
                        PsiFile containingFile = contextOfContext.getContainingFile();
                        XmlBackedJSClassImpl clazz = containingFile instanceof XmlFile
                            ? (XmlBackedJSClassImpl)XmlBackedJSClassImpl.getXmlBackedClass((XmlFile)containingFile) : null;

                        if (clazz != null) {
                            ResolveProcessor r = new ResolveProcessor(name1);
                            if (!clazz.processComponentNames(r)) {
                                PsiElement resultFromProcessor = r.getResult();
                                JSClass clazzFromComponent = resultFromProcessor instanceof JSClass jsClass ? jsClass : null;

                                if (clazzFromComponent != null) {
                                    resolved = new JSImportedElementResolveResult(clazzFromComponent.getQualifiedName(), clazz, null);
                                }
                            }
                        }
                    }
                }

                if (resolved != null) {
                    resultRef.set(resolved);
                    return false;
                }

                return !(context1 instanceof JSPackageStatement);
            }
        );

        JSImportedElementResolveResult result = resultRef.get();

        if (signature != null && result != null) {
            result = result.appendSignature(signature);
        }
        return result;
    }

    private static JSQualifiedNamedElement resolveTypeNameInTheSamePackage(final String str, final PsiElement context) {
        final String packageQualifierText = JSResolveUtil.findPackageStatementQualifier(context);
        final String candidateText = packageQualifierText != null ? packageQualifierText + "." + str : str;

        PsiElement byQName = JSClassBase.findClassFromNamespace(candidateText, context);
        if (byQName instanceof JSQualifiedNamedElement) {
            return (JSQualifiedNamedElement)byQName;
        }

        if (packageQualifierText != null) {
            byQName = JSClassBase.findClassFromNamespace(str, context);
            if (byQName instanceof JSQualifiedNamedElement qualifiedNamedElement) {
                return qualifiedNamedElement;
            }
        }

        return null;
    }

    public static
    @Nullable
    JSImportedElementResolveResult resolveTypeNameUsingImports(@Nonnull final JSReferenceExpression expr) {
        if (expr.getQualifier() != null) {
            return null;
        }
        if (JSResolveUtil.referenceExpressionShouldBeQualified(expr)) {
            return null;
        }

        if (expr.getReferencedName() == null) {
            return null;
        }

        return _resolveTypeName(expr.getText(), expr);
    }

    private static
    @Nullable
    JSImportedElementResolveResult resolveTypeNameUsingImports(final @Nonnull String referencedName, PsiNamedElement parent) {
        LanguageVersion languageVersion = parent.getLanguageVersion();
        if (languageVersion instanceof JavaScriptVersionWithHelper javaScriptVersionWithHelper) {
            ResolveHelper helper = javaScriptVersionWithHelper.getHelper();

            JSImportedElementResolveResult result = helper.resolveTypeNameUsingImports(referencedName, parent);
            if (result != null) {
                return result;
            }
        }

        final Map<String, JSImportedElementResolveResult> map = IMPORT_RESOLVE_CACHE.get(IMPORT_RESOLVE_CACHE_KEY, parent, null).getValue();
        JSImportedElementResolveResult result = map.get(referencedName);

        if (result == null) {
            result = resolveTypeNameUsingImportsInner(referencedName, parent);
            map.put(referencedName, result != null ? result : JSImportedElementResolveResult.EMPTY_RESULT);
        }

        return result != JSImportedElementResolveResult.EMPTY_RESULT ? result : null;
    }

    private static JSImportedElementResolveResult resolveTypeNameUsingImportsInner(
        final String referencedName,
        final PsiNamedElement parent
    ) {
        final Map<String, Object> value = IMPORT_LIST_CACHE.get(IMPORT_LIST_CACHE_KEY, parent, null).getValue();
        JSImportedElementResolveResult expression = FlexImportSupport.tryFindInMap(referencedName, parent, value);
        if (expression != null) {
            return expression;
        }

        if (parent instanceof JSPackageStatement) {
            return checkTheSamePackageOrGlobal(referencedName, parent);
        }
        else if (parent instanceof JSFile && parent.getLanguage().isKindOf(JavaScriptSupportLoader.ECMA_SCRIPT_L4)) {
            final PsiElement element = JSResolveUtil.getClassReferenceForXmlFromContext(parent);

            if (element instanceof XmlBackedJSClassImpl xmlBackedJSClass) {
                final ResolveProcessor processor = new ResolveProcessor(referencedName);
                final boolean b = xmlBackedJSClass.doImportFromScripts(processor, parent);

                if (!b) {
                    final JSQualifiedNamedElement jsClass = (JSQualifiedNamedElement)processor.getResult();
                    return new JSImportedElementResolveResult(jsClass.getQualifiedName(), jsClass, processor.getImportUsed());
                }

                JSQualifiedNamedElement jsClass = resolveTypeNameInTheSamePackage(referencedName, element);

                if (jsClass == null) {
                    final JSClass parentClass = (JSClass)element;
                    final JSClass[] classes = parentClass.getSuperClasses();

                    if (classes != null && classes.length > 0 && referencedName.equals(classes[0].getName())) {
                        jsClass = classes[0];
                    }
                }

                if (jsClass != null) {
                    return new JSImportedElementResolveResult(jsClass.getQualifiedName(), jsClass, null);
                }
            }
            else {
                final JSImportedElementResolveResult resolveResult = checkTheSamePackageOrGlobal(referencedName, parent);
                if (resolveResult != null) {
                    return resolveResult;
                }
            }

            expression = FlexImportSupport.resolveTypeNameUsingImplicitImports(referencedName, (JSFile)parent);
            if (expression != null) {
                return expression;
            }
        }
        else if (parent instanceof XmlBackedJSClassImpl xmlBackedJSClass) {
            JSQualifiedNamedElement jsClass = resolveTypeNameInTheSamePackage(referencedName, parent);
            if (jsClass != null) {
                return new JSImportedElementResolveResult(jsClass.getQualifiedName(), jsClass, null);
            }

            final Ref<JSImportedElementResolveResult> result = new Ref<JSImportedElementResolveResult>();
            processInlineComponentsInScope(
                xmlBackedJSClass,
                inlineComponent -> {
                    if (referencedName.equals(inlineComponent.getExplicitName())) {
                        result.set(new JSImportedElementResolveResult(inlineComponent.getQualifiedName(), inlineComponent, null));
                        return false;
                    }
                    return true;
                }
            );
            if (!result.isNull()) {
                return result.get();
            }
        }

        return null;
    }

    private static boolean processInlineComponentsInScope(XmlBackedJSClassImpl context, Processor<XmlBackedJSClassImpl> processor) {
        XmlTag rootTag = ((XmlFile)context.getContainingFile()).getDocument().getRootTag();
        boolean recursive = XmlBackedJSClassImpl.isInlineComponentTag(context.getParent());
        for (XmlBackedJSClassImpl inlineComponent : XmlBackedJSClassImpl.getChildInlineComponents(rootTag, recursive)) {
            if (!processor.process(inlineComponent)) {
                return false;
            }
        }
        return true;
    }

    private static JSImportedElementResolveResult checkTheSamePackageOrGlobal(final String referencedName, final PsiNamedElement parent) {
        final JSQualifiedNamedElement jsClass = resolveTypeNameInTheSamePackage(referencedName, parent);

        if (jsClass != null) {
            return new JSImportedElementResolveResult(jsClass.getQualifiedName(), jsClass, null);
        }
        return null;
    }

    public static boolean tryResolveImports(final PsiScopeProcessor processor, PsiNamedElement parent, @Nonnull PsiElement place) {
        return !isAdequatePlaceForImport(parent, place) || !importClass(processor, parent);
    }

    public static boolean importClass(final PsiScopeProcessor processor, final PsiNamedElement parent) {
        if (processor instanceof ResolveProcessor resolveProcessor && resolveProcessor.isLocalResolve()) {
            return false;
        }
        ResolveProcessor resolveProcessor = (ResolveProcessor)processor;
        final String s = resolveProcessor.getName();

        if (s != null) {
            if (resolveProcessor.specificallyAskingToResolveQualifiedNames()) {
                final Map<String, Object> value = IMPORT_LIST_CACHE.get(IMPORT_LIST_CACHE_KEY, parent, null).getValue();
                JSImportedElementResolveResult resolveResult =
                    FlexImportSupport.tryFindInMap(s, parent, value, resolveProcessor.getQualifiedNameToImport());
                if (dispatchResult(resolveResult, processor)) {
                    return true;
                }
            }

            final JSImportedElementResolveResult expression = resolveTypeNameUsingImports(s, parent);

            if (dispatchResult(expression, processor)) {
                return true;
            }
        }
        else {
            if (parent instanceof XmlBackedJSClassImpl xmlBackedJSClass && !processInlineComponentsInScope(
                xmlBackedJSClass,
                inlineComponent -> processor.execute(inlineComponent, ResolveState.initial())
            )) {
                return false;
            }
            final String packageQualifierText = JSResolveUtil.findPackageStatementQualifier(parent);
            importClassViaHelper(processor, parent, packageQualifierText);
        }

        return false;
    }

    private static boolean dispatchResult(JSImportedElementResolveResult expression, PsiScopeProcessor processor) {
        if (expression != null) {
            final PsiElement element = expression.resolvedElement;

            if (element != null) {
                ResolveState state = ResolveState.initial();
                if (expression.importStatement != null) {
                    state = state.put(ResolveProcessor.IMPORT_KEY, expression.importStatement);
                }
                return !processor.execute(element, state);
            }
        }

        return false;
    }

    public static void importClassViaHelper(
        final PsiScopeProcessor processor,
        final PsiNamedElement file,
        final String packageQualifierText
    ) {
        for (JSResolveHelper helper : Extensions.getExtensions(JSResolveHelper.EP_NAME)) {
            helper.importClass(processor, file, packageQualifierText);
        }
    }

    public static boolean isAdequatePlaceForImport(final PsiNamedElement parent, @Nonnull PsiElement place) {
        if (parent instanceof JSFile && !parent.getLanguage().isKindOf(JavaScriptSupportLoader.ECMA_SCRIPT_L4)) {
            return false;
        }

        if (place instanceof JSReferenceExpression) {
            final PsiElement placeParent = place.getParent();

            if (placeParent instanceof JSReferenceExpression) {
                final PsiElement currentParent = JSResolveUtil.getTopReferenceParent(placeParent);

                if (JSResolveUtil.isSelfReference(currentParent, place) ||
                    //currentParent instanceof JSDefinitionExpression ||
                    currentParent instanceof JSReferenceList) {
                    return false;
                }
            }
        }
        else if (place instanceof JSDocTagValue) {
            // further conditions to come
        }
        else if (!(place instanceof JSFile)) {
            return false;
        }

        return true;
    }

    private static class ImportListDataCache extends UserDataCache<CachedValue<Map<String, Object>>, PsiElement, Object> {
        @Override
        protected final CachedValue<Map<String, Object>> compute(final PsiElement owner, Object o) {
            return CachedValuesManager.getManager(owner.getProject()).createCachedValue(
                () -> {
                    final Map<String, Object> result = new HashMap<>();
                    collect(result, owner, null);
                    return new CachedValueProvider.Result<>(result, owner);
                },
                false
            );
        }

        private static void collect(final Map<String, Object> result, final PsiElement owner, Set<PsiFile> visitedIncludes) {
            PsiElement[] children = PsiElement.EMPTY_ARRAY;

            if (owner instanceof JSIncludeDirective includeDirective) {
                final PsiFile file = includeDirective.resolveFile();
                if (file != null && (visitedIncludes == null || !visitedIncludes.contains(file))) {
                    if (visitedIncludes == null) {
                        visitedIncludes = new HashSet<>();
                    }
                    visitedIncludes.add(file);
                    children = JSResolveUtil.getSourceElements(file);
                }
            }
            else if (owner instanceof JSFile || owner instanceof JSStubElementImpl) {
                children = JSResolveUtil.getSourceElements(owner);
            }
            else {
                children = owner.getChildren();
            }

            for (PsiElement c : children) {
                if (c instanceof JSImportStatement) {
                    final JSImportStatement s = ((JSImportStatement)c);

                    if (s.getImportText() != null) {
                        FlexImportSupport.appendToMap(result, s);
                    }
                }
                else if (!(c instanceof JSPackageStatement) && !(c instanceof JSFunction)) {
                    collect(result, c, visitedIncludes);
                }
            }
        }
    }
}
