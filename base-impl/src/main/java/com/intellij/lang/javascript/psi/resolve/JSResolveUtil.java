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

import com.intellij.javascript.documentation.JSDocumentationProvider;
import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.JSResolveHelper;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.index.JSSymbolUtil;
import com.intellij.lang.javascript.index.JSTypeEvaluateManager;
import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.impl.JSFileImpl;
import com.intellij.lang.javascript.psi.impl.JSReferenceExpressionImpl;
import com.intellij.lang.javascript.psi.impl.JSStubElementImpl;
import com.intellij.xml.XmlElementDescriptor;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.application.util.*;
import consulo.application.util.function.Processor;
import consulo.component.extension.Extensions;
import consulo.content.base.BinariesOrderRootType;
import consulo.content.bundle.Sdk;
import consulo.content.scope.SearchScope;
import consulo.javascript.language.JavaScriptVersionUtil;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import consulo.javascript.module.extension.JavaScriptModuleExtension;
import consulo.javascript.psi.JavaScriptImportStatementBase;
import consulo.language.ast.ASTNode;
import consulo.language.ast.TokenSet;
import consulo.language.editor.util.PsiUtilBase;
import consulo.language.file.inject.VirtualFileWindow;
import consulo.language.file.light.LightVirtualFile;
import consulo.language.impl.psi.LightElement;
import consulo.language.inject.InjectedLanguageManager;
import consulo.language.psi.*;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.language.psi.scope.LocalSearchScope;
import consulo.language.psi.search.FilenameIndex;
import consulo.language.psi.stub.FileBasedIndex;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.StubIndex;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.language.util.ModuleUtilCore;
import consulo.module.Module;
import consulo.module.content.ProjectFileIndex;
import consulo.module.content.ProjectRootManager;
import consulo.project.DumbService;
import consulo.project.Project;
import consulo.util.collection.ArrayUtil;
import consulo.util.collection.SmartList;
import consulo.util.collection.primitive.ints.IntMaps;
import consulo.util.collection.primitive.ints.IntObjectMap;
import consulo.util.dataholder.Key;
import consulo.util.lang.Comparing;
import consulo.util.lang.StringUtil;
import consulo.util.lang.ref.SimpleReference;
import consulo.virtualFileSystem.VirtualFile;
import consulo.virtualFileSystem.util.VirtualFileUtil;
import consulo.xml.ide.highlighter.XmlFileType;
import consulo.xml.psi.xml.*;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.*;

/**
 * @author max, maxim.mossienko
 */
public class JSResolveUtil {
    private static final Key<CachedValue<IntObjectMap<Object>>> MY_CACHED_STATEMENTS = Key.create("JS.RelevantStatements");
    private static final UserDataCache<CachedValue<IntObjectMap<Object>>, JSElement, Object> OUR_CACHED_DEFS_CACHE =
        new RelevantDefsUserDataCache();
    public static final String PROTOTYPE_FIELD_NAME = "prototype";

    private static final String ARRAY_TYPE_NAME = "Array";
    private static final String ARGUMENTS_TYPE_NAME = "Arguments";

    public static final String COMMENT_DELIMITERS = "|/";

    public static void processInjectedFileForTag(@Nonnull XmlTag tag, @Nonnull JSInjectedFilesVisitor visitor) {
        InjectedLanguageManager injectedLanguageManager = InjectedLanguageManager.getInstance(tag.getProject());

        for (XmlTagChild child : tag.getValue().getChildren()) {
            if (child instanceof XmlText xmlText) {
                injectedLanguageManager.enumerate(xmlText, visitor);
            }
        }
    }

    @RequiredReadAction
    public static String findPackageForMxml(PsiElement expression) {
        String s = null;
        PsiFile containingFile = expression.getContainingFile();

        if (containingFile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4 && containingFile.getContext() != null) {
            PsiFile contextContainigFile = containingFile.getContext().getContainingFile();
            VirtualFile file = contextContainigFile.getVirtualFile();
            if (file == null && contextContainigFile.getOriginalFile() != null) {
                file = contextContainigFile.getOriginalFile().getVirtualFile();
            }

            s = getExpectedPackageNameFromFile(file, containingFile.getProject(), true);
        }
        return s;
    }

    public static String getExpectedPackageNameFromFile(VirtualFile file, Project project, boolean allowEvaluationFromContextRoot) {
        ProjectFileIndex projectFileIndex = ProjectRootManager.getInstance(project).getFileIndex();
        Module moduleForFile = file != null ? projectFileIndex.getModuleForFile(file) : null;

        if (moduleForFile != null) {
            if (file instanceof VirtualFileWindow virtualFileWindow) {
                file = virtualFileWindow.getDelegate();
            }
            VirtualFile rootForFile = projectFileIndex.getSourceRootForFile(file);
            if (rootForFile == null && allowEvaluationFromContextRoot) {
                rootForFile = projectFileIndex.getContentRootForFile(file);
            }

            if (rootForFile != null) {
                return VirtualFileUtil.getRelativePath(file.isDirectory() ? file : file.getParent(), rootForFile, '.');
            }
        }
        return null;
    }

    public static void processInterfaceMethods(JSClass clazz, CollectMethodsToImplementProcessor implementedMethodProcessor) {
        clazz.processDeclarations(implementedMethodProcessor, ResolveState.initial(), clazz, clazz);
    }

    @RequiredReadAction
    public static String getExpressionType(JSExpression expression, PsiFile containingFile) {
        String type = getQualifiedExpressionType(expression, containingFile);

        return getShortenedType(type, expression);
    }

    public static String getShortenedType(String type, PsiElement context) {
        if (type == null) {
            type = "*";
        }
        else {
            String shortName = getShortTypeName(type);
            String expr = JSImportHandlingUtil.resolveTypeName(shortName, context);
            if (expr != null && !expr.equals(shortName)) {
                type = shortName;
            }
        }
        return type;
    }

    @RequiredReadAction
    public static String getQualifiedExpressionType(JSExpression expression, PsiFile containingFile) {
        String type = null;

        if (expression != null) {
            BaseJSSymbolProcessor.SimpleTypeProcessor processor =
                new BaseJSSymbolProcessor.SimpleTypeProcessor(JavaScriptVersionUtil.getFeatures(expression));
            BaseJSSymbolProcessor.doEvalForExpr(expression, containingFile, processor);
            type = processor.getType();
        }

        return type;
    }

    private static String getShortTypeName(String type) {
        String str = type;

        int i = str.indexOf('<');
        String signature = null;

        if (i != -1) {
            int index = str.lastIndexOf('.', i);
            if (index == -1) {
                return type;
            }
            signature = str.substring(index);
            str = str.substring(0, index);
        }

        int i2 = str.lastIndexOf('.');
        if (i2 != -1) {
            str = str.substring(i2 + 1);
        }

        return str + (signature != null ? signature : "");
    }

    // TODO remove this method, call generic one processing includes as well (like findParent())
    public static PsiElement getClassReferenceForXmlFromContext(PsiElement parent) {
        if (parent != null && parent.getContext() instanceof XmlElement xmlElement
            && xmlElement.getContainingFile() instanceof XmlFile) {
            return XmlBackedJSClassImpl.getContainingComponent(xmlElement);
        }
        return parent;
    }

    @Nullable
    public static XmlBackedJSClassImpl getXmlBackedClass(JSFile injectedJsFile) {
        PsiElement context = injectedJsFile.getContext();
        if (context instanceof XmlAttributeValue || context instanceof XmlText) {
            return XmlBackedJSClassImpl.getContainingComponent((XmlElement)context);
        }
        return null;
    }

    @RequiredReadAction
    public static <T extends JSNamedElement & JSAttributeListOwner> SearchScope findUseScope(T jsVariableBase) {
        PsiElement element = PsiTreeUtil.getParentOfType(
            jsVariableBase,
            JSFunction.class,
            JSCatchBlock.class,
            JSClass.class,
            JSObjectLiteralExpression.class,
            JSFile.class
        );
        PsiElement scopeElement = element;

        if (element instanceof JSFile jsFile) {
            PsiElement xmlFromContext = getClassReferenceForXmlFromContext(jsFile);

            if (xmlFromContext != jsFile) {
                element = xmlFromContext;
                scopeElement = element.getContainingFile();
            }
            else {
                element = null;
            }
        }

        if (element != null) {
            if (element instanceof JSFunction function) {
                PsiElement elt =
                    JSDocumentationUtils.findDocComment(JSDocumentationProvider.findElementForWhichPreviousCommentWillBeSearched(function));
                if (elt instanceof PsiComment comment) {
                    return new LocalSearchScope(new PsiElement[]{comment, function});
                }
            }

            if (element instanceof JSClass) {
                JSAttributeList attributeList = jsVariableBase.getAttributeList();

                if (attributeList == null || attributeList.getAccessType() != JSAttributeList.AccessType.PRIVATE) {
                    return ((JSStubElementImpl)jsVariableBase).getDefaultUseScope();
                }
            }
            return new LocalSearchScope(scopeElement);
        }
        return ((JSStubElementImpl)jsVariableBase).getDefaultUseScope();
    }

    @RequiredReadAction
    public static boolean isAssignableType(String expectedType, String type, PsiElement context) {
        if ((expectedType != null && hasMultipleOccurenceDelimiters(expectedType))
            || (type != null && hasMultipleOccurenceDelimiters(type))) {
            StringTokenizer expectedTypeIterator = new StringTokenizer(expectedType != null ? expectedType : "", COMMENT_DELIMITERS);

            while (expectedTypeIterator.hasMoreElements()) {
                String primitiveExpectedType = expectedTypeIterator.nextToken().trim();
                StringTokenizer typeIterator = new StringTokenizer(type != null ? type : "", COMMENT_DELIMITERS);

                while (typeIterator.hasMoreElements()) {
                    String primitiveType = typeIterator.nextToken().trim();
                    if (isAssignableType(primitiveExpectedType, primitiveType, context)) {
                        return true;
                    }
                }
            }

            return false;
        }
        if (expectedType == null || expectedType.equals("*") || expectedType.equals(OBJECT_CLASS_NAME) || expectedType.equals("Class")) {
            return true;
        }
        if (expectedType.equals(type)) {
            return true;
        }

        boolean nonecma = context.getContainingFile().getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4;

        if ("Number".equals(expectedType) && ("int".equals(type) || "uint".equals(type)
            || (("Boolean".equals(type) || "String".equals(type)) && nonecma))) {
            return true;
        }
        if ("int".equals(expectedType)
            && ("Number".equals(type) || ("String".equals(type) && nonecma) || "uint".equals(type))) {
            return true; // compatibility
        }

        if ("uint".equals(expectedType) && ("int".equals(type) || "Number".equals(type))) {
            return true;
        }
        if (ARRAY_TYPE_NAME.equals(type)) {
            if (ARGUMENTS_TYPE_NAME.equals(expectedType) || expectedType.startsWith(ARRAY_TYPE_NAME) /*Array[*/
                || JSTypeEvaluateManager.isArrayType(expectedType)) {
                return true;
            }
        }

        if (ARGUMENTS_TYPE_NAME.equals(type) && nonecma) {
            return true; // TODO: hack for indirect call Array.slice(arguments, 0)
        }

        if (ARRAY_TYPE_NAME.equals(expectedType) && JSTypeEvaluateManager.isArrayType(type)) {
            return true;
        }
        if ("String".equals(expectedType)
            && (("Number".equals(type) || "int".equals(type) || "Boolean".equals(type) || "RegExp".equals(type))
            && nonecma)) {
            return true;
        }

        if ("*".equals(type) || (OBJECT_CLASS_NAME.equals(type) && nonecma)) {
            return true; // Dynamic cast
        }
        if ("void".equals(type)) {
            return false;
        }
        PsiElement typeClass = type != null ? unwrapProxy(findClassByQName(type, context)) : null;

        if (!(typeClass instanceof JSClass)) {
            return true;
        }

        boolean result = typeClass.processDeclarations(
            new ResolveProcessor(null) {
                {
                    setToProcessHierarchy(true);
                    setTypeContext(true);
                    setToProcessMembers(false);
                    setLocalResolve(true);
                }

                @Override
                @RequiredReadAction
                public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                    if (!(element instanceof JSClass jsClass)) {
                        return true;
                    }
                    boolean sameType = jsClass.getQualifiedName().equals(expectedType);

                    if (!sameType) {
                        for (JSClass implementedClazz : jsClass.getImplementedInterfaces()) {
                            sameType =
                                !implementedClazz.processDeclarations(this, ResolveState.initial(), implementedClazz, implementedClazz);
                            if (sameType) {
                                break;
                            }
                        }
                    }
                    return !sameType;
                }
            },
            ResolveState.initial(),
            typeClass,
            typeClass
        );

        if (result && isImplicitCastPossible((JSClass)typeClass, expectedType)) {
            result = false;
        }

        return !result;
    }

    private static boolean hasMultipleOccurenceDelimiters(String expectedType) {
        String commentDelimiters = COMMENT_DELIMITERS;

        for (int i = 0; i < commentDelimiters.length(); ++i) {
            if (expectedType.indexOf(commentDelimiters.charAt(i)) != -1) {
                return true;
            }
        }
        return false;
    }

    @RequiredReadAction
    private static boolean isImplicitCastPossible(JSClass typeClass, String expectedType) {
        // TODO: move to flex support plugin
        if (expectedType.equals("flash.events.IEventDispatcher")) {
            JSAttributeList attributeList = typeClass.getAttributeList();
            if (attributeList != null) {
                return attributeList.getAttributesByName("Bindable").length != 0;
            }
        }
        return false;
    }

    public static PsiElement getTopReferenceParent(PsiElement parent) {
        PsiElement currentParent = parent;
        for (; currentParent instanceof JSReferenceExpression; currentParent = currentParent.getParent()) {
            ;
        }
        return currentParent;
    }

    public static PsiElement getTopReferenceExpression(@Nonnull PsiElement parent) {
        PsiElement element = parent;

        for (PsiElement currentParent = parent.getParent(); currentParent instanceof JSReferenceExpression;
             element = currentParent, currentParent = currentParent.getParent()) {
            ;
        }
        return element;
    }

    @RequiredReadAction
    public static boolean isSelfReference(PsiElement currentParent, PsiElement elt) {
        return currentParent instanceof JSPackageStatement || currentParent instanceof JSNamespaceDeclaration
            || (currentParent instanceof JSVariable variable && variable.getNameIdentifier() == elt)
            || (currentParent instanceof JSFunction function && function.getNameIdentifier() == elt)
            || currentParent instanceof JSClass;
    }

    public static PsiElement findParent(PsiElement element) {
        PsiElement parent = element instanceof JSVariable variable ? variable.getParent().getParent() : element.getParent();
        return parent instanceof JSFile jsFile ? findParentClass(jsFile) : parent;
    }

    private static PsiElement findParentClass(JSFile file) {
        JSClass xmlBackedClass = getXmlBackedClass(file);
        if (xmlBackedClass != null) {
            return xmlBackedClass;
        }

        PsiElement forcedContext = file.getUserData(contextKey);
        if (forcedContext != null && !forcedContext.isValid()) {
            forcedContext = null;
        }

        if (forcedContext instanceof JSClass jsClass) {
            return jsClass;
        }

        if (forcedContext instanceof JSFile jsFile) {
            return findParentClass(jsFile);
        }

        if (forcedContext instanceof XmlFile xmlFile && JavaScriptSupportLoader.isFlexMxmFile(xmlFile)) {
            return XmlBackedJSClassImpl.getXmlBackedClass(xmlFile);
        }

        if (forcedContext instanceof XmlElement xmlElement) {
            PsiFile containingFile = xmlElement.getContainingFile();
            if (JavaScriptSupportLoader.isFlexMxmFile(containingFile)) {
                return XmlBackedJSClassImpl.getXmlBackedClass((XmlFile)containingFile);
            }
        }

        if (forcedContext != null) {
            PsiFile containingFile = forcedContext.getContainingFile();
            if (containingFile instanceof JSFile jsFile && containingFile != file) {
                return findParentClass(jsFile);
            }
        }
        return file;
    }

    @Nullable
    public static JSClass getClassOfContext(PsiElement node) {
        JSClass jsClass = PsiTreeUtil.getParentOfType(node, JSClass.class);
        if (jsClass != null) {
            return jsClass;
        }
        else {
            PsiElement context = getClassReferenceForXmlFromContext(node.getContainingFile());
            if (context instanceof JSClass contextClass) {
                return contextClass;
            }
        }

        return null;
    }

    @Nullable
    @RequiredReadAction
    public static JSClass findClassOfQualifier(JSExpression qualifier, PsiFile containingFile) {
        String s = getQualifiedExpressionType(qualifier, containingFile);
        PsiElement qName = s != null ? unwrapProxy(findClassByQName(s, containingFile)) : null;
        return qName instanceof JSClass jsClass ? jsClass : null;
    }

    public static boolean referenceExpressionShouldBeQualified(JSReferenceExpression contextExpr) {
        PsiElement parent = contextExpr.getParent();
        return parent instanceof JSImportStatement
            || (parent instanceof JSReferenceList && contextExpr.getContainingFile().getContext() != null);
    }

    public static boolean isArtificialClassUsedForReferenceList(JSClass clazz) {
        return clazz.getContainingFile().getContext() != null;
    }

    @RequiredReadAction
    public static Collection<JSQualifiedNamedElement> findElementsByName(String name, Project project, GlobalSearchScope scope) {
        Set<JSQualifiedNamedElement> result = new HashSet<>();
        Collection<JSQualifiedNamedElement> jsQualifiedNamedElements =
            StubIndex.getElements(JavaScriptIndexKeys.ELEMENTS_BY_NAME, name, project, scope, JSQualifiedNamedElement.class);

        for (JSQualifiedNamedElement e : jsQualifiedNamedElements) {
            result.add((JSQualifiedNamedElement)e.getNavigationElement());
        }

        Collection<VirtualFile> files = new ArrayList<>();
        files.addAll(FileBasedIndex.getInstance()
            .getContainingFiles(FilenameIndex.NAME, name + JavaScriptSupportLoader.MXML_FILE_EXTENSION_DOT, scope));
        files.addAll(FileBasedIndex.getInstance()
            .getContainingFiles(FilenameIndex.NAME, name + JavaScriptSupportLoader.MXML_FILE_EXTENSION2_DOT, scope));

        for (VirtualFile file : files) {
            if (!file.isValid()) {
                continue;
            }
            PsiFile psiFile = PsiManager.getInstance(project).findFile(file);
            if (psiFile != null) {
                result.add(XmlBackedJSClassImpl.getXmlBackedClass((XmlFile)psiFile));
            }
        }
        return result;
    }

    @RequiredReadAction
    public static boolean isNewResolveAndCompletion(PsiFile psiFile) {
        return psiFile.getLanguage().isKindOf(JavaScriptSupportLoader.ECMA_SCRIPT_L4) || JavaScriptSupportLoader.isFlexMxmFile(psiFile);
    }

    @RequiredReadAction
    public static String getTypeFromSetAccessor(JSNamedElement jsNamedElement) {
        if (!(jsNamedElement instanceof JSFunction function && function.isSetProperty())) {
            return null;
        }
        JSParameterList jsParameterList = function.getParameterList();
        if (jsParameterList == null) {
            return null;
        }
        JSParameter[] jsParameters = jsParameterList.getParameters();
        if (jsParameters == null || jsParameters.length != 1) {
            return null;
        }
        return jsParameters[0].getTypeString();
    }

    public static boolean shouldProcessTopLevelGlobalContext(@Nonnull PsiElement place, @Nonnull PsiScopeProcessor processor) {
        PsiElement placeParent = null;

        return shouldProcessImports(place, processor) && (
            !((placeParent = place.getParent()) instanceof JSCallExpression)
                || (place instanceof JSReferenceExpression refExpr && refExpr.getQualifier() != null
                && ((ResolveProcessor)processor).specificallyAskingToResolveQualifiedNames())
                || placeParent instanceof JSNewExpression);
    }

    public static boolean shouldProcessImports(@Nonnull PsiElement place, @Nonnull PsiScopeProcessor processor) {
        if (!(processor instanceof ResolveProcessor resolveProcessor && !resolveProcessor.isLocalResolve())) {
            return false;
        }
        return !(place instanceof JSReferenceExpression refExpr && refExpr.getQualifier() != null)
            || resolveProcessor.specificallyAskingToResolveQualifiedNames();
    }

    @RequiredReadAction
    public static boolean processTopLevelClasses(
        PsiScopeProcessor processor,
        ResolveState state,
        Project project,
        GlobalSearchScope scope,
        boolean acceptOnlyClasses,
        boolean acceptQualifiedElements
    ) {
        boolean result = true;
        String resolvedName = ((ResolveProcessor)processor).getName();

        if (resolvedName == null) {
            for (String s : StubIndex.getInstance().getAllKeys(JavaScriptIndexKeys.ELEMENTS_BY_NAME, project)) {
                for (JSQualifiedNamedElement e : StubIndex.getElements(
                    JavaScriptIndexKeys.ELEMENTS_BY_NAME,
                    s,
                    project,
                    scope,
                    JSQualifiedNamedElement.class
                )) {
                    if (ARGUMENTS_TYPE_NAME.equals(e.getName())) {
                        continue;
                    }
                    if (acceptOnlyClasses && !(e instanceof JSClass)) {
                        continue;
                    }

                    if (!acceptQualifiedElements) {
                        String qName = e.getQualifiedName();
                        if (qName != null && qName.indexOf('.') != -1) {
                            continue;
                        }
                    }
                    result &= processor.execute(e, state);
                }
            }
        }
        else {
            for (JSQualifiedNamedElement e : StubIndex.getElements(
                JavaScriptIndexKeys.ELEMENTS_BY_QNAME,
                resolvedName,
                project,
                scope,
                JSQualifiedNamedElement.class
            )) {
                if (!e.getName().equals(resolvedName)) {
                    continue;
                }
                if (acceptOnlyClasses && !(e instanceof JSClass)) {
                    continue;
                }
                result = processor.execute(e, state);
                if (!result) {
                    break;
                }
            }
        }
        return result;
    }

    static boolean walkOverStructure(@Nonnull PsiElement context, Processor<PsiNamedElement> processor) {
        PsiNamedElement parent = PsiTreeUtil.getNonStrictParentOfType(context, JSQualifiedNamedElement.class, PsiFile.class);

        if (parent instanceof JSClass jsClass) {
            PsiElement forcedContext = jsClass.getUserData(contextKey);

            if (forcedContext instanceof XmlBackedJSClassImpl xmlBackedJSClass && !processor.process(xmlBackedJSClass)) {
                return false;
            }
        }

        while (parent != null) {
            if (!processor.process(parent)) {
                return false;
            }

            if (parent instanceof JSPackageStatement) {
                break;
            }

            if (parent instanceof PsiFile file) {
                PsiElement data = file.getUserData(contextKey);

                if (data instanceof JSElement jsElement) {
                    parent = PsiTreeUtil.getNonStrictParentOfType(jsElement, JSQualifiedNamedElement.class, PsiFile.class);
                }
                else {
                    break;
                }
            }
            else {
                parent = PsiTreeUtil.getParentOfType(parent, JSQualifiedNamedElement.class, PsiFile.class);
            }
        }

        return true;
    }

    private static final Key<ParameterizedCachedValue<Set<String>, JSElement>> ourCachedOpenedNsesKey = Key.create("opened.nses");

    private static final UserDataCache<ParameterizedCachedValue<Set<String>, JSElement>, JSElement, Object> ourCachedOpenedNsesCache =
        new UserDataCache<>() {
            @Override
            protected ParameterizedCachedValue<Set<String>, JSElement> compute(JSElement jsElement, Object p) {
                return CachedValuesManager.getManager(jsElement.getProject()).createParameterizedCachedValue(
                    new ParameterizedCachedValueProvider<Set<String>, JSElement>() {
                        @Override
                        public CachedValueProvider.Result<Set<String>> compute(JSElement context) {
                            class MyProcessor extends ResolveProcessor implements Processor<PsiNamedElement> {
                                Set<String> openedNses;

                                public MyProcessor() {
                                    super(null);
                                    putUserData(ResolveProcessor.LOOKING_FOR_USE_NAMESPACES, true);
                                }

                                @Override
                                public boolean process(PsiNamedElement psiNamedElement) {
                                    if (psiNamedElement instanceof JSElement element) {
                                        processDeclarationsInScope(element, this, ResolveState.initial(), element, element);
                                    }
                                    else {
                                        // TODO: XmlFile ?
                                    }
                                    return true;
                                }

                                @Override
                                @RequiredReadAction
                                public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                                    if (!(element instanceof JSUseNamespaceDirective)) {
                                        return true;
                                    }
                                    if (openedNses == null) {
                                        openedNses = new HashSet<>();
                                    }
                                    openedNses.add(((JSUseNamespaceDirective)element).getNamespaceToBeUsed());
                                    return true;
                                }
                            }
                            MyProcessor processor = new MyProcessor();
                            walkOverStructure(context, processor);
                            return new CachedValueProvider.Result<>(processor.openedNses, PsiModificationTracker.EVER_CHANGED);
                        }
                    },
                    false
                );
            }
        };

    public static Set<String> calculateOpenNses(PsiElement place) {
        SimpleReference<Set<String>> result = new SimpleReference<>();
        walkOverStructure(
            place,
            psiNamedElement -> {
                if (psiNamedElement instanceof JSElement element) {
                    result.set(ourCachedOpenedNsesCache.get(ourCachedOpenedNsesKey, element, null).getValue(element));
                }
                return false;
            }
        );
        return result.get() != null ? result.get() : Collections.<String>emptySet();
    }

    @RequiredReadAction
    public static boolean processGlobalThings(PsiScopeProcessor processor, ResolveState state, PsiElement place, PsiElement context) {
        boolean result = true;
        Project project = context.getProject();
        GlobalSearchScope scope = context.getResolveScope();

        if (shouldProcessTopLevelGlobalContext(place, processor)) {
            //result = processTopPackages((ResolveProcessor) processor, state, project, scope);
        }

        if (result) {
            boolean acceptOnlyClasses = place instanceof JSReferenceExpression refExpr && isExprInTypeContext(refExpr);

            result = processTopLevelClasses(processor, state, project, scope, acceptOnlyClasses, true);
        }
        return result;
    }

    @Nullable
    @RequiredReadAction
    public static JSParameter findParameterForUsedArgument(@Nonnull JSExpression mainOccurence, @Nonnull JSArgumentList parent) {
        int paramIndex = 0;

        for (JSExpression expr : parent.getArguments()) {
            if (expr == mainOccurence) {
                break;
            }
            paramIndex++;
        }

        JSExpression methodExpr = ((JSCallExpression)parent.getParent()).getMethodExpression();
        if (methodExpr instanceof JSReferenceExpression refExpr) {
            ResolveResult[] results = refExpr.multiResolve(false);

            if (results.length > 0 && results[0].getElement() instanceof JSFunction function) {
                JSParameterList parameterList = function.getParameterList();
                if (parameterList != null) {
                    JSParameter[] params = parameterList.getParameters();

                    if (paramIndex < params.length) {
                        return params[paramIndex];
                    }
                }
            }
        }
        return null;
    }

    public static abstract class JSInjectedFilesVisitor
        implements PsiLanguageInjectionHost.InjectedPsiVisitor, XmlBackedJSClassImpl.InjectedFileVisitor {
        @Override
        @RequiredReadAction
        public void visit(@Nonnull PsiFile injectedPsi, @Nonnull List<PsiLanguageInjectionHost.Shred> places) {
            if (injectedPsi instanceof JSFile jsFile) {
                process(jsFile);
            }
        }

        protected abstract void process(JSFile file);

        @Override
        public void visit(XmlTag rootTag, JSFile file) {
            process(file);
        }
    }

    public static final String OBJECT_CLASS_NAME = "Object";

    private JSResolveUtil() {
    }

    private static final Key<CachedValue<PsiElement[]>> ourFileElementsValueKey = Key.create("file.elements");

    @RequiredReadAction
    public static void treeWalkUp(PsiScopeProcessor processor, PsiElement elt, PsiElement lastParent, PsiElement place) {
        treeWalkUp(processor, elt, lastParent, place, null, null);
    }

    @RequiredReadAction
    public static void treeWalkUp(
        PsiScopeProcessor processor,
        PsiElement elt,
        PsiElement lastParent,
        PsiElement place,
        PsiElement terminatingParent
    ) {
        treeWalkUp(processor, elt, lastParent, place, terminatingParent, null);
    }

    @RequiredReadAction
    private static void treeWalkUp(
        PsiScopeProcessor processor,
        PsiElement elt,
        PsiElement lastParent,
        PsiElement place,
        PsiElement terminatingParent,
        PsiElement currentScope
    ) {
        if (elt == null) {
            return;
        }

        PsiElement parentElement = elt.getContext();
        if (elt instanceof JSFunction || elt instanceof JSObjectLiteralExpression) {
            currentScope = elt;
        }

        if (parentElement instanceof JSDefinitionExpression
            || (parentElement instanceof JSAssignmentExpression && !(elt instanceof JSFunctionExpression))) {
            if (elt == terminatingParent) {
                return;
            }
            elt = parentElement.getParent();
            parentElement = elt.getParent(); // when walking a| = b, start from enclosing statement
            if (parentElement instanceof JSExpressionStatement expr) {
                elt = expr;
                parentElement = expr.getParent();
            }
            currentScope = elt;
        }
        else if (parentElement instanceof JSVariable variable && currentScope == null) {
            // when walking from variable init start from enclosing statement (function expr / object literal could reference that could reference
            // var in this case, better to check for any scope change)
            currentScope = variable;
            parentElement = variable.getParent();
        }

        boolean parentIsClass = parentElement instanceof JSClass;
        int index = -1;
        PsiElement[] children = getChildren(parentElement);

        if (children != null && children.length > 0) {
            index = 0;
            for (PsiElement el : children) {
                if (el == elt) {
                    break;
                }
                ++index;
            }
        }

        boolean finish = false;
        PsiElement cur = elt;

        do {
            if (!cur.processDeclarations(processor, ResolveState.initial(), cur == elt ? lastParent : null, place)) {
                if (processor instanceof ResolveProcessor) {
                    return;
                }
            }
            if (terminatingParent == cur) {
                finish = true;
                break;
            }
            if (cur instanceof PsiFile || parentIsClass) {
                break;
            }
            if (cur instanceof JSStatement && parentElement instanceof JSIfStatement) {
                // Do not try to resolve variables from then branch in else branch.
                break;
            }

            if (index == -1) {
                cur = cur.getPrevSibling();
            }
            else if (index != 0) {
                cur = children[--index];
            }
            else {
                cur = null;
            }
        }
        while (cur != null);

        PsiElement func = parentIsClass || finish ? null : processFunctionDeclarations(processor, parentElement);
        if (func != null) {
            return;
        }

        if (elt instanceof PsiFile) {
            if (elt instanceof XmlFile xmlFile) {
                XmlDocument document = xmlFile.getDocument();
                XmlTag rootTag = document != null ? document.getRootTag() : null;
                String rootNs = rootTag != null ? rootTag.getNamespace() : null;

                if (JavaScriptSupportLoader.isMxmlNs(rootNs)) {
                    processXmlFile(processor, xmlFile, place);
                }
                else if (rootTag != null && xmlFile.getFileType() == XmlFileType.INSTANCE) { // TODO this is bindows specific
                    processXmlFile(processor, xmlFile, place);
                }
            }
            else if (elt instanceof JSFile jsFile) {
                if (parentElement != null) {
                    XmlTag tag = PsiTreeUtil.getParentOfType(parentElement, XmlTag.class);
                    PsiFile containingFile = parentElement.getContainingFile();

                    while (tag != null) {
                        if (XmlBackedJSClassImpl.isInlineComponentTag(tag)) {
                            for (JSVariable var : ourCachedPredefinedVars.get(ourCachedPredefinedVarsKey, (XmlFile)containingFile, null)
                                .getValue()) {
                                if (!processor.execute(var, ResolveState.initial())) {
                                    return;
                                }
                            }
                            JSClass clazz = XmlBackedJSClassImpl.getXmlBackedClass(tag);
                            clazz.processDeclarations(processor, ResolveState.initial(), clazz, place);
                            return;
                        }
                        tag = PsiTreeUtil.getParentOfType(tag, XmlTag.class);
                    }

                    parentElement = containingFile;
                }
                else {
                    parentElement = jsFile.getUserData(contextKey);
                }
            }
            if (!(elt instanceof JSExpressionCodeFragment) && parentElement == null) {
                return;
            }
        }

        if (finish) {
            return;
        }
        treeWalkUp(processor, parentElement, elt, place, terminatingParent, currentScope);
    }

    private static UserDataCache<CachedValue<List<JSVariable>>, XmlFile, Object> ourCachedPredefinedVars =
        new UserDataCache<>() {
            @Override
            @RequiredReadAction
            protected CachedValue<List<JSVariable>> compute(final XmlFile xmlFile, Object p) {
                return CachedValuesManager.getManager(xmlFile.getProject()).createCachedValue(
                    () -> {
                        SmartList<JSVariable> vars = new SmartList<>();
                        String qName = XmlBackedJSClassImpl.getXmlBackedClass(xmlFile).getQualifiedName();
                        vars.add(new ImplicitJSVariableImpl("outerDocument", qName, xmlFile));
                        vars.add(new ImplicitJSVariableImpl("data", OBJECT_CLASS_NAME, xmlFile));
                        return new CachedValueProvider.Result<List<JSVariable>>(vars, xmlFile);
                    },
                    false
                );
            }
        };

    private static Key<CachedValue<List<JSVariable>>> ourCachedPredefinedVarsKey = Key.create("ourCachedPredefinedVarsKey");

    public static boolean processXmlFile(PsiScopeProcessor processor, XmlFile xmlFile, PsiElement place) {
        JSClass clazz = XmlBackedJSClassImpl.getXmlBackedClass(xmlFile);
        return clazz.processDeclarations(processor, ResolveState.initial(), clazz, place);
    }

    private static PsiElement[] getChildren(PsiElement element) {
        if (!(element instanceof JSFile) && !(element instanceof JSBlockStatement block && block.getParent() instanceof JSNamedElement)) {
            return null;
        }
        CachedValue<PsiElement[]> value = element.getUserData(ourFileElementsValueKey);

        if (value == null) {
            value = CachedValuesManager.getManager(element.getProject())
                .createCachedValue(() -> new CachedValueProvider.Result<>(element.getChildren(), element), false);
            element.putUserData(ourFileElementsValueKey, value);
        }

        return value.getValue();
    }

    @Nullable
    @RequiredReadAction
    private static PsiElement processFunctionDeclarations(@Nonnull PsiScopeProcessor processor, @Nullable PsiElement context) {
        if (!(context instanceof JSElement)) {
            return null;
        }
        PsiElement[] children = getChildren(context);

        if (context != null) {
            int index = children != null ? children.length - 1 : -1;
            PsiElement cur = index >= 0 ? children[index] : context.getLastChild();

            while (cur != null) {
                if ((cur instanceof JSFunction || cur instanceof JSClass)
                    && !processor.execute(cur, ResolveState.initial())
                    && processor instanceof ResolveProcessor resolveProcessor) {
                    return resolveProcessor.getResult();
                }

                if (index == -1) {
                    cur = cur.getPrevSibling();
                }
                else if (index != 0) {
                    cur = children[--index];
                }
                else {
                    cur = null;
                }
            }
        }
        return null;
    }

    @RequiredReadAction
    public static JSClass findDeclaringClass(JSFunction method) {
        SimpleReference<JSClass> lastVisitedClass = new SimpleReference<>();
        iterateOverridenMethodsUp(
            method,
            jsClass -> {
                lastVisitedClass.set(jsClass);
                return true;
            },
            true
        );

        Collection<JSClass> visited = new HashSet<>();
        visitAllImplementedInterfaces(
            lastVisitedClass.get(),
            visited,
            jsClass -> {
                // hierarchy may contain maximum one interface declaring a certain method
                JSFunction interfaceMethod = jsClass.findFunctionByNameAndKind(method.getName(), method.getKind());
                if (interfaceMethod != null) {
                    // TODO check signature
                    lastVisitedClass.set(jsClass);
                    return false;
                }
                return true;
            }
        );

        return lastVisitedClass.get();
    }

    /**
     * @return true if processor said enough
     */
    private static boolean visitAllImplementedInterfaces(
        JSClass clazz,
        @Nonnull Collection<JSClass> visited,
        Processor<JSClass> processor
    ) {
        if (visited.contains(clazz)) {
            return true;
        }
        if (clazz.isInterface()) {
            visited.add(clazz);
            if (!processor.process(clazz)) {
                return true;
            }
        }
        JSClass[] interfaces = clazz.isInterface() ? clazz.getSuperClasses() : clazz.getImplementedInterfaces();
        for (JSClass anInterface : interfaces) {
            if (visitAllImplementedInterfaces(anInterface, visited, processor)) {
                return true;
            }
        }
        return false;
    }

    /**
     * @return true if processor said enough
     */
    @RequiredReadAction
    private static boolean iterateOverridenMethodsUp(JSFunction function, Processor<JSClass> processor, boolean allowDirectParent) {
        PsiElement clazz = findParent(function);
        if (!(clazz instanceof JSClass)) {
            return false;
        }
        PsiElement directParent = clazz;

        while (true) {
            if ((allowDirectParent || directParent != clazz) && !processor.process((JSClass)clazz)) {
                return true;
            }

            JSAttributeList attributeList = function.getAttributeList();
            if (attributeList == null || !attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE)) {
                return false;
            }

            // TODO check signature
            JSNamedElement overridenMethod = findOverriddenMethod(function, (JSClass)clazz);
            if (!(overridenMethod instanceof JSFunction)) {
                return false;
            }
            function = (JSFunction)overridenMethod;

            clazz = findParent(function);
            if (!(clazz instanceof JSClass)) {
                return false;
            }
        }
    }

    @RequiredReadAction
    public static boolean isReferenceTo(PsiPolyVariantReference reference, String referencedName, PsiElement _element) {
        String elementName;
        if (_element instanceof JSNamedElement namedElement) {
            elementName = namedElement.getName();
        }
        else if (_element instanceof XmlAttributeValue xmlAttributeValue) {
            elementName = xmlAttributeValue.getValue();
        }
        else if (_element instanceof PsiFile file) {
            elementName = file.getVirtualFile().getNameWithoutExtension();
        }
        else if (_element instanceof PsiDirectoryContainer directoryContainer) {
            elementName = directoryContainer.getName();
        }
        else {
            return false;
        }

        if (Comparing.equal(referencedName, elementName, true)) {
            PsiElement element = _element;

            ResolveResult[] resolveResults = reference.multiResolve(true);

            for (ResolveResult r : resolveResults) {
                PsiElement resolvedElement = r.getElement();

                if (resolvedElement.isEquivalentTo(element) ||
                    element.isEquivalentTo(resolvedElement) ||
                    ((element instanceof JSProperty || element instanceof XmlAttributeValue)
                        && resolvedElement != null
                        && resolvedElement.getParent() == element)) {

                    if (reference instanceof JSReferenceExpression referenceExpression
                        && ((referenceExpression.getParent() == resolvedElement && resolvedElement instanceof JSDefinitionExpression)
                        || resolvedElement instanceof JSFunctionExpression functionExpr
                        && functionExpr.getFunction().getNameIdentifier().getNextSibling() == reference)) {
                        return false; // do not include self to usages
                    }

                    return true;
                }

                if (resolvedElement instanceof JSFunctionExpression functionExpression) {
                    PsiElement nameIdentifier = functionExpression.getFunction().getNameIdentifier();
                    if (nameIdentifier != null && nameIdentifier.getNextSibling() == element) {
                        return true;
                    }
                }
                else if (resolvedElement instanceof JSFunction fun) {
                    if (fun.isConstructor()) {
                        if ((element instanceof JSClass jsClass && resolvedElement.getParent() == jsClass)
                            || (element instanceof JSFile jsFile && reference.getElement().getParent() != resolvedElement
                            && isPublicEntityReferenceToJSFile(findParent(resolvedElement), jsFile))) {
                            return true;
                        }
                    }
                    else if (element instanceof JSFunction anotherFun) {
                        boolean getProperty;

                        if ((getProperty = fun.isGetProperty()) || fun.isSetProperty()) {
                            if ((getProperty && anotherFun.isSetProperty()) || (!getProperty && anotherFun.isGetProperty())) {
                                PsiElement funParent = JSResolveUtil.findParent(fun);
                                PsiElement parent = JSResolveUtil.findParent(anotherFun);
                                if (funParent == parent || (parent != null && parent.isEquivalentTo(funParent))) {
                                    return true;
                                }
                            }
                        }
                        PsiElement resolvedElementParent = findParent(resolvedElement);
                        PsiElement elementParent = findParent(element);

                        if (elementParent instanceof JSClass anotherClass && resolvedElementParent instanceof JSClass) {
                            Collection<JSClass> visitedInterfaces = new HashSet<>();
                            return iterateOverridenMethodsUp(
                                fun,
                                jsClass -> {
                                    if (anotherClass.isInterface()) {
                                        return !visitAllImplementedInterfaces(
                                            jsClass,
                                            visitedInterfaces,
                                            jsClass1 -> !jsClass1.isEquivalentTo(anotherClass)
                                        );
                                    }
                                    else {
                                        return !jsClass.isEquivalentTo(anotherClass);
                                    }
                                },
                                anotherClass.isInterface()
                            );
                        }
                    }
                }
                else if (resolvedElement instanceof JSClass jsClass
                    && element instanceof JSFunction function
                    && function.getParent() == jsClass) {
                    return true;
                }

                if (
                    (resolvedElement instanceof JSClass
                        || resolvedElement instanceof JSNamespaceDeclaration
                        || resolvedElement instanceof JSFunction
                        || resolvedElement instanceof JSVariable)
                        && ((element instanceof XmlFile xmlFile && resolvedElement.getParent().getContainingFile() == xmlFile)
                        || (element instanceof JSFile jsFile && reference.getElement().getParent() != resolvedElement
                        && isPublicEntityReferenceToJSFile(resolvedElement, jsFile)))
                ) {
                    return true;
                }

                if (resolvedElement instanceof JSVariable variable && element instanceof JSFunction function
                    && fieldIsImplicitAccessorMethod(function, variable)) {
                    return true;
                }
            }
        }
        return false;
    }

    @RequiredReadAction
    private static boolean fieldIsImplicitAccessorMethod(JSFunction fun, JSVariable var) {
        if (!fun.isGetProperty() && !fun.isSetProperty()) {
            return false;
        }
        PsiElement funParent = findParent(fun);
        if (!(funParent instanceof JSClass jsClass && jsClass.isInterface())) {
            return false;
        }
        JSAttributeList attributeList = var.getAttributeList();
        if (attributeList == null || attributeList.getAccessType() != JSAttributeList.AccessType.PUBLIC) {
            return false;
        }

        if (findParent(var) instanceof JSClass varParentClass) {
            for (JSClass c : varParentClass.getImplementedInterfaces()) {
                if (c == funParent) {
                    return true;
                }
            }
        }
        return false;
    }

    private static boolean isPublicEntityReferenceToJSFile(PsiElement resolvedElement, PsiElement element) {
        PsiElement parent = resolvedElement.getParent();
        if (parent instanceof JSVarStatement varStatement) {
            parent = varStatement.getParent();
        }
        return parent instanceof JSPackageStatement packageStatement && packageStatement.getParent() == element;
    }

    @RequiredReadAction
    private static JSNamedElement findOverriddenMethod(PsiElement resolvedElement, JSClass resolvedElementClass) {
        SimpleReference<JSNamedElement> override = new SimpleReference<>();

        iterateType(
            (JSFunction)resolvedElement,
            resolvedElementClass,
            null,
            (processor, scope, className) -> {
                PsiElement result = processor.getResult();
                if (result instanceof JSNamedElement namedElement) {
                    override.set(namedElement);
                    return false;
                }
                return true;
            }
        );
        return override.get();
    }

    public static boolean checkClassHasParentOfAnotherOne(JSClass aClass, JSClass parent, @Nullable Set<JSClass> visited) {
        if (visited != null && visited.contains(aClass)) {
            return false;
        }

        for (JSClass superClazz : aClass.getSuperClasses()) {
            if (superClazz == parent) {
                return true;
            }
            if (visited == null) {
                visited = new HashSet<>();
            }
            visited.add(aClass);
            return checkClassHasParentOfAnotherOne(superClazz, parent, visited);
        }
        return false;
    }

    @Nullable
    @RequiredReadAction
    public static JSExpression getRealRefExprQualifier(JSReferenceExpression expr) {
        JSExpression qualifier = ((JSReferenceExpressionImpl)expr).getResolveQualifier();
        if (qualifier != null) {
            return qualifier;
        }

        if (isExprInTypeContext(expr)) {
            JSImportedElementResolveResult resolved = JSImportHandlingUtil.resolveTypeNameUsingImports(expr);
            if (resolved == null) {
                return expr.getQualifier();
            }

            return getRealRefExprQualifierFromResult(expr, resolved);
        }

        return qualifier;
    }

    @RequiredReadAction
    public static JSExpression getRealRefExprQualifierFromResult(
        JSReferenceExpression expr,
        JSImportedElementResolveResult resolved
    ) {
        return ((JSReferenceExpression)JSChangeUtil.createExpressionFromText(expr.getProject(), resolved.qualifiedName)).getQualifier();
    }

    @RequiredReadAction
    public static String findPackageStatementQualifier(PsiElement context) {
        if (context instanceof JSClass jsClass) {
            String s = jsClass.getQualifiedName();
            if (s != null) {
                int i = s.lastIndexOf('.');
                return i != -1 ? s.substring(0, i) : null;
            }
        }

        JSPackageStatement packageStatement = PsiTreeUtil.getNonStrictParentOfType(context, JSPackageStatement.class);

        if (packageStatement != null) {
            return packageStatement.getQualifiedName();
        }

        return null;
    }

    @RequiredReadAction
    public static boolean isExprInStrictTypeContext(JSReferenceExpression expr) {
        PsiElement parent = expr.getParent();
        boolean parentIsVar = parent instanceof JSVariable;

        return ((parentIsVar || parent instanceof JSFunction)
            && parent.getNode().findChildByType(JSTokenTypes.COLON) != null
            && (!parentIsVar || ((JSVariable)parent).getInitializer() != expr))
            || parent instanceof JSAttributeList || parent instanceof JSGenericSignature
            || parent instanceof JSImportStatement || parent instanceof JSReferenceList;
    }

    @RequiredReadAction
    public static boolean isExprInTypeContext(JSReferenceExpression expr) {
        PsiElement parent = expr.getParent();

        return isExprInStrictTypeContext(expr) || parent instanceof JSNewExpression || parent instanceof JSUseNamespaceDirective
            || (parent instanceof JSBinaryExpression binaryExpression && binaryExpression.getROperand() == expr
            && (binaryExpression.getOperationSign() == JSTokenTypes.IS_KEYWORD
            || binaryExpression.getOperationSign() == JSTokenTypes.AS_KEYWORD));
    }

    @Nullable
    @RequiredReadAction
    public static PsiElement findClassByQName(String link, @Nonnull PsiElement context) {
        return findClassByQName(link, context.getProject(), context.getResolveScope());
    }

    @Nullable
    @RequiredReadAction
    public static PsiElement findClassByQName(String link, @Nonnull GlobalSearchScope scope, @Nonnull Project project) {
        return findClassByQName(link, project, scope);
    }

    @RequiredReadAction
    private static PsiElement findClassByQName(String link, final Project project, GlobalSearchScope searchScope) {
        synchronized (project) {
            if (DumbService.isDumb(project)) {
                return null;
            }

            PsiElement[] result = new PsiElement[1];

            Collection<JSQualifiedNamedElement> candidates =
                StubIndex.getElements(JavaScriptIndexKeys.ELEMENTS_BY_QNAME, link, project, searchScope, JSQualifiedNamedElement.class);
            for (JSQualifiedNamedElement clazz : candidates) {
                if (link.equals(clazz.getQualifiedName())) {
                    if ("Object".equals(link)
                        && !JavaScriptIndex.ECMASCRIPT_JS2.equals(clazz.getContainingFile().getVirtualFile().getName()) // object from swf do
                        // not contain necessary members!
                    ) {
                        continue;
                    }
                    result[0] = clazz;
                    break;
                }
            }

            if (result[0] == null) {
                String className = link.substring(link.lastIndexOf('.') + 1);
                if (className.length() > 0
                    && (Character.isUpperCase(className.charAt(0)) || Character.isLowerCase(className.charAt(0)))
                    && !isBuiltInClassName(className)) {
                    // TODO optimization, remove when packages will be properly handled
                    result[0] = findClassByQNameViaHelper(link, project, className, searchScope);
                }
            }

            return result[0];
        }
    }

    private static boolean isBuiltInClassName(String className) {
        return OBJECT_CLASS_NAME.equals(className)
            || "Boolean".equals(className)
            || "Function".equals(className)
            || "String".equals(className);
    }

    public static boolean isPredefinedFile(PsiFile file) {
        return false;
    }

    @Nullable
    private static PsiElement findClassByQNameViaHelper(
        String link,
        Project project,
        String className,
        GlobalSearchScope scope
    ) {
        for (JSResolveHelper helper : Extensions.getExtensions(JSResolveHelper.EP_NAME)) {
            PsiElement result = helper.findClassByQName(link, project, className, scope);
            if (result != null) {
                return result;
            }
        }
        return null;
    }

    @RequiredReadAction
    public static String[] buildNameIdsForQualifier(JSExpression qualifier) {
        String[] nameIds;

        if (qualifier == null) {
            nameIds = JSSymbolUtil.buildNameIndexArray(qualifier);
        }
        else {
            ContextResolver resolver = new ContextResolver(qualifier);
            nameIds = resolver.getQualifierAsNameIndex();

            if (nameIds == null) {
                nameIds = new String[]{""};
            }
        }

        return nameIds;
    }

    public static JSExpression findClassIdentifier(JSExpression _qualifier) {
        JSExpression qualifier = _qualifier;
        if (qualifier instanceof JSReferenceExpression referenceExpression
            && PROTOTYPE_FIELD_NAME.equals(referenceExpression.getReferencedName())) {
            qualifier = referenceExpression.getQualifier();
            if (qualifier == null) {
                qualifier = _qualifier;
            }
        }

        if (qualifier instanceof JSReferenceExpression referenceExpression && referenceExpression.getQualifier() == null) {
            qualifier = JSSymbolUtil.findReferenceExpressionUsedForClassExtending(referenceExpression);
        }
        return qualifier;
    }

    @RequiredReadAction
    public static boolean iterateType(JSFunction node, PsiElement jsClass, String typeName, OverrideHandler handler) {
        if (jsClass instanceof JSClass) {
            String namespace = null;
            JSAttributeList attributeList = node.getAttributeList();
            if (attributeList != null) {
                namespace = attributeList.getNamespace();
            }
            return processOverrides(jsClass, handler, node.getName(), namespace, node);
        }

        return true;
    }

    @RequiredReadAction
    public static boolean processOverrides(
        PsiElement jsClass,
        OverrideHandler handler,
        String name,
        Object namespace,
        PsiElement context
    ) {
        JSClass clazz = (JSClass)jsClass;
        ResolveProcessor resolveProcessor = new ResolveProcessor(name, context) {
            {
                setToProcessHierarchy(true);
            }

            @Override
            @RequiredReadAction
            public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                if (!(element instanceof JSFunction)) {
                    return true;
                }
                JSFunction fun = (JSFunction)element;
                JSAttributeList attributeList = fun.getAttributeList();
                if (attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PRIVATE) {
                    return false;
                }

                if ((namespace == null && attributeList != null && attributeList.getNamespace() != null)
                    || (namespace != null && (attributeList == null || !namespace.equals(attributeList.getNamespace())))) {
                    return true;
                }
                return super.execute(element, state);
            }
        };

        for (JSClass superClazz : clazz.getSuperClasses()) {
            if (superClazz == clazz) {
                break;
            }
            boolean b = superClazz.processDeclarations(resolveProcessor, ResolveState.initial(), superClazz, context);

            if (!b) {
                PsiElement element = resolveProcessor.getResult();
                if (element == null) {
                    continue;
                }
                PsiElement parent = findParent(element);

                if (parent instanceof JSClass parentClass
                    && !handler.process(resolveProcessor, superClazz, parentClass.getQualifiedName())) {
                    return false;
                }
            }
        }

        return true;
    }

    @RequiredReadAction
    public static String getQNameToStartHierarchySearch(JSFunction node) {
        PsiElement parentNode = node.getParent();
        parentNode = getClassReferenceForXmlFromContext(parentNode);

        if (parentNode instanceof JSClass) {
            JSAttributeList attributeList = node.getAttributeList();

            if (attributeList == null ||
                !attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE) ||
                attributeList.hasModifier(JSAttributeList.ModifierType.STATIC) ||
                attributeList.getAccessType() == JSAttributeList.AccessType.PRIVATE) {
                return null;
            }

            if (parentNode instanceof JSClass jsClass) {
                return jsClass.getQualifiedName();
            }
        }
        else if (node instanceof JSFunctionExpression functionExpr
            && parentNode.getContainingFile().getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
            ContextResolver resolver = new ContextResolver(functionExpr.getFirstChild());
            return resolver.getQualifierAsString();
        }
        else if (parentNode instanceof JSFile jsFile && jsFile.getContainingFile().getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
            return node.getName();
        }
        return null;
    }

    @RequiredReadAction
    public static boolean isInPlaceWhereTypeCanBeDuringCompletion(PsiElement expr) {
        PsiElement parent = expr.getParent();
        if (parent instanceof JSArgumentList || (parent instanceof JSVariable variable && variable.getInitializer() == expr)) {
            return true;
        }
        return parent instanceof JSExpressionStatement
            && !(expr instanceof JSReferenceExpression refExpr && refExpr.getQualifier() != null);
    }

    public static boolean isPlaceWhereNsCanBe(PsiElement parent) {
        PsiElement grandParent = parent.getParent();
        return grandParent instanceof JSClass || grandParent instanceof JSPackageStatement
            || (grandParent instanceof JSFile jsFile && jsFile.getContext() == null);
    }

    @Nullable
    @RequiredReadAction
    public static String getTypeFromTagNameInMxml(@Nullable PsiElement psiElement) {
        JSClass clazz = getClassFromTagNameInMxml(psiElement);
        return clazz != null ? clazz.getQualifiedName() : null;
    }

    public static JSClass getClassFromTagNameInMxml(PsiElement psiElement) {
        XmlTag tag = psiElement != null ? PsiTreeUtil.getNonStrictParentOfType(psiElement, XmlTag.class) : null;
        if (tag != null && (tag.getNamespacePrefix().length() > 0 || JavaScriptSupportLoader.isFlexMxmFile(tag.getContainingFile()))) {
            if (isScriptContextTag(tag)) {
                tag = ((XmlFile)tag.getContainingFile()).getDocument().getRootTag();
            }
            XmlElementDescriptor descriptor = tag.getDescriptor();

            if (descriptor != null) {
                PsiElement decl = descriptor.getDeclaration();
                if (decl instanceof JSClass jsClass) {
                    return jsClass;
                }
                else if (decl instanceof XmlFile xmlFile) {
                    return XmlBackedJSClassImpl.getXmlBackedClass(xmlFile);
                }
            }
        }
        return null;
    }

    private static boolean isScriptContextTag(XmlTag tag) {
        String localName = tag.getLocalName();
        return "Script".equals(localName)
            || (!localName.isEmpty() && Character.isLowerCase(localName.charAt(0)) && !"method".equals(localName));
    }

    @RequiredReadAction
    public static boolean processMetaAttributesForClass(PsiElement jsClass, MetaDataProcessor processor) {
        return doProcessMetaAttributesForClass(unwrapProxy(jsClass), processor, null, true);
    }

    @RequiredReadAction
    private static boolean doProcessMetaAttributesForClass(
        PsiElement jsClass,
        MetaDataProcessor processor,
        PsiElement lastParent,
        boolean forward
    ) {
        if (jsClass instanceof JSClass jsClassClass) {
            if (OBJECT_CLASS_NAME.equals((jsClassClass.getQualifiedName()))) {
                return true;
            }
            PsiElement[] elements = getStubbedChildren(jsClass.getParent());
            int ind = elements.length - 1;
            while (ind >= 0 && elements[ind] != jsClass) {
                --ind;
            }
            --ind;


            while (ind >= 0) {
                PsiElement current = elements[ind];

                if (current instanceof JSIncludeDirective includeDirective) {
                    if (!processIncludeDirective(processor, jsClass, includeDirective, false)) {
                        return false;
                    }
                    --ind;
                }
                else {
                    break;
                }
            }

            JSAttributeList attributeList = jsClassClass.getAttributeList();
            if (attributeList != null && !processAttributeList(processor, jsClass, attributeList, true)) {
                return false;
            }
        }

        PsiElement[] elements = getStubbedChildren(jsClass);
        SimpleReference<PsiElement> continuePassElement = new SimpleReference<>();

        for (int i = forward ? 0 : elements.length - 1; i < elements.length && i >= 0; i += (forward ? 1 : -1)) {
            PsiElement el = elements[i];

            if (el instanceof JSIncludeDirective includeDirective) {
                if (!processIncludeDirective(processor, lastParent, includeDirective, forward)) {
                    return false;
                }
            }
            else if (el instanceof JSAttributeList attributeList /*&& lastParent != null*/) {
                if (!processAttributeList(processor, lastParent, attributeList, forward)) {
                    return false;
                }
            }
            else {
                continuePassElement.set(null);
                if (!processor.handleOtherElement(el, jsClass, continuePassElement)) {
                    return false;
                }
                PsiElement nextEl = continuePassElement.get();

                if (nextEl instanceof JSAttributeListOwner attributeListOwner) {
                    JSAttributeList attributeList = attributeListOwner.getAttributeList();

                    if (attributeList != null && !processAttributeList(processor, nextEl, attributeList, forward)) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    @RequiredReadAction
    public static boolean processAttributeList(
        MetaDataProcessor processor,
        PsiElement el,
        JSAttributeList attributeList,
        boolean forward
    ) {
        PsiElement[] elements = getStubbedChildren(attributeList);
        for (int i = forward ? 0 : elements.length - 1; i < elements.length && i >= 0; i += (forward ? 1 : -1)) {
            PsiElement cur = elements[i];

            if (cur instanceof JSIncludeDirective includeDirective) {
                if (!processIncludeDirective(processor, el, includeDirective, forward)) {
                    return false;
                }
            }
            else if (cur instanceof JSAttribute attribute) {
                if (!processor.process(attribute)) {
                    return false;
                }
            }
            else if (cur instanceof JSNamedElement) {
                break;
            }
        }

        return processor.handleOtherElement(attributeList, el, null);
    }

    @RequiredReadAction
    private static boolean processIncludeDirective(
        MetaDataProcessor processor,
        PsiElement lastParent,
        JSIncludeDirective el,
        boolean forward
    ) {
        PsiFile file = el.resolveFile();
        return !(file instanceof JSFile) || doProcessMetaAttributesForClass(file, processor, lastParent, forward);
    }

    public static PsiElement unwrapProxy(PsiElement jsClass) {
        return jsClass;
    }

    @RequiredReadAction
    public static JSDefinitionExpression getDefinitionExpr(JSExpressionStatement exprStatement) {
        JSExpression expression = exprStatement.getExpression();

        return expression instanceof JSAssignmentExpression assignment ? (JSDefinitionExpression)assignment.getLOperand() : null;
    }

    public static boolean processDeclarationsInScope(
        JSElement _scope,
        PsiScopeProcessor processor,
        ResolveState state,
        PsiElement lastParent,
        PsiElement place
    ) {
        JSElement scope = PsiUtilBase.getOriginalElement(_scope, _scope.getClass());
        if (scope == null) {
            return true;
        }
        String requiredName = null;

        if (processor instanceof ResolveProcessor resolveProcessor) {
            requiredName = resolveProcessor.getName();
        }

        boolean result = true;
        IntObjectMap<Object> defsMap = OUR_CACHED_DEFS_CACHE.get(MY_CACHED_STATEMENTS, scope, null).getValue();

        if (requiredName == null) {
            for (Object value : defsMap.values()) {
                result = dispatchResolve(processor, state, place, result, value);
            }
        }
        else {
            Object defs = defsMap.get(requiredName.hashCode());
            if (defs != null) {
                result = dispatchResolve(processor, state, place, result, defs);
            }
        }

        return result;
    }

    private static boolean dispatchResolve(
        PsiScopeProcessor processor,
        ResolveState state,
        PsiElement place,
        boolean result,
        Object o
    ) {
        if (o instanceof JSElement[] jsElements) {
            for (JSElement s : jsElements) {
                result &= s.processDeclarations(processor, state, null, place);
            }
        }
        else {
            JSElement s = (JSElement)o;
            result &= s.processDeclarations(processor, state, null, place);
        }
        return result;
    }

    @RequiredReadAction
    public static PsiElement getLocalVariableRef(JSFunction function, JSReferenceExpression expr) {
        ResolveProcessor processor = new ResolveProcessor(expr.getReferencedName(), true);

        while (function != null) {
            boolean val = function.processDeclarations(processor, ResolveState.initial(), function.getParameterList(), function);
            if (!val) {
                return processor.getResult();
            }
            function = PsiTreeUtil.getParentOfType(function, JSFunction.class);
            if (function == null) {
                break;
            }
        }
        return null;
    }

    public static class MyResolveResult implements ResolveResult {
        private final PsiElement myFunction;
        private final JavaScriptImportStatementBase myImportUsed;
        private boolean myValidResult;

        public MyResolveResult(PsiElement function) {
            this(function, true);
        }

        public MyResolveResult(PsiElement function, JSImportStatement importUsed) {
            this(function, importUsed, true);
        }

        public MyResolveResult(PsiElement function, boolean validResult) {
            this(function, null, validResult);
        }

        public MyResolveResult(PsiElement function, JavaScriptImportStatementBase importUsed, boolean validResult) {
            myFunction = function;
            myValidResult = validResult;
            myImportUsed = importUsed;
        }

        @Override
        public PsiElement getElement() {
            return myFunction;
        }

        @Override
        public boolean isValidResult() {
            return myValidResult;
        }

        public void setValid(boolean b) {
            myValidResult = b;
        }

        public JavaScriptImportStatementBase getImportUsed() {
            return myImportUsed;
        }
    }

    public static Key<PsiElement> contextKey = Key.create("context.key"); // JSElement or XmlElement

    public static class RelevantDefsUserDataCache extends UserDataCache<CachedValue<IntObjectMap<Object>>, JSElement, Object> {

        @Override
        protected CachedValue<IntObjectMap<Object>> compute(final JSElement jsElement, final Object o) {
            return CachedValuesManager.getManager(jsElement.getProject()).createCachedValue(
                () -> {
                    IntObjectMap<Object> relevantDefs = IntMaps.newIntObjectHashMap();
                    MyJSElementVisitor elementVisitor = new MyJSElementVisitor(jsElement, relevantDefs);
                    elementVisitor.startVisiting(jsElement);

                    return new CachedValueProvider.Result<>(relevantDefs, jsElement);
                },
                false
            );
        }

        private static class MyJSElementVisitor extends JSElementVisitor {
            private HashMap<String, Boolean> checkedVarsToOurStatus;
            private Set<JSFile> visitedIncludes;
            private final IntObjectMap<Object> myRelevantDefs;
            private final JSElement myBase;
            private JSElement context;

            public MyJSElementVisitor(JSElement base, IntObjectMap<Object> relevantDefs) {
                myRelevantDefs = relevantDefs;
                myBase = base;
                context = base;
            }

            @RequiredReadAction
            private void startVisiting(JSElement jsElement) {
                PsiElement first = null;

                JSSourceElement[] arrayElementsToScan = null;

                if (jsElement instanceof JSFunction function) {
                    JSSourceElement[] body = function.getBody();
                    if (body.length > 0 && body[0] instanceof JSBlockStatement block) {
                        first = PsiTreeUtil.findChildOfType(block, JSStatement.class);
                    }
                }
                else if (jsElement instanceof JSFile jsFile) {
                    if (myBase == context) {
                        first = jsFile.getFirstChild();
                    }
                    else {
                        arrayElementsToScan = getSourceElements(jsFile);
                    }
                }
                else if (jsElement instanceof JSClass || jsElement instanceof JSPackageStatement) {
                    arrayElementsToScan = getSourceElements(jsElement);
                }
                else {
                    first = jsElement.getFirstChild();
                }

                if (arrayElementsToScan != null) {
                    for (JSSourceElement elt : arrayElementsToScan) {
                        elt.accept(this);
                    }
                }
                else {
                    for (PsiElement e = first; e != null; e = e.getNextSibling()) {
                        if (e instanceof JSSourceElement sourceElement) {
                            sourceElement.accept(this);
                        }
                    }
                }
            }

            @Override
            @RequiredReadAction
            public void visitJSDefinitionExpression(JSDefinitionExpression node) {
                if (node.getExpression() instanceof JSReferenceExpression refExpr && refExpr.getQualifier() == null && false) {
                    String s = refExpr.getText();
                    if (checkedVarsToOurStatus == null) {
                        checkedVarsToOurStatus = new HashMap<>(3);
                    }
                    Boolean aBoolean = checkedVarsToOurStatus.get(s);

                    if (aBoolean == null) {
                        boolean isInjectedFile = isInjectedFile(myBase);
                        PsiElement element = null;

                        if (myBase instanceof JSFunction || isInjectedFile) {
                            ResolveProcessor processor = new ResolveProcessor(s);
                            PsiElement baseParent = myBase.getParent();
                            treeWalkUp(processor, baseParent, baseParent, baseParent);
                            element = processor.getResult();
                        }

                        aBoolean = (element == null) ? Boolean.TRUE : Boolean.FALSE;
                        checkedVarsToOurStatus.put(s, aBoolean);
                    }

                    if (aBoolean == Boolean.TRUE) {
                        addRelevantDef(node);
                    }
                }
            }

            @Override
            @RequiredReadAction
            public void visitJSVariable(JSVariable node) {
                addRelevantDef(node);
            }

            @Override
            @RequiredReadAction
            public void visitJSIncludeDirective(JSIncludeDirective includeDirective) {
                PsiFile _file = includeDirective.resolveFile();

                if (!(_file instanceof JSFile) || myBase.getContainingFile() == _file) {
                    return;
                }
                JSFile file = (JSFile)_file;
                if (visitedIncludes != null && visitedIncludes.contains(file)) {
                    return;
                }
                if (visitedIncludes == null) {
                    visitedIncludes = new HashSet<>();
                }
                visitedIncludes.add(file);

                JSElement prevContext = context;
                context = file;
                context.putUserData(contextKey, myBase);
                startVisiting(file);
                context = prevContext;
            }

            @Override
            public void visitJSVarStatement(JSVarStatement node) {
                for (JSVariable var : node.getVariables()) {
                    if (!var.isLocal()) {
                        var.accept(this);
                    }
                }
            }

            @Override
            public void visitJSParameter(JSParameter node) {
            }

            @Override
            public void visitJSFunctionExpression(JSFunctionExpression node) {
                // do not go inside other funcs
            }

            @Override
            public void visitJSObjectLiteralExpression(JSObjectLiteralExpression node) {
                // do not go inside other funcs
            }

            @Override
            public void visitJSClass(JSClass aClass) {
                // do not go inside other funcs
            }

            @Override
            public void visitJSPackageStatement(JSPackageStatement packageStatement) {
                // do not go inside other funcs
            }

            @Override
            @RequiredReadAction
            public void visitJSFunctionDeclaration(JSFunction node) {
                addRelevantDef(node);
            }

            @Override
            public void visitJSImportStatement(JSImportStatement importStatement) {
                // do not expand tree
            }

            @Override
            @RequiredReadAction
            public void visitJSUseNamespaceDirective(JSUseNamespaceDirective useNamespaceDirective) {
                String namespaceToBeUsed = useNamespaceDirective.getNamespaceToBeUsed();
                if (namespaceToBeUsed != null) {
                    addNamedElement(useNamespaceDirective, namespaceToBeUsed.hashCode());
                }
            }

            @RequiredReadAction
            private void addRelevantDef(JSNamedElement node) {
                String name = node.getName();

                if (name != null) {
                    int key = name.hashCode();
                    addNamedElement(node, key);
                }
            }

            private void addNamedElement(JSElement node, int key) {
                Object o = myRelevantDefs.get(key);

                if (o == null) {
                    myRelevantDefs.put(key, node);
                }
                else if (o instanceof JSElement element) {
                    JSElement[] newO = new JSElement[]{element, node};
                    myRelevantDefs.put(key, newO);
                }
                else {
                    JSElement[] oldO = (JSElement[])o;
                    JSElement[] newO = new JSElement[oldO.length + 1];
                    System.arraycopy(oldO, 0, newO, 0, oldO.length);
                    newO[oldO.length] = node;
                    myRelevantDefs.put(key, newO);
                }
            }

            @Override
            public void visitElement(PsiElement element) {
                element.acceptChildren(this);
            }
        }
    }

    public static class ContextResolver {
        private JSElement qualifyingExpression;
        private JSNamedElement parentContainer;
        private String typeName;

        @RequiredReadAction
        public ContextResolver(JSExpression expr) {
            if (expr instanceof JSThisExpression || expr instanceof JSSuperExpression) {
                // We need to resolve ns mapping for 'this', which function was the constructor of the object
                resolveContext(expr);
            }
            else {
                qualifyingExpression = expr instanceof JSReferenceExpression referenceExpression
                    ? JSSymbolUtil.findReferenceExpressionUsedForClassExtending(referenceExpression) : expr;
            }
        }

        @RequiredReadAction
        public ContextResolver(PsiElement element) {
            resolveContext(element);
        }

        @RequiredReadAction
        private void resolveContext(PsiElement context) {
            parentContainer = PsiTreeUtil.getParentOfType(context, JSFunction.class, JSClass.class);

            if (parentContainer instanceof JSFunctionExpression) {
                boolean changedFunctionScope = false;

                while (parentContainer instanceof JSFunctionExpression functionExpr) {
                    PsiElement functionExprParent = functionExpr.getParent();

                    if (functionExprParent instanceof JSAssignmentExpression assignment) {
                        JSExpression jsExpression = assignment.getLOperand();
                        JSElement functionExprName =
                            jsExpression instanceof JSDefinitionExpression definition ? definition.getExpression() : null;
                        qualifyingExpression = functionExprName;

                        if (functionExprName instanceof JSReferenceExpression refExpr) {
                            JSExpression functionExprNameQualifier = refExpr.getQualifier();

                            if (functionExprNameQualifier instanceof JSThisExpression) {
                                parentContainer = PsiTreeUtil.getParentOfType(functionExprName, JSFunction.class);
                                qualifyingExpression = null;
                                changedFunctionScope = true;
                                continue;
                            }
                            else if (functionExprNameQualifier instanceof JSReferenceExpression functionRefExpr) {
                                String functionExpressionNameQualifierText = functionRefExpr.getReferencedName();

                                if (PROTOTYPE_FIELD_NAME.equals(functionExpressionNameQualifierText)) {
                                    qualifyingExpression = functionRefExpr.getQualifier();
                                }
                                else if (!changedFunctionScope) {
                                    String referencedName;

                                    if (functionRefExpr.getQualifier() == null
                                        && ((referencedName = refExpr.getReferencedName()) == null
                                        || referencedName.isEmpty()
                                        || !Character.isUpperCase(referencedName.charAt(0)))) {
                                        qualifyingExpression = functionExprNameQualifier;
                                    }
                                }
                            }
                        }
                    }
                    else if (functionExprParent instanceof JSProperty property) {
                        JSElement element = PsiTreeUtil.getParentOfType(
                            property,
                            JSVariable.class,
                            JSAssignmentExpression.class,
                            JSArgumentList.class
                        );
                        if (element instanceof JSVariable variable) {
                            qualifyingExpression = variable;
                        }
                        else if (element instanceof JSAssignmentExpression assignment) {
                            JSExpression loperand = assignment.getLOperand();
                            if (loperand != null) {
                                qualifyingExpression = ((JSDefinitionExpression)loperand).getExpression();
                            }
                        }
                        else if (element instanceof JSArgumentList argumentList) {
                            qualifyingExpression = JSSymbolUtil.findQualifyingExpressionFromArgumentList(argumentList);
                        }
                    }
                    else if (functionExprParent instanceof JSNewExpression newExpression) {
                        JSElement element = PsiTreeUtil.getParentOfType(
                            newExpression,
                            JSVariable.class,
                            JSAssignmentExpression.class,
                            JSArgumentList.class
                        );

                        if (element instanceof JSVariable variable) {
                            qualifyingExpression = variable;
                        }
                        else if (element instanceof JSAssignmentExpression assignment) {
                            qualifyingExpression = ((JSDefinitionExpression)assignment.getLOperand()).getExpression();
                        }
                    }
                    else if (functionExprParent instanceof JSReferenceExpression functionRefExpr) {
                        parentContainer = PsiTreeUtil.getParentOfType(functionRefExpr, JSFunction.class);
                        continue;
                    }
                    else if (parentContainer.getName() == null) {
                        parentContainer = PsiTreeUtil.getParentOfType(functionExprParent, JSFunction.class);
                        continue;
                    }

                    break;
                }

                if (qualifyingExpression instanceof JSReferenceExpression qualifyingReferenceExpression) {
                    String functionExpressionNameQualifierText = qualifyingReferenceExpression.getReferencedName();

                    if (PROTOTYPE_FIELD_NAME.equals(functionExpressionNameQualifierText)) {
                        qualifyingExpression = qualifyingReferenceExpression.getQualifier();
                    }
                    else {

                        qualifyingExpression = JSSymbolUtil.findReferenceExpressionUsedForClassExtending(qualifyingReferenceExpression);
                    }
                }
            }

            PsiElement parentContainerParent = parentContainer != null ? parentContainer.getParent() : null;

            if (parentContainerParent instanceof JSFile jsFile) {
                parentContainerParent = getClassReferenceForXmlFromContext(jsFile);
            }

            if (parentContainerParent instanceof JSClass jsClass) {
                parentContainer = jsClass;
            }

            if (parentContainer instanceof JSFunctionExpression
                && parentContainer.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4) {
                parentContainer = null;
                qualifyingExpression = null;
                typeName = OBJECT_CLASS_NAME;
            }
        }

        @Nullable
        @RequiredReadAction
        public String getQualifierAsString() {
            String qualifierAsString = getOriginalQualifierAsString();
            return normalizeQualifier(qualifierAsString);
        }

        private static String normalizeQualifier(String qualifierAsString) {
            if ("Window".equals(qualifierAsString) || "Document".equals(qualifierAsString)) {
                qualifierAsString = StringUtil.decapitalize(qualifierAsString);
            }
            return qualifierAsString;
        }

        @RequiredReadAction
        private String getOriginalQualifierAsString() {
            if (qualifyingExpression instanceof JSLiteralExpression literal) {
                return StringUtil.stripQuotesAroundValue(literal.getText());
            }
            else if (qualifyingExpression instanceof JSReferenceExpression refExpr) {
                return refExpr.getText();
            }
            else if (qualifyingExpression instanceof JSNamedElement namedElement) {
                return namedElement.getName();
            }
            else if (parentContainer instanceof JSQualifiedNamedElement qualifiedNamedElement) {
                return qualifiedNamedElement.getQualifiedName();
            }

            return typeName;
        }

        @Nullable
        public String[] getQualifierAsNameIndex() {
            String qualifierAsString = getQualifierAsString();
            if (qualifierAsString != null) {
                List<String> list = new ArrayList<>();
                qualifierAsString = BaseJSSymbolProcessor.addIndexListFromQName(qualifierAsString, qualifyingExpression, list);
                return ArrayUtil.toStringArray(list);
            }

            return null;
        }

        @RequiredReadAction
        public static String getQualifierOfExprAsString(JSElement expr) {
            if (expr instanceof JSNamedElement namedElement) {
                return namedElement.getName();
            }
            if (expr instanceof JSReferenceExpression refExpr && PROTOTYPE_FIELD_NAME.equals(refExpr.getReferencedName())) {
                expr = refExpr.getQualifier();
            }
            return normalizeQualifier(StringUtil.stripQuotesAroundValue(expr.getText()));
        }
    }

    private static boolean isInjectedFile(PsiElement element) {
        return element instanceof PsiFile file && file.getVirtualFile() instanceof LightVirtualFile;
    }

    @RequiredReadAction
    public static void processGlobalSymbols(PsiElement target, PsiScopeProcessor processor) {
        Sdk sdk = ModuleUtilCore.getSdk(target, JavaScriptModuleExtension.class);
        if (sdk != null) {
            VirtualFile[] files = sdk.getRootProvider().getFiles(BinariesOrderRootType.getInstance());

            for (VirtualFile file : files) {
                PsiFile psiFile = PsiManager.getInstance(target.getProject()).findFile(file);
                if (psiFile instanceof JSFile jsFile
                    && !jsFile.processDeclarations(processor, ResolveState.initial(), null, target)) {
                    return;
                }
            }
        }
    }

    public interface OverrideHandler {
        boolean process(ResolveProcessor processor, PsiElement scope, String className);
    }

    public interface MetaDataProcessor {
        @RequiredReadAction
        boolean process(@Nonnull JSAttribute jsAttribute);

        boolean handleOtherElement(PsiElement el, PsiElement context, @Nullable SimpleReference<PsiElement> continuePassElement);
    }

    public static abstract class CollectMethodsToImplementProcessor extends ResolveProcessor {
        private final String myName;

        public CollectMethodsToImplementProcessor(String name, PsiElement context) {
            super(null, context);
            myName = name;

            setToSkipClassDeclarationsOnce(true);
            setToProcessHierarchy(true);
            setToProcessMembers(false);
            setTypeContext(true);
        }

        @Override
        @RequiredReadAction
        public boolean execute(@Nonnull PsiElement element, ResolveState state) {
            ResolveProcessor processor = new ResolveProcessor(myName, place) {
                @Override
                @RequiredReadAction
                public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                    if (element instanceof JSFunction function) {
                        if (OBJECT_CLASS_NAME.equals(((JSClass)findParent(function)).getQualifiedName()) || function.isConstructor()) {
                            return true;
                        }
                    }
                    else {
                        return true;
                    }
                    return super.execute(element, state);
                }

                {
                    setToProcessHierarchy(true);
                }
            };
            for (JSClass implementedInterface : ((JSClass)element).getImplementedInterfaces()) {
                if (!implementedInterface.isInterface()) {
                    continue;
                }
                boolean b =
                    implementedInterface.processDeclarations(processor, ResolveState.initial(), implementedInterface, implementedInterface);

                if (!b) {
                    process(processor);
                    break;
                }
            }

            if (myName == null && processor.getResults() != null) {
                process(processor);
            }
            return false;
        }

        protected abstract boolean process(ResolveProcessor processor);
    }

    static JSSourceElement[] getSourceElements(PsiElement owner) {
        if (owner instanceof JSStubElementImpl jsStubElement) {
            return (JSSourceElement[])jsStubElement.getStubOrPsiChildren(JSElementTypes.SOURCE_ELEMENTS, JSSourceElement.EMPTY_ARRAY);
        }
        else if (owner instanceof JSFile jsFile) {
            return jsFile.getStatements();
        }
        return JSSourceElement.EMPTY_ARRAY;
    }

    @RequiredReadAction
    public static PsiElement[] getStubbedChildren(PsiElement owner) {
        return getStubbedChildren(owner, ourStubbedFilter);
    }

    @RequiredReadAction
    public static PsiElement[] getStubbedChildren(PsiElement owner, TokenSet filter) {
        if (owner instanceof JSStubElementImpl stubElement) {
            StubElement stub = stubElement.getStub();
            if (stub != null) {
                return stub.getChildrenByType(filter, PsiElement.EMPTY_ARRAY);
            }
            return getChildrenFromTokenSet(stubElement.getNode(), filter);
        }
        else if (owner instanceof JSFile file) {
            StubElement stub = ((JSFileImpl)file).getStub();
            if (stub != null) {
                return stub.getChildrenByType(filter, PsiElement.EMPTY_ARRAY);
            }
            return getChildrenFromTokenSet(file.getNode(), filter);
        }
        return PsiElement.EMPTY_ARRAY;
    }

    private static PsiElement[] getChildrenFromTokenSet(ASTNode astNode, TokenSet filter) {
        ASTNode[] astNodes = astNode.getChildren(filter);
        PsiElement[] result = new PsiElement[astNodes.length];
        for (int i = 0; i < astNodes.length; ++i) {
            result[i] = astNodes[i].getPsi();
        }
        return result;
    }

    private static final TokenSet ourStubbedFilter = TokenSet.create(
        JSElementTypes.CLASS,
        JSElementTypes.VAR_STATEMENT,
        JSElementTypes.FUNCTION_DECLARATION,
        JSElementTypes.ATTRIBUTE_LIST,
        JSElementTypes.INCLUDE_DIRECTIVE,
        JSElementTypes.ATTRIBUTE,
        JSElementTypes.PACKAGE_STATEMENT
    );

    private static class ImplicitJSVariableImpl extends LightElement implements JSVariable {
        private final PsiFile myContainingFile;
        private final String myName;
        private final String myType;

        public ImplicitJSVariableImpl(String name, String qName, PsiFile containingFile) {
            super(containingFile.getManager(), JavaScriptSupportLoader.ECMA_SCRIPT_L4.getBaseLanguage());
            myContainingFile = containingFile;
            myName = name;
            myType = qName;
        }

        @Override
        public JSAttributeList getAttributeList() {
            return null;
        }

        @Override
        public boolean isValid() {
            return myContainingFile.isValid();
        }

        @Override
        public PsiElement getParent() {
            return myContainingFile;
        }

        @Override
        public PsiFile getContainingFile() {
            return myContainingFile;
        }

        @RequiredReadAction
        @Override
        public String getText() {
            return null;
        }

        @Override
        public void accept(@Nonnull PsiElementVisitor visitor) {
            if (visitor instanceof JSElementVisitor elementVisitor) {
                elementVisitor.visitJSVariable(this);
            }
            else {
                visitor.visitElement(this);
            }
        }

        @Override
        public PsiElement copy() {
            return new ImplicitJSVariableImpl(myName, myType, myContainingFile);
        }

        @Override
        public boolean canNavigate() {
            return false;
        }

        @Override
        public boolean isPhysical() {
            return false;
        }

        @Nonnull
        @Override
        public ASTNode getNode() {
            return null;
        }

        @Override
        public boolean hasInitializer() {
            return false;
        }

        @Override
        @RequiredReadAction
        public JSExpression getInitializer() {
            return null;
        }

        @Override
        @RequiredReadAction
        public String getInitializerText() {
            return null;
        }

        @Override
        public void setInitializer(JSExpression expr) throws IncorrectOperationException {
        }

        @Nonnull
        @Override
        public JavaScriptType getType() {
            return JavaScriptType.UNKNOWN;
        }

        @Override
        public String getTypeString() {
            return myType;
        }

        @RequiredReadAction
        @Override
        public JavaScriptTypeElement getTypeElement() {
            return null;
        }

        @Override
        public boolean isConst() {
            return false;
        }

        @Override
        public boolean isLocal() {
            return false;
        }

        @Override
        public boolean isDeprecated() {
            return false;
        }

        @Override
        @RequiredReadAction
        public String getQualifiedName() {
            return myName;
        }

        @Override
        @RequiredReadAction
        public String getName() {
            return myName;
        }

        @Override
        @RequiredWriteAction
        public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
            throw new IncorrectOperationException();
        }

        @Override
        @RequiredReadAction
        public PsiElement getNameIdentifier() {
            return null;
        }

        @Override
        public String toString() {
            return "js_implicit_var:" + myName + ", type:" + myType + ", file:" + myContainingFile;
        }

        @Override
        public boolean isEquivalentTo(PsiElement another) {
            return another == this ||
                (another instanceof ImplicitJSVariableImpl implicitJSVar
                    && myContainingFile == implicitJSVar.myContainingFile && myName.equals(implicitJSVar.myName));
        }
    }
}
