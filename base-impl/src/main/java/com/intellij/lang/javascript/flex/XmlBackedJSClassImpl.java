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

package com.intellij.lang.javascript.flex;

import com.intellij.lang.javascript.JSLanguageInjector;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.impl.JSClassBase;
import com.intellij.lang.javascript.psi.impl.JSPsiImplUtils;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.xml.XmlElementDescriptor;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.application.util.*;
import consulo.language.ast.ASTNode;
import consulo.language.codeStyle.CodeStyleManager;
import consulo.language.inject.InjectedLanguageManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiReference;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.resolve.PsiElementProcessor;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.util.dataholder.Key;
import consulo.virtualFileSystem.VirtualFile;
import consulo.xml.psi.XmlRecursiveElementVisitor;
import consulo.xml.psi.xml.*;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.*;

/**
 * @author Maxim.Mossienko
 */
public class XmlBackedJSClassImpl extends JSClassBase implements JSClass {
    public static final String COMPONENT_TAG_NAME = "Component";
    public static final String CLASS_NAME_ATTRIBUTE_NAME = "className";

    private volatile JSReferenceList myExtendsList;
    private volatile JSReferenceList myImplementsList;

    public XmlBackedJSClassImpl(XmlTag tag) {
        super(tag.getNode());
    }

    @Nullable
    @Override
    @RequiredReadAction
    public JSReferenceList getExtendsList() {
        JSReferenceList refList = myExtendsList;
        if (refList == null) {
            XmlTag rootTag = getParent();
            refList = createReferenceList(rootTag.getLocalName());
            refList.getParent().putUserData(JSResolveUtil.contextKey, this);
            myExtendsList = refList;
        }
        return refList;
    }

    @RequiredReadAction
    private JSReferenceList createReferenceList(String s) {
        JSClass element = (JSClass)JSChangeUtil.createJSTreeFromText(getProject(), "class C extends " + s + " {}").getPsi();
        return element.getExtendsList();
    }

    @Override
    @RequiredReadAction
    public int getTextOffset() {
        return 0;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public JSReferenceList getImplementsList() {
        JSReferenceList refList = myImplementsList;

        if (refList == null) {
            XmlTag rootTag = getParent();
            myImplementsList = refList = createReferenceList(rootTag != null ? rootTag.getAttributeValue("implements") : null);
        }
        return refList;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public PsiElement getNavigationElement() {
        return getParent();
    }

    @Override
    @RequiredReadAction
    public String getName() {
        XmlTag parent = getParent();
        if (isInlineComponentTag(parent)) {
            String explicitName = getExplicitName();
            if (explicitName != null) {
                return explicitName;
            }
        }
        PsiFile psi = parent.getContainingFile();
        VirtualFile file = psi.getVirtualFile();
        if (file == null && psi.getOriginalFile() != null) {
            file = psi.getOriginalFile().getVirtualFile();
        }
        return file != null ? file.getNameWithoutExtension() : null;
    }

    @Nullable
    @RequiredReadAction
    public String getExplicitName() {
        XmlTag parent = getParent();
        return parent.getAttributeValue(CLASS_NAME_ATTRIBUTE_NAME, parent.getNamespace());
    }

    @Override
    @RequiredReadAction
    public String getQualifiedName() {
        String name = getName();
        if (name == null) {
            return null;
        }
        PsiFile containingFile = getNode().getPsi().getContainingFile();
        String expectedPackageNameFromFile = JSResolveUtil.getExpectedPackageNameFromFile(containingFile.getVirtualFile(),
            containingFile.getProject(), true
        );
        if (expectedPackageNameFromFile != null && expectedPackageNameFromFile.length() > 0) {
            return expectedPackageNameFromFile + "." + name;
        }

        return name;
    }

    @Override
    public boolean isInterface() {
        return false;
    }

    @Override
    public boolean isDeprecated() {
        return false;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public PsiElement getNameIdentifier() {
        return getParent();
    }

    @Override
    @RequiredWriteAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        int i = name.lastIndexOf('.');
        if (i != -1) {
            name = name.substring(0, i);
        }
        JSPsiImplUtils.updateFileName(this, name, getName());
        return null;
    }

    @Override
    @Nullable
    public JSAttributeList getAttributeList() {
        return null;
    }

    @Override
    protected boolean processMembers(
        PsiScopeProcessor processor,
        ResolveState substitutor,
        PsiElement lastParent,
        PsiElement place
    ) {
        for (JSFile file : ourCachedScripts.get(CACHED_SCRIPTS_KEY, getParent(), null).getValue()) {
            if (!file.processDeclarations(processor, ResolveState.initial(), null, place)) {
                return false;
            }
        }
        return true;
    }

    @Override
    @RequiredReadAction
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState substitutor,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        boolean b = super.processDeclarations(processor, substitutor, lastParent, place);

        if (b && JSResolveUtil.shouldProcessImports(place, processor)) {
            b = JSImportHandlingUtil.tryResolveImports(processor, this, place);
            if (!b) {
                return false;
            }
            b = doImportFromScripts(processor, place);
        }

        return b;
    }

    public boolean doImportFromScripts(PsiScopeProcessor processor, PsiElement place) {
        PsiElement context = place.getContainingFile().getContext();
        if (context instanceof XmlText xmlText) {
            context = xmlText.getParent();
        }

        boolean notResolvingTypeViaImport = !(place instanceof JSFile);

        if ((context instanceof XmlAttributeValue
            && !(place instanceof JSReferenceExpression referenceExpression
            && JSResolveUtil.referenceExpressionShouldBeQualified(referenceExpression)))
            || (context instanceof XmlTag tag && (!SCRIPT_TAG_NAME.equals(tag.getLocalName()) || notResolvingTypeViaImport))
            || context == null
        ) {
            boolean useImports = JSResolveUtil.shouldProcessImports(place, processor);
            boolean adequatePlace = false;

            XmlTag parent = getParent();
            JSFile[] files = ourCachedScripts.get(CACHED_SCRIPTS_KEY, parent, null).getValue();
            for (JSFile file : files) {
                if (JSImportHandlingUtil.isAdequatePlaceForImport(file, place)) {
                    if (useImports && JSImportHandlingUtil.importClass(processor, file)) {
                        return false;
                    }
                    adequatePlace = true;
                }
            }

            if (adequatePlace && processor instanceof ResolveProcessor resolveProcessor) {
                if (!processComponentNames(resolveProcessor)) {
                    return false;
                }
            }
        }

        if (processor instanceof ResolveProcessor && JSResolveUtil.shouldProcessTopLevelGlobalContext(place, processor)
            && notResolvingTypeViaImport) {
            if (!JSResolveUtil.processGlobalThings(processor, ResolveState.initial(), place, this)) {
                return false;
            }
        }
        return true;
    }

    public boolean processComponentNames(ResolveProcessor processor) {
        String s = processor.getName();
        Map<String, String> value =
            myCachedComponentImportsCache.get(OUR_CACHED_SHORT_COMPONENTS_REF_KEY, (XmlFile)getContainingFile(), null).getValue();

        if (s != null) {
            String qName = value.get(s);
            if (qName != null) {
                PsiElement clazz = JSResolveUtil.findClassByQName(qName, this);
                if (clazz != null) {
                    return processor.execute(clazz, ResolveState.initial());
                }
            }
        }
        else {
            for (String qName : value.values()) {
                PsiElement clazz = JSResolveUtil.findClassByQName(qName, this);
                if (clazz != null && !processor.execute(clazz, ResolveState.initial())) {
                    return false;
                }
            }
        }
        return true;
    }

    private static final Key<CachedValue<JSFile[]>> CACHED_SCRIPTS_KEY = Key.create("cached.scripts");
    private static final Key<CachedValue<Map<String, String>>> OUR_CACHED_SHORT_COMPONENTS_REF_KEY = Key.create("cached.component.refs");

    private static final String SCRIPT_TAG_NAME = "Script";

    private static final UserDataCache<CachedValue<JSFile[]>, XmlTag, Object> ourCachedScripts =
        new UserDataCache<>() {
            @Override
            protected CachedValue<JSFile[]> compute(XmlTag tag, Object p) {
                return CachedValuesManager.getManager(tag.getProject()).createCachedValue(
                    () -> {
                        List<JSFile> injectedFiles = new ArrayList<>(2);
                        List<PsiElement> dependencies = new ArrayList<>();
                        dependencies.add(tag);
                        new InjectedScriptsVisitor(
                            tag,
                            doProcessAllTags(tag),
                            false,
                            false,
                            (rootTag, file) -> {
                                injectedFiles.add(file);
                                dependencies.add(file);
                            }
                        ).go();
                        return new CachedValueProvider.Result<>(
                            injectedFiles.toArray(new JSFile[injectedFiles.size()]),
                            dependencies.toArray()
                        );
                    },
                    false
                );
            }
        };

    private static final UserDataCache<CachedValue<Map<String, String>>, XmlFile, Object> myCachedComponentImportsCache = new
        UserDataCache<>() {
            @Override
            protected CachedValue<Map<String, String>> compute(final XmlFile file, final Object p) {
                return CachedValuesManager.getManager(file.getProject()).createCachedValue(
                    new CachedValueProvider<Map<String, String>>() {
                        @Override
                        public Result<Map<String, String>> compute() {
                            Map<String, String> cachedComponentImports = new HashMap<>();
                            List<PsiFile> dependencies = new ArrayList<>();
                            dependencies.add(file);

                            file.acceptChildren(new XmlRecursiveElementVisitor() {
                                @Override
                                @RequiredReadAction
                                public void visitXmlTag(XmlTag tag) {
                                    XmlElementDescriptor descriptor = tag.getDescriptor();
                                    if (descriptor != null) {
                                        PsiElement declaration = descriptor.getDeclaration();
                                        if (declaration instanceof XmlFile xmlFile) {
                                            declaration = XmlBackedJSClassImpl.getXmlBackedClass(xmlFile);
                                        }
                                        if (declaration instanceof JSClass jsClass) {
                                            cachedComponentImports.put(jsClass.getName(), jsClass.getQualifiedName());
                                            dependencies.add(declaration.getContainingFile());
                                        }
                                    }
                                    super.visitXmlTag(tag);
                                }
                            });
                            return new Result<>(cachedComponentImports, dependencies.toArray());
                        }
                    },
                    false
                );
            }
        };


    public static boolean doProcessAllTags(XmlTag rootTag) {
        return JSLanguageInjector.isMozillaXulOrXblNs(rootTag != null ? rootTag.getNamespace() : null);
    }

    public static boolean doProcessAllTags(XmlFile file) {
        return doProcessAllTags(getRootTag(file));
    }

    @Override
    @RequiredReadAction
    public boolean isValid() {
        return getNode().getPsi().isValid();
    }

    private static Key<ParameterizedCachedValue<XmlBackedJSClassImpl, XmlTag>> ourArtificialPsiKey = Key.create("xml.backed.class");

    @Nullable
    public static JSClass getXmlBackedClass(XmlFile xmlFile) {
        XmlTag rootTag = getRootTag(xmlFile);
        return rootTag != null ? getXmlBackedClass(rootTag) : null;
    }

    @Nullable
    private static XmlTag getRootTag(XmlFile xmlFile) {
        XmlDocument document = xmlFile.getDocument();
        return document != null ? document.getRootTag() : null;
    }

    public static XmlBackedJSClassImpl getXmlBackedClass(XmlTag tag) {
        return myCachedClassCache.get(ourArtificialPsiKey, tag, null).getValue(tag);
    }

    public static Collection<JSClass> getClasses(XmlFile file) {
        XmlTag rootTag = getRootTag(file);
        if (rootTag == null) {
            return Collections.emptyList();
        }
        Collection<JSClass> result = new ArrayList<>();
        result.add(getXmlBackedClass(rootTag));
        result.addAll(getChildInlineComponents(rootTag, true));
        return result;
    }

    public static XmlBackedJSClassImpl getContainingComponent(XmlElement element) {
        if (element instanceof XmlTag && isInlineComponentTag((XmlTag)element)) {
            return getXmlBackedClass((XmlTag)element);
        }
        XmlTag parentTag = PsiTreeUtil.getParentOfType(element, XmlTag.class);
        if (parentTag != null) {
            return getContainingComponent(parentTag);
        }

        if (!(element instanceof XmlTag)) {
            return null;
        }
        return getXmlBackedClass((XmlTag)element);
    }

    private static final UserDataCache<ParameterizedCachedValue<XmlBackedJSClassImpl, XmlTag>, XmlTag, Object> myCachedClassCache =
        new UserDataCache<>() {
            @Override
            protected ParameterizedCachedValue<XmlBackedJSClassImpl, XmlTag> compute(final XmlTag tag, final Object p) {
                return CachedValuesManager.getManager(tag.getProject()).createParameterizedCachedValue(
                    (ParameterizedCachedValueProvider<XmlBackedJSClassImpl, XmlTag>)tag1 ->
                        new CachedValueProvider.Result<>(new XmlBackedJSClassImpl(tag1), tag1),
                    false
                );
            }
        };

    @Override
    @RequiredReadAction
    public XmlTag getParent() {
        return (XmlTag)getNode().getPsi();
    }

    @Override
    public boolean isEquivalentTo(PsiElement element2) {
        return this == element2
            || element2 == getContainingFile()
            || (element2 instanceof XmlBackedJSClassImpl && getContainingFile() == element2.getContainingFile());
    }

    @Override
    @RequiredWriteAction
    public PsiElement add(@Nonnull PsiElement element) throws IncorrectOperationException {
        if (element instanceof JSFunction || element instanceof JSVarStatement) {
            JSFile jsFile = createOrGetFirstScriptTag();

            if (jsFile != null) {
                PsiElement child = jsFile.getLastChild();

                String text;
                if (child instanceof PsiWhiteSpace whiteSpace && (text = whiteSpace.getText()).indexOf("]]>") != -1) {
                    int cdataAt;
                    String marker = "<![CDATA[";

                    if ((cdataAt = text.indexOf(marker)) == -1) {
                        element = jsFile.addBefore(element, child);
                    }
                    else {
                        int markerEnd = cdataAt + marker.length();
                        ASTNode fromText = JSChangeUtil.createJSTreeFromText(
                            getProject(),
                            text.substring(0, markerEnd) + element.getText() + text.substring(markerEnd)
                        );
                        jsFile.getNode().replaceAllChildrenToChildrenOf(fromText.getTreeParent());
                        element = PsiTreeUtil.getParentOfType(jsFile.findElementAt(markerEnd + 1), element.getClass());
                    }
                }
                else {
                    element = jsFile.add(element);
                }

                CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(jsFile.getNode(), element.getNode());
                return element;
            }
        }

        return super.add(element);
    }

    @RequiredReadAction
    public JSFile createScriptTag() throws IncorrectOperationException {
        XmlTag rootTag = getParent();

        if (rootTag != null) {
            String ns = findScriptNs(rootTag);

            rootTag.add(rootTag.createChildTag(SCRIPT_TAG_NAME, ns, "<![CDATA[\n]]>", false));
            return findFirstScriptTag();
        }
        return null;
    }

    public static String findScriptNs(XmlTag rootTag) {
        String ns = rootTag.getNamespace();
        if (JavaScriptSupportLoader.isFlexMxmFile(rootTag.getContainingFile())) {
            ns = "";
            for (String testNs : JavaScriptSupportLoader.MXML_URIS) {
                if (rootTag.getPrefixByNamespace(testNs) != null) {
                    ns = testNs;
                    break;
                }
            }
        }
        return ns;
    }

    @Override
    @RequiredWriteAction
    public PsiElement addBefore(@Nonnull PsiElement element, PsiElement anchor) throws IncorrectOperationException {
        if (anchor == null) {
            return add(element);
        }
        return anchor.getParent().addBefore(element, anchor);
    }

    @Nullable
    @RequiredReadAction
    public JSFile findFirstScriptTag() {
        JSFile[] value = ourCachedScripts.get(CACHED_SCRIPTS_KEY, getParent(), null).getValue();
        if (value.length > 0) {
            return value[0];
        }
        return null;
    }

    @RequiredReadAction
    public JSFile createOrGetFirstScriptTag() throws IncorrectOperationException {
        JSFile jsFile = findFirstScriptTag();

        if (jsFile == null) {
            jsFile = createScriptTag();
        }

        return jsFile;
    }

    public static XmlTag[] findMxmlSubTags(XmlTag tag, String scriptTagName) {
        String ns = findScriptNs(tag);
        return tag.findSubTags(scriptTagName, ns);
    }

    public static void visitInjectedFiles(XmlFile file, InjectedFileVisitor visitor) {
        new InjectedScriptsVisitor(getRootTag(file), true, true, true, visitor).go();
    }

    public static boolean isInlineComponentTag(XmlTag tag) {
        return COMPONENT_TAG_NAME.equals(tag.getLocalName())
            && JavaScriptSupportLoader.isMxmlNs(tag.getNamespace())
            && !(tag.getParent() instanceof XmlDocument);
    }

    public static Collection<XmlBackedJSClassImpl> getChildInlineComponents(XmlTag rootTag, boolean recursive) {
        Collection<XmlBackedJSClassImpl> result = new ArrayList<>();
        rootTag.processElements(
            new PsiElementProcessor() {
                @Override
                public boolean execute(@Nonnull PsiElement element) {
                    if (element instanceof XmlTag tag) {
                        if (isInlineComponentTag(tag)) {
                            result.add(getXmlBackedClass(tag));
                            if (recursive) {
                                tag.processElements(this, null);
                            }
                        }
                        else {
                            tag.processElements(this, null);
                        }
                    }
                    return true;
                }
            },
            null
        );
        return result;
    }

    public interface InjectedFileVisitor {
        void visit(XmlTag rootTag, JSFile file);
    }

    public static class InjectedScriptsVisitor implements PsiElementProcessor {
        private final boolean myVisitAllTags;
        private final boolean myVisitAttributes;
        private final XmlTag myRootTag;
        private final boolean myVisitInnerComponents;
        private final InjectedFileVisitor myVisitor;

        public InjectedScriptsVisitor(
            XmlTag rootTag,
            boolean visitAllTags,
            boolean visitAttributes,
            boolean visitInnerComponents,
            InjectedFileVisitor visitor
        ) {
            myVisitAllTags = visitAllTags;
            myVisitAttributes = visitAttributes;
            myRootTag = rootTag;
            myVisitInnerComponents = visitInnerComponents;
            myVisitor = visitor;
        }

        public void go() {
            myRootTag.processElements(this, null);
        }

        @Override
        @RequiredReadAction
        public boolean execute(@Nonnull PsiElement element) {
            if (element instanceof XmlTag tag) {
                if (myVisitAllTags || SCRIPT_TAG_NAME.equals(tag.getLocalName())) {
                    String srcLocation = tag.getAttributeValue("source");
                    if (srcLocation != null) {
                        PsiReference ref = findFileReference(tag.getAttribute("source").getValueElement());
                        if (ref != null && ref.resolve() instanceof JSFile jsFile) {
                            jsFile.putUserData(JSResolveUtil.contextKey, tag);
                            myVisitor.visit(myRootTag, jsFile);
                        }
                    }
                    else {
                        JSResolveUtil.processInjectedFileForTag(
                            tag,
                            new JSResolveUtil.JSInjectedFilesVisitor() {
                                @Override
                                protected void process(JSFile file) {
                                    myVisitor.visit(myRootTag, file);
                                }
                            }
                        );
                    }
                }
                if (isInlineComponentTag(tag)) {
                    if (myVisitInnerComponents) {
                        new InjectedScriptsVisitor(tag, myVisitAllTags, myVisitAttributes, true, myVisitor).go();
                    }
                }
                else {
                    tag.processElements(this, null);
                }
            }
            if (myVisitAttributes && element instanceof XmlAttribute) {
                XmlAttributeValue value = ((XmlAttribute)element).getValueElement();
                if (value != null) {
                    InjectedLanguageManager.getInstance(element.getProject()).enumerate(
                        value,
                        (injectedPsi, places) -> {
                            if (places.get(0).getHost() instanceof XmlAttributeValue) {
                                myVisitor.visit(myRootTag, (JSFile)injectedPsi);
                            }
                        }
                    );
                }
            }
            return true;
        }

        @Nullable
        private static PsiReference findFileReference(XmlAttributeValue valueElement) {
            if (valueElement == null) {
                return null;
            }
            PsiReference[] references = valueElement.getReferences();
            if (references.length > 0) {
                return references[references.length - 1];
            }
            return null;
        }
    }
}
