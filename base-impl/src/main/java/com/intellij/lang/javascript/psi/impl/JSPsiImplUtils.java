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

import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.content.base.SourcesOrderRootType;
import consulo.javascript.lang.psi.impl.elementType.BaseJavaScriptElementType;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import consulo.javascript.psi.JavaScriptImportStatementBase;
import consulo.language.ast.ASTNode;
import consulo.language.psi.*;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.StubIndex;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.module.content.ProjectFileIndex;
import consulo.module.content.ProjectRootManager;
import consulo.module.content.layer.orderEntry.OrderEntry;
import consulo.project.DumbService;
import consulo.project.Project;
import consulo.util.collection.HashingStrategy;
import consulo.util.lang.StringUtil;
import consulo.virtualFileSystem.VirtualFile;
import consulo.xml.psi.xml.XmlFile;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * @author Maxim.Mossienko
 * @since 2008-06-08
 */
public class JSPsiImplUtils {
    private static final String ARRAY_TYPE_NAME = "Array";
    private static final String ARRAY_ELEMENT_TYPE_ANNOTATION_NAME = "ArrayElementType";

    @Nullable
    @RequiredReadAction
    public static JavaScriptTypeElement findTypeElement(@Nonnull PsiElement element) {
        if (element instanceof StubBasedPsiElement stubBasedPsiElement) {
            StubElement<?> stub = stubBasedPsiElement.getStub();
            if (stub != null) {
                List<StubElement> childrenStubs = stub.getChildrenStubs();
                for (StubElement childrenStub : childrenStubs) {
                    IStubElementType stubType = childrenStub.getStubType();
                    if (stubType instanceof BaseJavaScriptElementType) {
                        return (JavaScriptTypeElement)stub.getPsi();
                    }
                }
            }
        }

        return findChildrenByClass(element, JavaScriptTypeElement.class);
    }

    @Nullable
    @RequiredReadAction
    @SuppressWarnings("unchecked")
    private static <T> T findChildrenByClass(@Nonnull PsiElement element, Class<T> aClass) {
        for (PsiElement cur = element.getFirstChild(); cur != null; cur = cur.getNextSibling()) {
            if (aClass.isInstance(cur)) {
                return (T)cur;
            }
        }
        return null;
    }

    @Deprecated
    public static ASTNode getTypeExpressionFromDeclaration(JSNamedElement element) {
        ASTNode myNode = element.getNode();
        ASTNode node = myNode != null ? myNode.findChildByType(JSTokenTypes.COLON) : null;

        return node != null ? myNode.findChildByType(JSDocumentationUtils.TYPE_FILTER, node) : null;
    }

    @Deprecated
    @RequiredReadAction
    public static String getTypeFromDeclaration(JSNamedElement element) {
        ASTNode typeExpr = getTypeExpressionFromDeclaration(element);

        String s = null;
        if (typeExpr != null) {
            s = typeExpr.getText();
        }
        else if (element instanceof JSParameter parameter && parameter.isRest()) {
            s = ARRAY_TYPE_NAME;
        }

        if (ARRAY_TYPE_NAME.equals(s)) {
            PsiComment psiComment = typeExpr != null ? PsiTreeUtil.getPrevSiblingOfType(typeExpr.getPsi(), PsiComment.class) : null;

            if (psiComment != null) {
                String elementType = JSDocumentationUtils.unwrapCommentDelimiters(psiComment.getText()).trim();

                if (elementType.length() > 0) {
                    return s + "[" + elementType;
                }
            }

            if (element instanceof JSAttributeListOwner attributeListOwner) {
                JSAttributeList attributeList = attributeListOwner.getAttributeList();

                if (attributeList != null) {
                    String type = getArrayElementTypeFromAnnotation(attributeList);
                    if (type != null && type.length() > 0) {
                        return s + "[" + type;
                    }
                }
            }

        }

        return s;
    }

    @Nullable
    @RequiredReadAction
    public static String getArrayElementTypeFromAnnotation(JSAttributeList attributeList) {
        return getTypeFromAnnotationParameter(attributeList, ARRAY_ELEMENT_TYPE_ANNOTATION_NAME, null);
    }

    @RequiredReadAction
    public static String getType(JSNamedElement element) {
        String typeFromDeclaration = getTypeFromDeclaration(element);
        if (typeFromDeclaration != null) {
            return typeFromDeclaration;
        }

        return JSDocumentationUtils.findTypeFromComments(element);
    }

    @RequiredWriteAction
    public static void updateFileName(JSQualifiedNamedElement jsClassBase, String newName, String oldName)
        throws IncorrectOperationException {

        PsiFile containingFile = jsClassBase.getContainingFile();

        if (containingFile.getContext() == null) {
            VirtualFile virtualFile = containingFile.getVirtualFile();

            if (virtualFile != null && virtualFile.getNameWithoutExtension().equals(oldName)) {
                String s = containingFile.getName();
                containingFile.setName(newName + "." + s.substring(s.lastIndexOf('.') + 1));
            }
        }
    }

    @Nullable
    @RequiredReadAction
    public static JSPackageStatement findPackageStatement(JSFile file) {
        JSPackageStatement packageStatement = null;

        for (PsiElement statement : file.getChildren()) {
            if (statement instanceof JSPackageStatement packageStmt) {
                packageStatement = packageStmt;
                break;
            }
        }
        return packageStatement;
    }

    @Nonnull
    @RequiredReadAction
    static PsiElement findTopLevelNavigatableElement(@Nonnull JSQualifiedNamedElement jsClass) {
        PsiElement sourceElement = findTopLevelNavigatableElementWithSource(jsClass, null);
        if (sourceElement != null) {
            return sourceElement;
        }
        return jsClass;
    }

    @Nullable
    @RequiredReadAction
    public static PsiElement findTopLevelNavigatableElementWithSource(
        @Nonnull JSQualifiedNamedElement jsClass,
        @Nullable Consumer<JSQualifiedNamedElement> candidatesConsumer
    ) {
        if (candidatesConsumer != null) {
            candidatesConsumer.accept(jsClass);
        }

        PsiElement sourceElement = findSourceElement(jsClass);
        if (sourceElement != null) {
            return sourceElement;
        }

        if (DumbService.isDumb(jsClass.getProject())) {
            return null;
        }

        GlobalSearchScope searchScope = jsClass.getResolveScope();
        String qName = jsClass.getQualifiedName();
        Collection<JSQualifiedNamedElement> candidates = StubIndex.getElements(
            JavaScriptIndexKeys.ELEMENTS_BY_QNAME,
            qName,
            jsClass.getProject(),
            searchScope,
            JSQualifiedNamedElement.class
        );
        for (Iterator<JSQualifiedNamedElement> i = candidates.iterator(); i.hasNext(); ) {
            if (!qName.equals(i.next().getQualifiedName())) {
                i.remove();
            }
        }

        for (JSQualifiedNamedElement candidate : candidates) {
            if (candidate == jsClass) {
                continue;
            }

            if (candidatesConsumer != null) {
                candidatesConsumer.accept(candidate);
            }

            PsiElement candidateSourceElement = findSourceElement(candidate);
            if (candidateSourceElement != null) {
                return candidateSourceElement;
            }
        }
        return null;
    }

    @Nullable
    @RequiredReadAction
    private static PsiElement findSourceElement(JSQualifiedNamedElement jsClass) {
        PsiFile containingFile = jsClass.getContainingFile();
        if (containingFile == null) {
            return null;
        }

        VirtualFile vFile = containingFile.getVirtualFile();
        ProjectFileIndex projectFileIndex = ProjectRootManager.getInstance(jsClass.getProject()).getFileIndex();
        if (vFile == null || projectFileIndex.getClassRootForFile(vFile) == null) {
            return null;
        }

        List<OrderEntry> orderEntries = projectFileIndex.getOrderEntriesForFile(vFile);

        String qName = jsClass.getQualifiedName();
        String baseSourceName = jsClass.getName();
        int index = qName != null ? qName.lastIndexOf('.') : -1;
        String packageName = index != -1 ? qName.substring(0, index) : "";
        String relativeFilePath = packageName.length() == 0 ? baseSourceName : packageName.replace('.', '/') + '/' + baseSourceName;
        String relativeFilePath2 = relativeFilePath + ".mxml";
        String relativeFilePath3 = relativeFilePath + ".mxm";
        relativeFilePath += ".as";

        for (OrderEntry orderEntry : orderEntries) {
            VirtualFile[] files = orderEntry.getFiles(SourcesOrderRootType.getInstance());

            for (VirtualFile file : files) {
                VirtualFile source = file.findFileByRelativePath(relativeFilePath);

                if (source != null) {
                    PsiFile psiSource = jsClass.getManager().findFile(source);

                    if (psiSource instanceof JSFile jsFile) {
                        JSPackageStatement statement = findPackageStatement(jsFile);

                        if (statement != null) {
                            for (JSSourceElement el : statement.getStatements()) {
                                if (el.getClass() == jsClass.getClass() && jsClass.getName().equals(el.getName())) {
                                    return el;
                                }
                            }
                        }
                        return psiSource;
                    }
                }
                else {
                    source = file.findFileByRelativePath(relativeFilePath2);
                    if (source == null) {
                        source = file.findFileByRelativePath(relativeFilePath3);
                    }
                    if (source != null) {
                        PsiFile psiSource = jsClass.getManager().findFile(source);
                        if (psiSource instanceof XmlFile xmlFile) {
                            return XmlBackedJSClassImpl.getXmlBackedClass(xmlFile);
                        }
                    }
                }
            }
        }
        return null;
    }

    @RequiredReadAction
    static String getQName(JSNamedElement element) {
        PsiElement node = element.getNameIdentifier();
        String name = node != null ? node.getText() : null;
        PsiElement responsibleParent = element.getParent();

        if (responsibleParent instanceof JSVarStatement varStatement) {
            responsibleParent = varStatement.getParent();
        }

        if (responsibleParent instanceof JSPackageStatement packageStatement && name != null) {
            String packageName = packageStatement.getQualifiedName();
            if (!StringUtil.isEmpty(packageName)) {
                return packageName.concat(".").concat(name);
            }
        }
        return name;
    }

    @Nullable
    @RequiredReadAction
    public static String getTypeFromAnnotationParameter(
        @Nonnull JSAttributeList attributeList,
        @Nonnull String annotationName,
        @Nullable String annotationParameter
    ) {
        String arrayType = null;
        JSAttribute[] byName = attributeList.getAttributesByName(annotationName);
        if (byName.length > 0) {
            JSAttributeNameValuePair jsAttributeNameValuePair = byName[0].getValueByName(annotationParameter);
            arrayType = jsAttributeNameValuePair != null ? jsAttributeNameValuePair.getSimpleValue() : null;
        }
        return arrayType;
    }

    /**
     * @see <code>QUALIFIED_NAME_HASHING_STRATEGY</code>
     */
    @RequiredReadAction
    static boolean isTheSameClass(PsiElement typeSource, JSClass jsClass) {
        if (typeSource == jsClass) {
            return true;
        }
        if (typeSource instanceof JSClass typeSourceClass && jsClass != null) {
            String qName = typeSourceClass.getQualifiedName();
            return qName != null && qName.equals(jsClass.getQualifiedName());
        }
        return false;
    }

    public static final HashingStrategy<JSQualifiedNamedElement> QUALIFIED_NAME_HASHING_STRATEGY = new HashingStrategy<>() {
        @Override
        @RequiredReadAction
        public int hashCode(JSQualifiedNamedElement object) {
            return object == null || object.getQualifiedName() == null ? 0 : object.getQualifiedName().hashCode();
        }

        @Override
        @RequiredReadAction
        public boolean equals(JSQualifiedNamedElement o1, JSQualifiedNamedElement o2) {
            return Objects.equals(o1.getQualifiedName(), o2.getQualifiedName());
        }
    };

    static void doRenameParentDirectoryIfNeeded(VirtualFile file, String name, Object requestor) throws IOException {
        VirtualFile directory = file.isDirectory() ? file : file.getParent();
        if (!name.equals(directory.getName())) {
            directory.rename(requestor, name);
        }
    }

    @Nullable
    @RequiredReadAction
    static String getQNameForMove(@Nonnull PsiElement targetElement, PsiElement elementToBind) {
        String qName = null;
        Project project = targetElement.getProject();

        if (elementToBind instanceof PsiFile file) {
            String newName = file.getName();
            int index = newName.lastIndexOf('.');
            if (index != -1) {
                newName = newName.substring(0, index);
            }
            VirtualFile elementToBindFile = file.getContainingFile().getVirtualFile();

            String packageName = JSResolveUtil.getExpectedPackageNameFromFile(elementToBindFile, project, false);

            if (targetElement instanceof JSReferenceExpression targetRefExpr) {
                JSExpression qualifier = targetRefExpr.getQualifier();
                String targetElementPackageName = qualifier != null
                    ? qualifier.getText()
                    : JSResolveUtil.getExpectedPackageNameFromFile(targetRefExpr.getContainingFile().getVirtualFile(), project, false);
                if (!differentPackageName(targetElementPackageName, packageName)) {
                    return null;
                }
                if (qualifier == null && !(targetRefExpr.getParent() instanceof JavaScriptImportStatementBase)) {
                    return null;
                }
            }

            qName = packageName.isEmpty() ? newName : packageName + "." + newName;
        }
        else if (elementToBind instanceof PsiDirectoryContainer directoryContainer) {
            PsiDirectory[] directories = directoryContainer.getDirectories(targetElement.getResolveScope());
            if (directories.length > 0) {
                qName = JSResolveUtil.getExpectedPackageNameFromFile(directories[0].getVirtualFile(), project, false);
            }
        }
        return qName;
    }

    public static boolean differentPackageName(String s, String expectedPackageNameFromFile) {
        boolean sIsEmpty = isEmpty(s);
        boolean expectedIsEmpty = isEmpty(expectedPackageNameFromFile);

        return (sIsEmpty && !expectedIsEmpty) || (!sIsEmpty && (expectedIsEmpty || !s.equals(expectedPackageNameFromFile)));
    }

    public static boolean isEmpty(String expectedPackageNameFromFile) {
        return expectedPackageNameFromFile == null || expectedPackageNameFromFile.isEmpty();
    }
}
