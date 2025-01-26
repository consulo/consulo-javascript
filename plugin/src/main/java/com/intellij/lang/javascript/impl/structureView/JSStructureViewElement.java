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

package com.intellij.lang.javascript.impl.structureView;

import consulo.annotation.access.RequiredReadAction;
import consulo.document.util.TextRange;
import consulo.fileEditor.structureView.StructureViewTreeElement;
import consulo.language.inject.InjectedLanguageManager;
import com.intellij.lang.javascript.index.JSSymbolUtil;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.navigation.Navigatable;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.javascript.psi.JavaScriptImportStatementBase;
import consulo.navigation.ItemPresentation;
import consulo.util.collection.primitive.ints.IntMaps;
import consulo.util.collection.primitive.ints.IntObjectMap;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.*;

/**
 * @author max
 */
public class JSStructureViewElement implements StructureViewTreeElement {
    protected PsiElement myElement;
    private boolean myInherited;

    public JSStructureViewElement(PsiElement element) {
        myElement = element;
    }

    @Override
    public PsiElement getValue() {
        return myElement;
    }

    PsiElement getRealElement() {
        return myElement;
    }

    @Override
    public void navigate(boolean requestFocus) {
        ((Navigatable)myElement).navigate(requestFocus);
    }

    @Override
    public boolean canNavigate() {
        return ((Navigatable)myElement).canNavigate();
    }

    @Override
    public boolean canNavigateToSource() {
        return canNavigate();
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public StructureViewTreeElement[] getChildren() {
        PsiElement element = getUpToDateElement();
        // since we use proxies for StructureViewElement then real element may invalidate due to structural change
        if (element == null) {
            return EMPTY_ARRAY;
        }

        Set<String> referencedNamedIds = new HashSet<>();

        List<StructureViewTreeElement> children = collectMyElements(referencedNamedIds);

        ArrayList<StructureViewTreeElement> elementsFromSupers = null;

        if (element instanceof JSClass jsClass) {
            for (JSClass superClass : jsClass.getSuperClasses()) {
                StructureViewTreeElement[] structureViewTreeElements = createStructureViewElement(superClass).getChildren();

                if (elementsFromSupers == null) {
                    elementsFromSupers = new ArrayList<>(structureViewTreeElements.length);
                }
                else {
                    elementsFromSupers.ensureCapacity(elementsFromSupers.size() + structureViewTreeElements.length);
                }

                for (StructureViewTreeElement e : structureViewTreeElements) {
                    if (!isVisible((PsiElement)e.getValue(), element)) {
                        continue;
                    }
                    // TODO CSS styles can also be inherited, so we better show them
                    // This can't be done until 'inherited' property is elevated up to StructureViewTreeElement interface
                    if (!(e instanceof JSStructureViewElement)) {
                        continue;
                    }
                    ((JSStructureViewElement)e).setInherited(true);
                    elementsFromSupers.add(e);
                }
            }
        }

        Collections.sort(
            children,
            (_o1, _o2) -> {
                PsiElement e = getPsiElement(_o1);
                PsiElement e2 = getPsiElement(_o2);

                TextRange t1 = InjectedLanguageManager.getInstance(e.getProject()).injectedToHost(e, e.getTextRange());
                TextRange t2 = InjectedLanguageManager.getInstance(e.getProject()).injectedToHost(e, e.getTextRange());
                final int offset = e.getTextOffset() + t1.getStartOffset();
                final int offset2 = e2.getTextOffset() + t2.getStartOffset();

                return offset - offset2;
            }
        );

        if (elementsFromSupers != null) {
            Map<String, JSFunction> functionsByNames = new HashMap<>();
            for (StructureViewTreeElement child : children) {
                PsiElement el = getPsiElementResolveProxy(child);
                if (el instanceof JSFunction function) {
                    functionsByNames.put(function.getName(), function);
                }
            }

            for (StructureViewTreeElement superTreeElement : elementsFromSupers) {
                PsiElement superElement = getPsiElementResolveProxy(superTreeElement);
                boolean addSuperElement = true;
                if (superElement instanceof JSFunction superFunction) {
                    JSFunction function = functionsByNames.get(superFunction.getName());
                    if (function != null) {
                        // TODO check signature
                        addSuperElement = false;
                    }
                }

                if (addSuperElement) {
                    children.add(superTreeElement);
                }
            }
        }
        return children.toArray(new StructureViewTreeElement[children.size()]);
    }

    protected JSStructureViewElement createStructureViewElement(PsiElement element) {
        return new JSStructureViewElement(element);
    }

    private static PsiElement getPsiElement(StructureViewTreeElement element) {
        return element instanceof JSStructureViewElement structureViewElement
            ? structureViewElement.myElement
            : (PsiElement)element.getValue();
    }

    public static PsiElement getPsiElementResolveProxy(StructureViewTreeElement element) {
        return getPsiElement(element);
    }

    protected List<StructureViewTreeElement> collectMyElements(Set<String> referencedNamedIds) {
        IntObjectMap<PsiElement> offset2Element = IntMaps.newIntObjectHashMap();

        collectChildrenFromElement(myElement, referencedNamedIds, offset2Element);

        List<StructureViewTreeElement> children = new ArrayList<>(offset2Element.size());
        offset2Element.forEach((textOffset, element) -> children.add(createStructureViewElement(element)));
        return children;
    }

    @RequiredReadAction
    private static boolean isVisible(PsiElement namedElement, PsiElement element) {
        if (namedElement instanceof JSAttributeListOwner attributeListOwner) {
            JSAttributeList attributeList = attributeListOwner.getAttributeList();

            if (attributeList != null) {
                JSAttributeList.AccessType type = attributeList.getAccessType();

                if (type == JSAttributeList.AccessType.PACKAGE_LOCAL) {
                    JSPackageStatement packageStatement = PsiTreeUtil.getParentOfType(namedElement, JSPackageStatement.class);
                    JSPackageStatement packageStatement2 = PsiTreeUtil.getParentOfType(element, JSPackageStatement.class);

                    return packageStatement == packageStatement2
                        || (packageStatement != null && packageStatement2 != null
                        && Objects.equals(packageStatement.getQualifiedName(), packageStatement2.getQualifiedName()));
                }
                return type != JSAttributeList.AccessType.PRIVATE;
            }
        }
        return true;
    }

    private static void collectChildrenFromElement(
        PsiElement element,
        Set<String> referencedNamedIds,
        IntObjectMap<PsiElement> offset2Element
    ) {
        element.acceptChildren(new JSElementVisitor() {
            Set<PsiFile> visited;
            PsiElement context = element;

            @Override
            @RequiredReadAction
            public void visitElement(PsiElement element) {
                if (element instanceof JSNamedElement namedElement
                    && namedElement.getName() != null
                    && !(element instanceof JSDefinitionExpression)
                    && !(element instanceof JSLabeledStatement)
                    && !(element instanceof JSPackageStatement)
                    && !(element instanceof JavaScriptImportStatementBase)) {
                    if (!(element instanceof JSFunction) || !(element.getParent() instanceof JSProperty)) {
                        addElement(element);
                    }
                    else {
                        element.acceptChildren(this);
                    }
                }
                else {
                    element.acceptChildren(this);
                }
            }

            @Override
            public void visitJSParameter(@Nonnull JSParameter node) {
                // Do not add parameters to structure view
            }

            @Override
            @RequiredReadAction
            public void visitJSIncludeDirective(@Nonnull JSIncludeDirective includeDirective) {
                if (includeDirective.resolveFile() instanceof JSFile jsFile) {
                    if (visited != null && visited.contains(jsFile)) {
                        return;
                    }
                    if (visited == null) {
                        visited = new HashSet<>();
                    }
                    visited.add(jsFile);

                    PsiElement prevContext = context;
                    context = jsFile;
                    context.putUserData(JSResolveUtil.contextKey, element);
                    jsFile.acceptChildren(this);
                    context = prevContext;
                }
            }

            @Override
            public void visitJSObjectLiteralExpression(@Nonnull JSObjectLiteralExpression node) {
                PsiElement parent = node.getParent();
                if (parent instanceof JSVariable
                    || parent instanceof JSProperty
                    || parent instanceof JSFile
                    || parent instanceof JSReturnStatement
                    || parent instanceof JSAssignmentExpression) {
                    node.acceptChildren(this);
                    return;
                }
                if (parent instanceof JSArgumentList argumentList) {
                    JSElement expression = JSSymbolUtil.findQualifyingExpressionFromArgumentList(argumentList);
                    if (expression != null) {
                        return;
                    }
                }

                if (node.getProperties().length > 0) {
                    addElement(node);
                }
            }

            @Override
            public void visitJSVariable(@Nonnull JSVariable node) {
                if (element instanceof JSFunction) {
                    return;
                }
                super.visitJSVariable(node);
            }

            @Override
            @RequiredReadAction
            public void visitJSAssignmentExpression(@Nonnull JSAssignmentExpression node) {
                JSExpression rOperand = node.getROperand();
                JSExpression lOperand = node.getLOperand();

                boolean outsideFunction = PsiTreeUtil.getParentOfType(node, JSFunction.class) == null;
                if (!outsideFunction) {
                    return;
                }

                if (rOperand instanceof JSCallExpression call) {
                    rOperand = call.getMethodExpression();
                }

                if (rOperand instanceof JSFunction function) {
                    JSExpression qualifier = null;
                    JSExpression operand = ((JSDefinitionExpression)lOperand).getExpression();

                    if (operand instanceof JSReferenceExpression operandRefExpr) {
                        qualifier = operandRefExpr.getQualifier();
                    }

                    if (qualifier == null || qualifier instanceof JSThisExpression) {
                        addElement(function);
                    }
                }
                else if (lOperand != null) {
                    JSExpression operand = ((JSDefinitionExpression)lOperand).getExpression();
                    if (operand instanceof JSReferenceExpression operandRefExpr
                        && operandRefExpr.getQualifier() instanceof JSThisExpression) {
                        PsiElement resolved = operandRefExpr.resolve();
                        if (resolved == lOperand) {
                            addElement(lOperand);
                        }
                    }
                    //super.visitJSAssignmentExpression(node);
                }
            }

            @RequiredReadAction
            private void addElement(PsiElement lOperand) {
                if (lOperand instanceof JSNamedElement namedElement) {
                    String namedId = namedElement.getName();
                    if (referencedNamedIds.contains(namedId)) {
                        return;
                    }
                    referencedNamedIds.add(namedId);
                }
                offset2Element.put(lOperand.getTextOffset(), lOperand);
            }
        });
    }

    @Override
    public ItemPresentation getPresentation() {
        return new JSStructureItemPresentation(this);
    }

    public boolean isInherited() {
        return myInherited;
    }

    private void setInherited(boolean b) {
        myInherited = b;
    }

    public
    @Nullable
    PsiElement getUpToDateElement() {
        boolean isValid = myElement.isValid();

        if (!isValid) {
            return null;
        }

        return myElement;
    }

    static abstract class JSStructureItemPresentationBase implements ItemPresentation {
        final protected JSStructureViewElement element;

        JSStructureItemPresentationBase(JSStructureViewElement _element) {
            element = _element;
        }

        @Override
        public String getLocationString() {
            return null;
        }
    }
}
