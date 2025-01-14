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

import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.application.AllIcons;
import consulo.codeEditor.Editor;
import consulo.fileEditor.structureView.StructureViewTreeElement;
import consulo.fileEditor.structureView.tree.*;
import consulo.ide.localize.IdeLocalize;
import consulo.language.editor.structureView.TextEditorBasedStructureViewModel;
import consulo.language.inject.InjectedLanguageManager;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiLanguageInjectionHost;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.platform.base.icon.PlatformIconGroup;
import consulo.ui.ex.action.Shortcut;
import consulo.ui.ex.keymap.KeymapManager;
import consulo.util.lang.ref.SimpleReference;
import jakarta.annotation.Nonnull;

import java.util.Comparator;

/**
 * @author max
 * @author maxim
 */
public class JSStructureViewModel extends TextEditorBasedStructureViewModel {
    private PsiElement myRoot;
    private Filter[] myFilters = new Filter[]{
        ourFieldsFilter,
        INHERITED_FILTER
    };

    private static final String ID = "KIND";
    private static Sorter ourKindSorter = new Sorter() {
        private Comparator myComparator = new Comparator() {
            @Override
            @RequiredReadAction
            public int compare(Object o, Object o2) {
                return getWeight(o) - getWeight(o2);
            }

            @RequiredReadAction
            private int getWeight(Object s) {
                if (s instanceof JSSuperGroup) {
                    return 5;
                }
                Object o = ((StructureViewTreeElement)s).getValue();

                if (o instanceof JSProperty property && property.getValue() instanceof JSFunction function) {
                    o = function;
                }

                if (o instanceof JSClass) {
                    return 7;
                }
                return 30;
            }
        };

        @Override
        public Comparator getComparator() {
            return myComparator;
        }

        @Override
        public boolean isVisible() {
            return false;
        }

        @Override
        @Nonnull
        public ActionPresentation getPresentation() {
            return null; // will not be shown
        }

        @Override
        @Nonnull
        public String getName() {
            return ID;
        }
    };

    private Sorter[] mySorters = new Sorter[]{
        ourKindSorter,
        Sorter.ALPHA_SORTER
    };

    private static Filter ourFieldsFilter = new Filter() {
        public static final String ID = "SHOW_FIELDS";

        @Override
        @RequiredReadAction
        public boolean isVisible(TreeElement treeNode) {
            if (treeNode instanceof JSStructureViewElement structureViewElement) {
                PsiElement element = structureViewElement.getRealElement();
                return element instanceof JSClass || element instanceof JSFunction
                    || (element instanceof JSProperty property && property.getValue() instanceof JSFunction)
                    || element instanceof JSObjectLiteralExpression;
            }
            return true;
        }

        @Override
        public boolean isReverted() {
            return true;
        }

        @Override
        @Nonnull
        public ActionPresentation getPresentation() {
            return new ActionPresentationData(IdeLocalize.actionStructureviewShowFields().get(), null, AllIcons.Nodes.Variable);
        }

        @Override
        @Nonnull
        public String getName() {
            return ID;
        }
    };

    private static final Filter INHERITED_FILTER = new FileStructureFilter() {
        public static final String ID = "SHOW_INHERITED";

        @Override
        public boolean isVisible(TreeElement treeNode) {
            return !(treeNode instanceof JSStructureViewElement structureViewElement && structureViewElement.isInherited());
        }

        @Override
        @Nonnull
        public ActionPresentation getPresentation() {
            return new ActionPresentationData(
                IdeLocalize.actionStructureviewShowInherited().get(),
                null,
                PlatformIconGroup.hierarchySupertypes()
            );
        }

        @Override
        @Nonnull
        public String getName() {
            return ID;
        }

        @Override
        public boolean isReverted() {
            return true;
        }

        @Override
        public String getCheckBoxText() {
            return IdeLocalize.fileStructureToggleShowInherited().get();
        }

        @Override
        public Shortcut[] getShortcut() {
            return KeymapManager.getInstance().getActiveKeymap().getShortcuts("FileStructurePopup");
        }
    };

    private static final Grouper INHERITED_GROUPER = new JSSuperGrouper();

    private static final Class[] SUITABLE_CLASSES = new Class[]{
        JSFunction.class,
        JSVariable.class,
        JSDefinitionExpression.class,
        JSClass.class,
        JSProperty.class
    };

    private Grouper[] myGroupers = new Grouper[]{INHERITED_GROUPER};


    public JSStructureViewModel(PsiElement root) {
        super(root.getContainingFile());
        myRoot = root;
    }

    public JSStructureViewModel(PsiElement root, Editor editor) {
        super(editor);
        myRoot = root;
    }

    @Override
    @Nonnull
    public StructureViewTreeElement getRoot() {
        return new JSStructureViewElement(myRoot);
    }

    @Override
    @Nonnull
    public Grouper[] getGroupers() {
        return myGroupers;
    }

    @Override
    @Nonnull
    public Sorter[] getSorters() {
        return mySorters;
    }

    @Override
    @Nonnull
    public Filter[] getFilters() {
        return myFilters;
    }

    @Override
    protected boolean isSuitable(PsiElement element) {
        return super.isSuitable(element)
            && !(element instanceof JSVariable variable && PsiTreeUtil.getParentOfType(variable, JSFunction.class) != null);
    }

    @Override
    @RequiredReadAction
    public Object getCurrentEditorElement() {
        Object editorElement = super.getCurrentEditorElement();

        PsiFile file = getPsiFile();
        if (editorElement == null && !(file instanceof JSFile)) {
            final int offset = getEditor().getCaretModel().getOffset();
            PsiElement at = file.findElementAt(offset);
            PsiLanguageInjectionHost injectionHost = PsiTreeUtil.getParentOfType(at, PsiLanguageInjectionHost.class);

            if (injectionHost != null) {
                final SimpleReference<PsiElement> ref = new SimpleReference<>();
                InjectedLanguageManager.getInstance(file.getProject()).enumerate(
                    injectionHost,
                    (injectedPsi, places) -> {
                        PsiLanguageInjectionHost.Shred shred = places.get(0);
                        int injectedStart = shred.getRangeInsideHost().getStartOffset() + shred.getHost().getTextOffset();
                        int offsetInInjected = offset - injectedStart;

                        ref.set(injectedPsi.findElementAt(offsetInInjected));
                    }
                );

                PsiElement element = ref.get();
                if (element != null) {
                    editorElement = findAcceptableElement(element);
                    return editorElement;
                }
            }
        }

        if (editorElement instanceof JSDefinitionExpression definition
            && definition.getParent() instanceof JSAssignmentExpression assignment
            && assignment.getROperand() instanceof JSFunctionExpression functionExpr) {
            editorElement = functionExpr;
        }

        if (editorElement instanceof JSNamedElement namedElement) {
            PsiFile containingFile = namedElement.getContainingFile();
            PsiElement context = containingFile.getContext();
            int offset = namedElement.getTextOffset();
            PsiElement element;

            if (context != null) {
                element = JavaScriptIndex.findSymbolWithNameAndOffsetInEntry(namedElement.getName(), offset);
            }
            else {
                element = JavaScriptIndex.findSymbolByFileAndNameAndOffset(
                    containingFile.getVirtualFile().getPath(),
                    namedElement.getName(),
                    offset
                );
            }

            if (element != null) {
                editorElement = element;
            }
        }
        return editorElement;
    }

    @Override
    protected PsiFile getPsiFile() {
        return myRoot.getContainingFile();
    }

    @Nonnull
    @Override
    protected Class[] getSuitableClasses() {
        return SUITABLE_CLASSES;
    }

    public void setFilters(Filter[] filters) {
        myFilters = filters;
    }

    public void setGroupers(Grouper[] groupers) {
        myGroupers = groupers;
    }

    public void setSorters(Sorter[] sorters) {
        mySorters = sorters;
    }

    @Override
    public boolean shouldEnterElement(Object element) {
        return shouldEnterElementStatic(element);
    }

    public static boolean shouldEnterElementStatic(Object element) {
        return element instanceof JSClass;
    }
}
