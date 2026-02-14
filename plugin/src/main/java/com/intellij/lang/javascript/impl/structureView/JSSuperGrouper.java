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

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.fileEditor.structureView.tree.*;
import consulo.ide.localize.IdeLocalize;
import consulo.language.psi.PsiElement;
import consulo.platform.base.icon.PlatformIconGroup;
import consulo.project.ui.view.tree.AbstractTreeNode;
import jakarta.annotation.Nonnull;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Maxim.Mossienko
 */
class JSSuperGrouper implements Grouper {
    private static final String SHOW_CLASSES = "SHOW_CLASSES";

    @Nonnull
    @Override
    @RequiredReadAction
    public Collection<Group> group(Object parent, Collection<TreeElement> children) {
        AbstractTreeNode parentNode = (AbstractTreeNode)parent;
        if (isParentGrouped(parentNode)) {
            return Collections.emptyList();
        }
        Map<String, Group> groups = new HashMap<>();

        for (TreeElement _child : children) {
            if (!(_child instanceof JSStructureViewElement)) {
                continue;
            }
            JSStructureViewElement child = (JSStructureViewElement)_child;
            PsiElement value = child.getValue();

            if (value instanceof JSVariable variable) {
                if (!child.isInherited()) {
                    continue;
                }
                PsiElement parentElement = variable.getParent();
                if (parentElement instanceof JSVarStatement varStatement) {
                    parentElement = varStatement.getParent();
                }
                if (parentElement instanceof JSClass jsClass) {
                    addGroup(groups, _child, jsClass.getQualifiedName());
                }
            }
            else if (value instanceof JSFunction function) {
                processFunction((JSStructureViewElement)parentNode.getValue(), groups, _child, function);
            }
        }
        return groups.values();
    }

    @RequiredReadAction
    private static void processFunction(
        JSStructureViewElement parentElement,
        Map<String, Group> groups,
        TreeElement _child,
        PsiElement value
    ) {
        PsiElement element = JSStructureViewElement.getPsiElementResolveProxy(parentElement);
        if (element instanceof JSClass parentClass) {
            JSClass declaringClass = JSResolveUtil.findDeclaringClass((JSFunction)value);
            if (parentClass != declaringClass) {
                addGroup(groups, _child, declaringClass.getQualifiedName());
            }
        }
    }

    private static void addGroup(Map<String, Group> groups, TreeElement _child, String qName) {
        JSSuperGroup group = (JSSuperGroup)groups.get(qName);
        if (group == null) {
            groups.put(qName, group = new JSSuperGroup(qName));
        }

        group.addChild(_child);
    }

    @Override
    @Nonnull
    public ActionPresentation getPresentation() {
        return new ActionPresentationData(
            IdeLocalize.actionStructureviewGroupMethodsByDefiningType().get(),
            null,
            PlatformIconGroup.gutterImplementingmethod()
        );
    }

    @Override
    @Nonnull
    public String getName() {
        return SHOW_CLASSES;
    }

    private static boolean isParentGrouped(AbstractTreeNode parent) {
        while (parent != null) {
            if (parent.getValue() instanceof JSSuperGroup) {
                return true;
            }
            parent = (AbstractTreeNode)parent.getParent();
        }
        return false;
    }
}
