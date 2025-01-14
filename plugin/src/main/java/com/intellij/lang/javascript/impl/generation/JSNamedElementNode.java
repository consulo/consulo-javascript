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

package com.intellij.lang.javascript.impl.generation;

import com.intellij.javascript.JSParameterInfoHandler;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.editor.generation.ClassMember;
import consulo.language.editor.generation.MemberChooserObject;
import consulo.language.editor.generation.PsiElementMemberChooserObject;
import consulo.language.icon.IconDescriptorUpdaters;
import consulo.language.psi.PsiElement;
import consulo.platform.base.icon.PlatformIconGroup;
import consulo.ui.image.Image;
import consulo.ui.image.ImageEffects;

/**
 * @author Maxim.Mossienko
 * @since 2008-07-17
 */
public class JSNamedElementNode extends PsiElementMemberChooserObject implements ClassMember {
    @RequiredReadAction
    public JSNamedElementNode(JSNamedElement node) {
        super(node, buildTextFor(node), buildIcon(node));
    }

    @RequiredReadAction
    private static Image buildIcon(JSNamedElement node) {
        Image icon = IconDescriptorUpdaters.getIcon(node, 0);

        if (node instanceof JSFunction function) {
            Image accessIcon;

            if (function.isGetProperty()) {
                accessIcon = PlatformIconGroup.nodesRead_access();
            }
            else if (function.isSetProperty()) {
                accessIcon = PlatformIconGroup.nodesWrite_access();
            }
            else {
                accessIcon = null;
            }

            if (accessIcon != null) {
                icon = ImageEffects.appendRight(icon, accessIcon);
            }
        }
        return icon;
    }

    @RequiredReadAction
    private static String buildTextFor(JSNamedElement node) {
        StringBuilder text = new StringBuilder(node.getName());

        if (node instanceof JSFunction function) {
            text.append("(");
            JSParameterList parameterList = function.getParameterList();

            if (parameterList != null) {
                boolean first = true;
                for (JSParameter p : parameterList.getParameters()) {
                    if (!first) {
                        text.append(", ");
                    }
                    first = false;
                    text.append(JSParameterInfoHandler.getSignatureForParameter(p, false));
                }
            }

            text.append(")");
            String typeString = function.getReturnTypeString();
            if (typeString != null) {
                text.append(":").append(typeString);
            }
        }
        else if (node instanceof JSVariable var) {
            String typeString = var.getTypeString();
            if (typeString != null) {
                text.append(":").append(typeString);
            }
        }
        return text.toString();
    }

    @Override
    @RequiredReadAction
    public MemberChooserObject getParentNodeDelegate() {
        PsiElement element = getPsiElement();
        PsiElement parent = JSResolveUtil.findParent(element);
        return new JSNamedElementNode((JSNamedElement)parent);
    }
}
