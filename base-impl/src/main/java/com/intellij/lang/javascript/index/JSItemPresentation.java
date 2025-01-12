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

package com.intellij.lang.javascript.index;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.component.util.Iconable;
import consulo.language.icon.IconDescriptorUpdaters;
import consulo.language.psi.PsiFile;
import consulo.navigation.ItemPresentation;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.image.Image;
import jakarta.annotation.Nullable;

/**
 * @author Maxim.Mossienko
 * @since 2006-04-07
 */
public class JSItemPresentation implements ItemPresentation {
    private JSNamedElement myElement;

    public JSItemPresentation(final JSNamedElement elementProxy) {
        this.myElement = elementProxy;
    }

    @Override
    public String getPresentableText() {
        return myElement.getName();
    }

    @Nullable
    @Override
    @RequiredReadAction
    public String getLocationString() {
        final PsiFile psiFile = myElement.getContainingFile();
        if (myElement instanceof JSVariable || myElement instanceof JSFunction) {
            if (JSResolveUtil.findParent(myElement) instanceof JSClass possibleJsClass) {
                StringBuilder presentation = new StringBuilder();

                presentation.append(possibleJsClass.getQualifiedName());
                presentation.append('(').append(getFileName(psiFile)).append(')');
                return presentation.toString();
            }
        }
        else if (myElement instanceof JSClass jsClass) {
            String s = jsClass.getQualifiedName();
            int i = s.lastIndexOf('.');

            if (i != -1) {
                StringBuilder presentation = new StringBuilder();

                presentation.append(s.substring(0, i));
                presentation.append('(').append(getFileName(psiFile)).append(')');
                return presentation.toString();
            }
        }
        return getFileName(psiFile);
    }

    @RequiredReadAction
    private static String getFileName(PsiFile psiFile) {
        String s = psiFile.getName();
        if (JSResolveUtil.isPredefinedFile(psiFile)) {
            return s.substring(s.lastIndexOf('/') + 1);
        }
        return s;
    }

    @Override
    @Nullable
    @RequiredUIAccess
    public Image getIcon() {
        return IconDescriptorUpdaters.getIcon(myElement, Iconable.ICON_FLAG_VISIBILITY);
    }
}
