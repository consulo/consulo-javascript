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

package com.intellij.lang.javascript.impl.search;

import com.intellij.lang.javascript.psi.JSNamedElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.ide.navigation.GotoTargetRendererProvider;
import consulo.language.editor.ui.PsiElementListCellRenderer;
import consulo.language.psi.PsiElement;
import consulo.navigation.ItemPresentation;
import jakarta.annotation.Nullable;

/**
 * @author Maxim.Mossienko
 * @since 2008-04-28
 */
@ExtensionImpl
public class JSGotoTargetRendererProvider implements GotoTargetRendererProvider {
    static class JSClassListCellRenderer extends PsiElementListCellRenderer<JSNamedElement> {
        @Override
        @RequiredReadAction
        public String getElementText(JSNamedElement element) {
            return element.getName();
        }

        @Override
        protected String getContainerText(JSNamedElement element, String name) {
            ItemPresentation presentation = element.getPresentation();
            return presentation != null ? presentation.getLocationString() : null;
        }

        @Override
        protected int getIconFlags() {
            return 0;
        }
    }

    @Nullable
    @Override
    public PsiElementListCellRenderer getRenderer(PsiElement element) {
        return element instanceof JSNamedElement ? new JSClassListCellRenderer() : null;
    }
}
