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

import com.intellij.lang.javascript.psi.JSExpressionCodeFragment;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.file.FileTypeManager;
import consulo.language.file.FileViewProvider;
import consulo.language.file.light.LightVirtualFile;
import consulo.language.impl.ast.FileElement;
import consulo.language.impl.file.SingleRootFileViewProvider;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiManager;
import consulo.project.Project;
import org.jetbrains.annotations.NonNls;

import jakarta.annotation.Nonnull;

/**
 * @author nik
 */
public class JSExpressionCodeFragmentImpl extends JSFileImpl implements JSExpressionCodeFragment {
    private PsiElement myContext;
    private boolean myPhysical;
    private FileViewProvider myViewProvider;

    public JSExpressionCodeFragmentImpl(Project project, @NonNls String name, CharSequence text, boolean isPhysical) {
        super(new SingleRootFileViewProvider(
            PsiManager.getInstance(project),
            new LightVirtualFile(name, FileTypeManager.getInstance().getFileTypeByFileName(name), text),
            isPhysical
        ));
        myPhysical = isPhysical;
        ((SingleRootFileViewProvider)getViewProvider()).forceCachedPsi(this);
    }

    //todo[nik] extract these methods from PsiCodeFragmentImpl?
    @Override
    @RequiredReadAction
    protected JSExpressionCodeFragmentImpl clone() {
        JSExpressionCodeFragmentImpl clone = (JSExpressionCodeFragmentImpl)cloneImpl((FileElement)calcTreeElement().clone());
        clone.myPhysical = false;
        clone.myOriginalFile = this;
        SingleRootFileViewProvider cloneViewProvider =
            new SingleRootFileViewProvider(getManager(), new LightVirtualFile(getName(), getLanguage(), getText()), false);
        cloneViewProvider.forceCachedPsi(clone);
        clone.myViewProvider = cloneViewProvider;
        return clone;
    }

    @Override
    public PsiElement getContext() {
        return myContext;
    }

    @Override
    @Nonnull
    public FileViewProvider getViewProvider() {
        return myViewProvider != null ? myViewProvider : super.getViewProvider();
    }

    @Override
    public boolean isValid() {
        return super.isValid() && myContext == null || myContext.isValid();
    }

    @Override
    public boolean isPhysical() {
        return myPhysical;
    }

    public void setContext(PsiElement context) {
        myContext = context;
    }
}
