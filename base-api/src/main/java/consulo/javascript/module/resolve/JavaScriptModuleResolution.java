/*
 * Copyright 2013-2026 consulo.io
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
package consulo.javascript.module.resolve;

import consulo.annotation.access.RequiredReadAction;
import consulo.application.Application;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiManager;
import consulo.virtualFileSystem.VirtualFile;
import org.jspecify.annotations.Nullable;

/**
 * Resolves a module specifier with the {@link JavaScriptModuleResolver}s.
 *
 * @since 2026-10-31
 */
public final class JavaScriptModuleResolution {
    private JavaScriptModuleResolution() {
    }

    /**
     * @param context an element of the importing file
     * @return the file of the module, {@code null} if no resolver knows it
     */
    @RequiredReadAction
    public static @Nullable VirtualFile resolveFile(PsiElement context, String specifier, JavaScriptModuleRequest.Kind kind) {
        if (specifier.isEmpty()) {
            return null;
        }
        PsiFile file = context.getContainingFile();
        if (file == null) {
            return null;
        }
        file = file.getOriginalFile();
        VirtualFile virtualFile = file.getVirtualFile();
        VirtualFile directory = virtualFile == null ? null : virtualFile.getParent();
        if (directory == null) {
            return null;
        }
        JavaScriptModuleRequest request = new JavaScriptModuleRequest(file, directory, specifier, kind);
        for (JavaScriptModuleResolver resolver : Application.get().getExtensionList(JavaScriptModuleResolver.class)) {
            VirtualFile resolved = resolver.resolve(request);
            if (resolved != null && resolved.isValid()) {
                return resolved;
            }
        }
        return null;
    }

    /**
     * @return the PSI file of the module, {@code null} if no resolver knows it
     */
    @RequiredReadAction
    public static @Nullable PsiFile resolve(PsiElement context, String specifier, JavaScriptModuleRequest.Kind kind) {
        VirtualFile resolved = resolveFile(context, specifier, kind);
        return resolved == null || resolved.isDirectory() ? null : PsiManager.getInstance(context.getProject()).findFile(resolved);
    }
}
