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
import consulo.document.util.TextRange;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiManager;
import consulo.language.psi.PsiReferenceBase;
import consulo.language.util.IncorrectOperationException;
import consulo.virtualFileSystem.VirtualFile;
import org.jspecify.annotations.Nullable;

import java.util.List;

/**
 * The module of a specifier: {@code 'vue'} of {@code import ... from 'vue'}, {@code './util'} of
 * {@code require('./util')}. Resolved by the {@link JavaScriptModuleResolver}s, else as a path relative to the file.
 *
 * @since 2026-10-31
 */
public class JavaScriptModuleReference extends PsiReferenceBase<PsiElement> {
    private static final List<String> EXTENSIONS = List.of(".js", ".mjs", ".cjs", ".jsx", ".ts", ".tsx", ".d.ts", ".json");

    private final String mySpecifier;
    private final JavaScriptModuleRequest.Kind myKind;

    /**
     * @param stringRange the range of the quoted specifier in the element
     */
    public JavaScriptModuleReference(PsiElement element, TextRange stringRange, JavaScriptModuleRequest.Kind kind) {
        super(element, unquoted(stringRange), true);
        mySpecifier = unquoted(stringRange).substring(element.getText());
        myKind = kind;
    }

    /**
     * @return the reference of a quoted specifier at {@code stringRange} of the element, {@code null} for an empty one
     */
    public static @Nullable JavaScriptModuleReference create(PsiElement element, TextRange stringRange, JavaScriptModuleRequest.Kind kind) {
        return stringRange.getLength() <= 2 ? null : new JavaScriptModuleReference(element, stringRange, kind);
    }

    public String getSpecifier() {
        return mySpecifier;
    }

    @RequiredReadAction
    @Override
    public @Nullable PsiElement resolve() {
        PsiElement element = getElement();
        PsiFile module = JavaScriptModuleResolution.resolve(element, mySpecifier, myKind);
        if (module != null || !(mySpecifier.startsWith(".") || mySpecifier.startsWith("/"))) {
            return module;
        }
        PsiFile file = element.getContainingFile();
        VirtualFile virtualFile = file == null ? null : file.getOriginalFile().getVirtualFile();
        VirtualFile directory = virtualFile == null ? null : virtualFile.getParent();
        VirtualFile resolved = directory == null ? null : findRelative(directory, mySpecifier);
        return resolved == null ? null : PsiManager.getInstance(element.getProject()).findFile(resolved);
    }

    @Override
    public Object[] getVariants() {
        return EMPTY_ARRAY;
    }

    /**
     * The specifier is kept: a module is not renamed with its file
     */
    @Override
    public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException {
        return getElement();
    }

    /**
     * A file relative to the directory: as written, with an extension, else the index of a directory
     */
    private static @Nullable VirtualFile findRelative(VirtualFile directory, String path) {
        VirtualFile exact = path.startsWith("/") ? null : directory.findFileByRelativePath(path);
        if (exact != null && !exact.isDirectory()) {
            return exact;
        }
        for (String extension : EXTENSIONS) {
            VirtualFile file = directory.findFileByRelativePath(path + extension);
            if (file != null && !file.isDirectory()) {
                return file;
            }
        }
        if (exact != null) {
            for (String extension : EXTENSIONS) {
                VirtualFile index = exact.findChild("index" + extension);
                if (index != null && !index.isDirectory()) {
                    return index;
                }
            }
        }
        return null;
    }

    private static TextRange unquoted(TextRange stringRange) {
        return stringRange.getLength() < 2 ? stringRange : new TextRange(stringRange.getStartOffset() + 1, stringRange.getEndOffset() - 1);
    }
}
