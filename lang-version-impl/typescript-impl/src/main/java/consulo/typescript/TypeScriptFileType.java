/*
 * Copyright 2013-2016 must-be.org
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

package consulo.typescript;

import consulo.module.Module;
import consulo.virtualFileSystem.VirtualFile;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.javascript.language.JavaScriptFileTypeWithVersion;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.version.LanguageVersion;
import consulo.language.file.LanguageFileType;
import consulo.localize.LocalizeValue;
import consulo.typescript.version.TypeScriptLanguageVersion;
import consulo.ui.image.Image;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 03.03.2016
 */
public class TypeScriptFileType extends LanguageFileType implements JavaScriptFileTypeWithVersion {
    public static final TypeScriptFileType INSTANCE = new TypeScriptFileType();

    private TypeScriptFileType() {
        super(JavaScriptLanguage.INSTANCE);
    }

    @Nonnull
    @Override
    public String getId() {
        return "TypeScript";
    }

    @Nonnull
    @Override
    public LocalizeValue getDescription() {
        return JavaScriptLocalize.typescriptFiletypeDescription();
    }

    @Nonnull
    @Override
    public String getDefaultExtension() {
        return "ts";
    }

    @Nullable
    @Override
    public Image getIcon() {
        return JavaScriptIconGroup.typescript();
    }

    @RequiredReadAction
    @Nonnull
    @Override
    public LanguageVersion getLanguageVersion(@Nullable Module module, @Nullable VirtualFile virtualFile) {
        return TypeScriptLanguageVersion.getInstance();
    }
}
