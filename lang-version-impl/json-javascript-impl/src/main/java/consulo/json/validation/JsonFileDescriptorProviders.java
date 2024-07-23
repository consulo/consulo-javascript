/*
 * Copyright 2013-2015 must-be.org
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

package consulo.json.validation;

import consulo.annotation.access.RequiredReadAction;
import consulo.application.util.CachedValuesManager;
import consulo.json.JsonFileType;
import consulo.json.validation.descriptor.JsonObjectDescriptor;
import consulo.language.psi.PsiFile;
import consulo.application.util.CachedValueProvider;
import consulo.language.psi.PsiModificationTracker;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class JsonFileDescriptorProviders {
    @Nullable
    @RequiredReadAction
    public static JsonObjectDescriptor getRootDescriptor(@Nullable final PsiFile file) {
        if (file == null || file.getFileType() != JsonFileType.INSTANCE) {
            return null;
        }
        return CachedValuesManager.getManager(file.getProject()).createCachedValue(
            new CachedValueProvider<JsonObjectDescriptor>() {
                @Nullable
                @Override
                @RequiredReadAction
                public Result<JsonObjectDescriptor> compute() {
                    for (JsonFileDescriptorProvider provider : JsonFileDescriptorProvider.EP_NAME.getExtensions()) {
                        if (provider.isMyFile(file)) {
                            JsonObjectDescriptor objectDescriptor = new JsonObjectDescriptor();
                            provider.fillRootObject(objectDescriptor, file);

                            return Result.create(objectDescriptor, file, PsiModificationTracker.OUT_OF_CODE_BLOCK_MODIFICATION_COUNT);
                        }
                    }
                    return null;
                }
            },
            false
        ).getValue();
    }
}
