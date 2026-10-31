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
package consulo.javascript.module.declarations;

import consulo.application.Application;
import consulo.module.content.layer.ModifiableRootModel;
import consulo.module.content.layer.ModuleRootModel;
import consulo.module.content.layer.orderEntry.CustomOrderEntry;
import consulo.module.content.layer.orderEntry.CustomOrderEntryTypeProvider;
import consulo.module.content.layer.orderEntry.OrderEntry;
import consulo.virtualFileSystem.VirtualFile;
import org.jspecify.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * The declarations order entries of a module ({@link JavaScriptDeclarationsOrderEntryModel}): a framework adds its
 * declarations when its extension is enabled, instead of contributing global files of its own.
 *
 * @since 2026-10-31
 */
public final class JavaScriptDeclarations {
    public static final String TYPE_ID = "javascript-declarations";

    private JavaScriptDeclarations() {
    }

    /**
     * Adds the declarations to the model (not committed), replacing the entry of the same name; only if they differ.
     * No roots remove the entry.
     *
     * @return whether the model was changed
     */
    @SuppressWarnings("unchecked")
    public static boolean set(ModifiableRootModel model, String name, List<String> rootUrls) {
        if (rootUrls.isEmpty()) {
            return remove(model, name);
        }
        JavaScriptDeclarationsOrderEntryModel wanted = new JavaScriptDeclarationsOrderEntryModel(name, rootUrls);
        CustomOrderEntry<JavaScriptDeclarationsOrderEntryModel> existing = find(model, name);
        if (existing != null) {
            if (existing.getModel().isEquivalentTo(wanted)) {
                return false;
            }
            model.removeOrderEntry(existing);
        }
        CustomOrderEntryTypeProvider<JavaScriptDeclarationsOrderEntryModel> type = findType();
        if (type == null) {
            return existing != null;
        }
        model.addCustomOderEntry(type, wanted);
        return true;
    }

    /**
     * @return whether the model was changed
     */
    public static boolean remove(ModifiableRootModel model, String name) {
        CustomOrderEntry<JavaScriptDeclarationsOrderEntryModel> existing = find(model, name);
        if (existing == null) {
            return false;
        }
        model.removeOrderEntry(existing);
        return true;
    }

    /**
     * The existing roots of the declarations of a module
     */
    public static List<VirtualFile> getRoots(ModuleRootModel model) {
        List<VirtualFile> result = new ArrayList<>();
        for (OrderEntry orderEntry : model.getOrderEntries()) {
            if (orderEntry instanceof CustomOrderEntry<?> custom && custom.getModel() instanceof JavaScriptDeclarationsOrderEntryModel declarations) {
                result.addAll(declarations.getRoots());
            }
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private static @Nullable CustomOrderEntry<JavaScriptDeclarationsOrderEntryModel> find(ModuleRootModel model, String name) {
        for (OrderEntry orderEntry : model.getOrderEntries()) {
            if (orderEntry instanceof CustomOrderEntry<?> custom
                && custom.getModel() instanceof JavaScriptDeclarationsOrderEntryModel declarations
                && declarations.getName().equals(name)) {
                return (CustomOrderEntry<JavaScriptDeclarationsOrderEntryModel>) custom;
            }
        }
        return null;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private static @Nullable CustomOrderEntryTypeProvider<JavaScriptDeclarationsOrderEntryModel> findType() {
        for (CustomOrderEntryTypeProvider type : Application.get().getExtensionList(CustomOrderEntryTypeProvider.class)) {
            if (TYPE_ID.equals(type.getId())) {
                return type;
            }
        }
        return null;
    }
}
