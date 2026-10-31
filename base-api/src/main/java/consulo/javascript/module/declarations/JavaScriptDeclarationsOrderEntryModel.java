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

import consulo.content.RootProvider;
import consulo.content.RootProviderBase;
import consulo.content.base.BinariesOrderRootType;
import consulo.content.base.SourcesOrderRootType;
import consulo.module.content.layer.ModuleRootLayer;
import consulo.module.content.layer.orderEntry.CustomOrderEntryModel;
import consulo.virtualFileSystem.VirtualFile;
import consulo.virtualFileSystem.VirtualFileManager;

import java.util.ArrayList;
import java.util.List;

/**
 * Declaration files of a framework the code of a module sees without imports ({@code declare global}, the generated
 * auto-imports of Nuxt): the roots are indexed and their global declarations visible to the module.
 *
 * @since 2026-10-31
 */
public class JavaScriptDeclarationsOrderEntryModel implements CustomOrderEntryModel {
    private final String myName;
    private final List<String> myRootUrls;
    private final RootProvider myRootProvider = new RootProviderBase() {
        @Override
        public String[] getUrls(String rootType) {
            return isServed(rootType) ? myRootUrls.toArray(String[]::new) : new String[0];
        }

        @Override
        public VirtualFile[] getFiles(String rootType) {
            return isServed(rootType) ? getRoots().toArray(VirtualFile.EMPTY_ARRAY) : VirtualFile.EMPTY_ARRAY;
        }
    };

    /**
     * @param name     the name of the declarations: the entry of a name is replaced ({@link JavaScriptDeclarations#set})
     * @param rootUrls files or directories; the ones which don't exist yet (generated later) are seen once they do
     */
    public JavaScriptDeclarationsOrderEntryModel(String name, List<String> rootUrls) {
        myName = name;
        myRootUrls = List.copyOf(rootUrls);
    }

    public String getName() {
        return myName;
    }

    public List<String> getRootUrls() {
        return myRootUrls;
    }

    /**
     * @return the existing roots
     */
    public List<VirtualFile> getRoots() {
        List<VirtualFile> result = new ArrayList<>();
        for (String url : myRootUrls) {
            VirtualFile file = VirtualFileManager.getInstance().findFileByUrl(url);
            if (file != null && file.isValid()) {
                result.add(file);
            }
        }
        return result;
    }

    @Override
    public void bind(ModuleRootLayer moduleRootLayer) {
    }

    @Override
    public String getPresentableName() {
        return myName;
    }

    @Override
    public boolean isValid() {
        return true;
    }

    @Override
    public boolean isSynthetic() {
        return true;
    }

    @Override
    public RootProvider getRootProvider() {
        return myRootProvider;
    }

    @Override
    public Object getEqualObject() {
        return JavaScriptDeclarations.TYPE_ID + " " + myName;
    }

    @Override
    public boolean isEquivalentTo(CustomOrderEntryModel model) {
        return model instanceof JavaScriptDeclarationsOrderEntryModel other && myName.equals(other.myName) && myRootUrls.equals(other.myRootUrls);
    }

    @Override
    public JavaScriptDeclarationsOrderEntryModel clone() {
        return new JavaScriptDeclarationsOrderEntryModel(myName, myRootUrls);
    }

    private static boolean isServed(String rootType) {
        return SourcesOrderRootType.ID.equals(rootType) || BinariesOrderRootType.ID.equals(rootType);
    }
}
