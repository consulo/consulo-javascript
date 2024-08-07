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

package consulo.json.jom;

import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ExtensionAPI;
import consulo.component.extension.ExtensionPointName;
import consulo.language.psi.PsiFile;
import consulo.ui.image.Image;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
@ExtensionAPI(ComponentScope.APPLICATION)
public abstract class JomFileDescriptor<T extends JomElement> {
    public static ExtensionPointName<JomFileDescriptor> EP_NAME = ExtensionPointName.create(JomFileDescriptor.class);

    private Class<T> myDefinitionClass;

    public JomFileDescriptor(@Nonnull Class<T> definitionClass) {
        myDefinitionClass = definitionClass;
    }

    @Nonnull
    public Class<T> getDefinitionClass() {
        return myDefinitionClass;
    }

    @Nonnull
    public abstract Image getIcon();

    public abstract boolean isMyFile(@Nonnull PsiFile psiFile);
}
