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

package consulo.json.validation.descriptionByAnotherPsiElement;

import consulo.annotation.component.ExtensionImpl;
import consulo.component.ComponentManager;
import consulo.component.extension.ExtensionExtender;
import consulo.component.extension.ExtensionPoint;
import consulo.fileEditor.EditorNotificationProvider;
import consulo.project.Project;

import jakarta.annotation.Nonnull;

import java.util.function.Consumer;

/**
 * @author VISTALL
 * @since 12.11.2015
 */
@ExtensionImpl
@SuppressWarnings("unchecked")
public class DescriptionByAnotherPsiElementRegistrar implements ExtensionExtender<EditorNotificationProvider> {
    @Override
    public void extend(@Nonnull ComponentManager componentManager, @Nonnull Consumer<EditorNotificationProvider> consumer) {
        ExtensionPoint<DescriptionByAnotherPsiElementProvider> extensionPoint =
            componentManager.getExtensionPoint(DescriptionByAnotherPsiElementProvider.class);
        for (DescriptionByAnotherPsiElementProvider<?> provider : extensionPoint) {
            consumer.accept(new DescriptionByAnotherPsiElementEditorNotification((Project)componentManager, provider));
        }
    }

    @Nonnull
    @Override
    public Class<EditorNotificationProvider> getExtensionClass() {
        return EditorNotificationProvider.class;
    }

    @Override
    public boolean hasAnyExtensions(ComponentManager componentManager) {
        return componentManager.getExtensionPoint(DescriptionByAnotherPsiElementProvider.class).hasAnyExtensions();
    }
}
