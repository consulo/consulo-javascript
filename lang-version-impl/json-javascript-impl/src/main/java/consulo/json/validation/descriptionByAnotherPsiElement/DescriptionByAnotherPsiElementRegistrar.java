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

import javax.annotation.Nonnull;
import javax.inject.Singleton;

import com.intellij.openapi.project.Project;
import com.intellij.util.NotNullFunction;
import consulo.editor.notifications.EditorNotificationProvider;
import consulo.editor.notifications.EditorNotificationProviders;

/**
 * @author VISTALL
 * @since 12.11.2015
 */
@Singleton
public class DescriptionByAnotherPsiElementRegistrar
{
	DescriptionByAnotherPsiElementRegistrar()
	{
		for(final DescriptionByAnotherPsiElementProvider<?> provider : DescriptionByAnotherPsiElementProvider.EP_NAME.getExtensions())
		{
			EditorNotificationProviders.registerProvider(new NotNullFunction<Project, EditorNotificationProvider<?>>()
			{
				@Nonnull
				@Override
				public EditorNotificationProvider<?> fun(Project project)
				{
					//noinspection unchecked
					return new DescriptionByAnotherPsiElementEditorNotification(project, provider);
				}
			});
		}
	}
}
