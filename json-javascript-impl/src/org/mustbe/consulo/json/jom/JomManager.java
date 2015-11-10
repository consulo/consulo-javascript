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

package org.mustbe.consulo.json.jom;

import org.consulo.lombok.annotations.ProjectService;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.json.JsonFileType;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.CachedValue;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
@ProjectService
public class JomManager
{
	private static final Key<CachedValue<JomFileElement<?>>> JOM_FILE_ELEMENT = Key.create("jom.file.lement");

	private final Project myProject;

	public JomManager(Project project)
	{
		myProject = project;
	}

	@Nullable
	@RequiredReadAction
	@SuppressWarnings("unchecked")
	public <T extends JomElement> JomFileElement<T> getFileElement(@NotNull final PsiFile psiFile)
	{
		FileType fileType = psiFile.getFileType();
		if(fileType != JsonFileType.INSTANCE)
		{
			return null;
		}

		CachedValue<JomFileElement<?>> cachedValue = psiFile.getUserData(JOM_FILE_ELEMENT);
		if(cachedValue == null)
		{
			JomFileDescriptor jomFileDescriptor = findFileDescriptor(psiFile);

			if(jomFileDescriptor == null)
			{
				return null;
			}

			final JomFileDescriptor finalJomFileDescriptor = jomFileDescriptor;
			cachedValue = CachedValuesManager.getManager(myProject).createCachedValue(new CachedValueProvider<JomFileElement<?>>()
			{
				@Nullable
				@Override
				public Result<JomFileElement<?>> compute()
				{
					return Result.<JomFileElement<?>>create(new JomFileElement((JSFile) psiFile, finalJomFileDescriptor), psiFile);
				}
			}, false);

			psiFile.putUserData(JOM_FILE_ELEMENT, cachedValue);
		}

		return (JomFileElement<T>) cachedValue.getValue();
	}

	@Nullable
	private static JomFileDescriptor findFileDescriptor(@NotNull PsiFile psiFile)
	{
		JomFileDescriptor jomFileDescriptor = null;
		for(JomFileDescriptor temp : JomFileDescriptor.EP_NAME.getExtensions())
		{
			if(temp.isMyFile(psiFile))
			{
				jomFileDescriptor = temp;
				break;
			}
		}
		return jomFileDescriptor;
	}
}
