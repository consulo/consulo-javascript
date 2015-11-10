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

package org.mustbe.consulo.json.validation;

import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.json.JsonFileType;
import org.mustbe.consulo.json.validation.descriptor.JsonObjectDescriptor;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class JsonFileDescriptorProviders
{
	@Nullable
	@RequiredReadAction
	public static JsonObjectDescriptor getRootDescriptor(@Nullable final PsiFile file)
	{
		if(file == null || file.getFileType() != JsonFileType.INSTANCE)
		{
			return null;
		}
		return CachedValuesManager.getManager(file.getProject()).createCachedValue(new CachedValueProvider<JsonObjectDescriptor>()
		{
			@Nullable
			@Override
			@RequiredReadAction
			public Result<JsonObjectDescriptor> compute()
			{
				for(JsonFileDescriptorProvider provider : JsonFileDescriptorProvider.EP_NAME.getExtensions())
				{
					if(provider.isMyFile(file))
					{
						JsonObjectDescriptor objectDescriptor = new JsonObjectDescriptor();
						provider.fillRootObject(objectDescriptor, file);

						return Result.create(objectDescriptor, file);
					}
				}
				return null;
			}
		}, false).getValue();
	}
}
