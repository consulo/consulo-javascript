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

package org.mustbe.consulo.json.validation.descriptor;

import java.util.HashMap;
import java.util.Map;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.openapi.util.Factory;
import com.intellij.util.containers.ContainerUtil;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class JsonObjectDescriptor implements JsonNodeDescriptor
{
	private Map<String, JsonPropertyDescriptor> myProperties = new HashMap<String, JsonPropertyDescriptor>();

	@NotNull
	public JsonPropertyDescriptor addSimpleProperty(@NotNull final String propertyName, @NotNull final JsonPropertyType value)
	{
		if(value == JsonPropertyType.Object)
		{
			throw new IllegalArgumentException("We cant add object type as simple");
		}

		return ContainerUtil.getOrCreate(myProperties, propertyName, new Factory<JsonPropertyDescriptor>()
		{
			@Override
			public JsonPropertyDescriptor create()
			{
				return new JsonPropertyDescriptor(propertyName, value);
			}
		});
	}

	@NotNull
	public JsonPropertyDescriptor addObjectProperty(@NotNull final String propertyName, @NotNull final JsonObjectDescriptor value)
	{
		return ContainerUtil.getOrCreate(myProperties, propertyName, new Factory<JsonPropertyDescriptor>()
		{
			@Override
			public JsonPropertyDescriptor create()
			{
				return new JsonPropertyDescriptor(propertyName, value);
			}
		});
	}

	@Nullable
	public JsonPropertyDescriptor getProperty(@NotNull final String propertyName)
	{
		return myProperties.get(propertyName);
	}
}
