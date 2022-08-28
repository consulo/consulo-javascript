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

package consulo.json.validation.descriptor;

import java.util.HashMap;
import java.util.Map;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import consulo.json.validation.NativeArray;
import consulo.util.collection.ContainerUtil;
import consulo.language.impl.ast.Factory;
import consulo.util.collection.Maps;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class JsonObjectDescriptor
{
	private Map<String, JsonPropertyDescriptor> myProperties = new HashMap<String, JsonPropertyDescriptor>();

	@Nonnull
	public JsonPropertyDescriptor addProperty(@Nullable final String propertyName, @Nonnull final Class<?> value)
	{
		if(value == Object.class)
		{
			throw new IllegalArgumentException("We cant add object type, use JsonObjectDescriptor as parameter");
		}

		return myProperties.computeIfAbsent(propertyName, p -> new JsonPropertyDescriptor(p, value));
	}

	@Nonnull
	public JsonPropertyDescriptor addProperty(@Nullable final String propertyName, @Nonnull final JsonObjectDescriptor value)
	{
		return myProperties.computeIfAbsent(propertyName, p -> new JsonPropertyDescriptor(p, value));
	}

	@Nonnull
	public JsonPropertyDescriptor addProperty(@Nullable final String propertyName, @Nonnull final NativeArray value)
	{
		return myProperties.computeIfAbsent(propertyName, p -> new JsonPropertyDescriptor(p, value));
	}

	@Nullable
	public JsonPropertyDescriptor getProperty(@Nonnull final String propertyName)
	{
		JsonPropertyDescriptor propertyDescriptor = myProperties.get(propertyName);
		if(propertyDescriptor != null)
		{
			return propertyDescriptor;
		}

		return myProperties.get(null);
	}

	public Map<String, JsonPropertyDescriptor> getProperties()
	{
		return myProperties;
	}
}
