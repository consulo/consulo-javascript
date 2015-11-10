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

import org.jetbrains.annotations.NotNull;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class JsonPropertyDescriptor implements JsonNodeDescriptor
{
	private String myName;
	private Object myValue;

	public JsonPropertyDescriptor(@NotNull String name, @NotNull Object value)
	{
		myName = name;
		myValue = value;
	}

	@NotNull
	public Object getValue()
	{
		return myValue;
	}

	@NotNull
	public JsonPropertyType getType()
	{
		return myValue instanceof JsonPropertyType ? (JsonPropertyType) myValue : JsonPropertyType.Object;
	}

	public String getName()
	{
		return myName;
	}
}
