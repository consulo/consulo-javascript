/*
 * Copyright 2013-2016 must-be.org
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

package consulo.javascript.lang;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 23.02.2016
 */
public enum JavaScriptFeature
{
	CLASS("Classes"),
	OCTAL_LITERAL("Octal literals"),
	BINARY_LITERAL("Binary literals"),
	PARAMETER_DEFAULT_VALUE("Default parameter values"),
	REST_PARAMETER("Rest parameters"),
	SPREAD_OPERATOR("Spread operator"),
	EXPONENTIATION_OPERATOR("exponentiation operator");

	private String myName;

	JavaScriptFeature(String name)
	{
		myName = name;
	}

	@Nonnull
	public String getName()
	{
		return myName;
	}
}
