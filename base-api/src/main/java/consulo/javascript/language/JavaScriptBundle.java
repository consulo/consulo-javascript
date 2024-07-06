/*
 * Copyright 2000-2005 JetBrains s.r.o.
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

package consulo.javascript.language;

import consulo.annotation.DeprecationInfo;
import consulo.annotation.internal.MigratedExtensionsTo;
import consulo.javascript.localize.JavaScriptLocalize;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.PropertyKey;
import consulo.component.util.localize.AbstractBundle;

@Deprecated(forRemoval = true)
@DeprecationInfo("Use JavaScriptLocalize")
@MigratedExtensionsTo(JavaScriptLocalize.class)
public class JavaScriptBundle extends AbstractBundle
{
	@NonNls
	public static final String BUNDLE = "messages.JavaScriptBundle";

	private static final JavaScriptBundle ourInstance = new JavaScriptBundle();

	private JavaScriptBundle()
	{
		super(BUNDLE);
	}

	public static String message(@PropertyKey(resourceBundle = BUNDLE) String key)
	{
		return ourInstance.getMessage(key);
	}

	public static String message(@PropertyKey(resourceBundle = BUNDLE) String key, Object... params)
	{
		return ourInstance.getMessage(key, params);
	}
}
