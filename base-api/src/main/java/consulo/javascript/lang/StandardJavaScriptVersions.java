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

package consulo.javascript.lang;

import java.util.List;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import com.intellij.openapi.components.ServiceManager;

/**
 * @author VISTALL
 * @since 11.12.2015
 */
public abstract class StandardJavaScriptVersions
{
	public static interface Marker
	{
		int getWeight();
	}

	@Nonnull
	public static StandardJavaScriptVersions getInstance()
	{
		return ServiceManager.getService(StandardJavaScriptVersions.class);
	}

	@Nonnull
	public abstract JavaScriptLanguageVersion getDefaultVersion();
	@Nonnull
	public abstract List<JavaScriptLanguageVersion> getValidLanguageVersions();

	@Nonnull
	public abstract JavaScriptLanguageVersion findVersionById(@Nullable String id);
}
