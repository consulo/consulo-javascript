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

package consulo.javascript.language;

import consulo.language.psi.PsiElement;
import consulo.language.version.LanguageVersion;

import jakarta.annotation.Nonnull;
import java.util.Collections;
import java.util.Set;

/**
 * @author VISTALL
 * @since 23.02.2016
 */
public class JavaScriptVersionUtil
{
	public static boolean containsFeature(@Nonnull PsiElement element, @Nonnull JavaScriptFeature feature)
	{
		return getFeatures(element).contains(feature);
	}

	public static Set<JavaScriptFeature> getFeatures(@Nonnull PsiElement element)
	{
		LanguageVersion languageVersion = element.getLanguageVersion();
		return languageVersion instanceof JavaScriptLanguageVersion ? ((JavaScriptLanguageVersion) languageVersion).getFeatures() : Collections.<JavaScriptFeature>emptySet();
	}
}
