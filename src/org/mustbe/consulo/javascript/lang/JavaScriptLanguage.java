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

package org.mustbe.consulo.javascript.lang;

import org.jetbrains.annotations.Nullable;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.openapi.fileTypes.LanguageFileType;

/**
 * @author VISTALL
 * @since 05.12.2015
 */
public class JavaScriptLanguage extends Language
{
	public static final JavaScriptLanguage INSTANCE = new JavaScriptLanguage();

	public JavaScriptLanguage()
	{
		super("JavaScript", "text/javascript", "application/javascript");
	}

	@Nullable
	@Override
	public LanguageFileType getAssociatedFileType()
	{
		return JavaScriptFileType.INSTANCE;
	}

	@Override
	public boolean isCaseSensitive()
	{
		return true;
	}
}
