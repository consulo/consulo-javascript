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

package com.intellij.lang.javascript.formatter;

import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CustomCodeStyleSettings;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 12, 2008
 *         Time: 10:25:17 PM
 */
public class JSCodeStyleSettings extends CustomCodeStyleSettings
{
	public int INDENT_PACKAGE_CHILDREN = DO_NOT_INDENT;
	public boolean USE_SEMICOLON_AFTER_STATEMENT = true;
	public String FIELD_PREFIX = "_";
	public String PROPERTY_PREFIX = "";

	public static final int DO_NOT_INDENT = 0;
	public static final int INDENT = 1;

	protected JSCodeStyleSettings(CodeStyleSettings container)
	{
		super("JSCodeStyleSettings", container);
	}
}
