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

import com.intellij.application.options.CodeStyleAbstractConfigurable;
import com.intellij.application.options.CodeStyleAbstractPanel;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.psi.codeStyle.CodeStyleSettings;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Mar 12, 2008
 * Time: 10:36:16 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSCodeStyleConfigurable extends CodeStyleAbstractConfigurable
{
	public JSCodeStyleConfigurable(final CodeStyleSettings settings, final CodeStyleSettings codeStyleSettings)
	{
		super(settings, codeStyleSettings, JSBundle.message("js.code.style.tab.name"));
	}

	@Override
	protected CodeStyleAbstractPanel createPanel(final CodeStyleSettings settings)
	{
		return new JSCodeStylePanel(settings);
	}

	@Override
	public String getHelpTopic()
	{
		return null; // TODO
	}
}
