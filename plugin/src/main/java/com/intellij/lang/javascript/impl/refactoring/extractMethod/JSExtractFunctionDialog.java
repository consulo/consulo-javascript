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

package com.intellij.lang.javascript.impl.refactoring.extractMethod;

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTextField;

import consulo.javascript.language.JavaScriptBundle;
import consulo.ui.ex.awt.DialogWrapper;

/**
 * @author Maxim.Mossienko
 *         Date: Aug 9, 2008
 *         Time: 9:22:10 AM
 */
public class JSExtractFunctionDialog extends DialogWrapper implements JSExtractFunctionSettings
{
	private JPanel myPanel;
	private JTextField myFunctionName;

	JSExtractFunctionDialog()
	{
		super(false);

		setTitle(JavaScriptBundle.message("javascript.extract.method.title"));
		init();
	}

	@Override
	protected JComponent createCenterPanel()
	{
		return myPanel;
	}

	@Override
	public String getMethodName()
	{
		return myFunctionName.getText();
	}
}
