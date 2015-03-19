/*
 * Copyright 2013-2014 must-be.org
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

package org.mustbe.consulo.javascript.run.debug;

import java.util.List;

import org.chromium.sdk.JsScope;
import org.chromium.sdk.JsVariable;
import org.jetbrains.annotations.NotNull;
import com.intellij.icons.AllIcons;
import com.intellij.xdebugger.frame.XCompositeNode;
import com.intellij.xdebugger.frame.XNamedValue;
import com.intellij.xdebugger.frame.XValueChildrenList;
import com.intellij.xdebugger.frame.XValueNode;
import com.intellij.xdebugger.frame.XValuePlace;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;

/**
 * @author VISTALL
 * @since 20.03.14
 */
public class V8ScopeValue extends XNamedValue
{
	private final JsScope myVariableScope;

	public V8ScopeValue(JsScope variableScope)
	{
		super(variableScope.getType().name());
		myVariableScope = variableScope;
	}

	@Override
	public void computeChildren(@NotNull XCompositeNode node)
	{
		List<? extends JsVariable> variables = myVariableScope.getVariables();

		XValueChildrenList valueChildrenList = new XValueChildrenList();
		for(JsVariable variable : variables)
		{
			valueChildrenList.add(new V8VariableValue(variable));
		}
		node.addChildren(valueChildrenList, true);
	}

	@Override
	public void computePresentation(@NotNull XValueNode xValueNode, @NotNull XValuePlace xValuePlace)
	{
		xValueNode.setPresentation(AllIcons.Ide.SharedScope, new XValuePresentation()
		{
			@NotNull
			@Override
			public String getSeparator()
			{
				return "";
			}

			@Override
			public void renderValue(@NotNull XValueTextRenderer xValueTextRenderer)
			{

			}
		}, true);
	}
}
