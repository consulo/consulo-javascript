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

package org.mustbe.consulo.javascript.run.debug;

import java.util.Collection;

import org.chromium.sdk.JsFunction;
import org.chromium.sdk.JsObject;
import org.chromium.sdk.JsValue;
import org.chromium.sdk.JsVariable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.icons.AllIcons;
import com.intellij.xdebugger.frame.XCompositeNode;
import com.intellij.xdebugger.frame.XNamedValue;
import com.intellij.xdebugger.frame.XValueChildrenList;
import com.intellij.xdebugger.frame.XValueNode;
import com.intellij.xdebugger.frame.XValuePlace;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import lombok.val;

/**
 * @author VISTALL
 * @since 20.03.14
 */
public class V8VariableValue extends XNamedValue
{
	@NotNull
	private final JsVariable myJsVariable;

	public V8VariableValue(@NotNull JsVariable jsVariable)
	{
		super(jsVariable.getName());
		myJsVariable = jsVariable;
	}

	@Override
	public void computeChildren(@NotNull XCompositeNode node)
	{
		JsValue value = myJsVariable.getValue();
		if(value instanceof JsObject)
		{
			Collection<? extends JsVariable> properties = ((JsObject) value).getProperties();
			XValueChildrenList valueChildrenList = new XValueChildrenList();
			for(JsVariable property : properties)
			{
				valueChildrenList.add(new V8VariableValue(property));
			}
			node.addChildren(valueChildrenList, true);
		}
		else
		{
			node.addChildren(XValueChildrenList.EMPTY, true);
		}
	}

	@Override
	public void computePresentation(@NotNull XValueNode xValueNode, @NotNull XValuePlace xValuePlace)
	{
		val value = myJsVariable.getValue();
		val valueType = value.getType();

		xValueNode.setPresentation(AllIcons.Nodes.Variable, new XValuePresentation()
		{
			@NotNull
			@Override
			public String getSeparator()
			{
				if(value instanceof JsObject)
				{
					return "";
				}
				return super.getSeparator();
			}

			@Nullable
			@Override
			public String getType()
			{
				switch(valueType)
				{
					case TYPE_NUMBER:
					case TYPE_STRING:
					case TYPE_NULL:
					case TYPE_DATE:
					case TYPE_REGEXP:
					case TYPE_UNDEFINED:
					case TYPE_BOOLEAN:
						return null;
					default:
						if(value instanceof JsFunction)
						{
							return "Function";
						}
						else if(value instanceof JsObject)
						{
							return ((JsObject) value).getClassName();
						}
						return null;
				}
			}

			@Override
			public void renderValue(@NotNull XValueTextRenderer xValueTextRenderer)
			{
				switch(myJsVariable.getValue().getType())
				{
					case TYPE_NUMBER:
						xValueTextRenderer.renderNumericValue(myJsVariable.getValue().getValueString());
						break;
					case TYPE_STRING:
						xValueTextRenderer.renderStringValue(myJsVariable.getValue().getValueString());
						break;
					case TYPE_FUNCTION:
						break;
					case TYPE_BOOLEAN:
						xValueTextRenderer.renderKeywordValue(myJsVariable.getValue().getValueString());
						break;
					case TYPE_ERROR:
						break;
					case TYPE_REGEXP:
						xValueTextRenderer.renderStringValue(myJsVariable.getValue().getValueString());
						break;
					case TYPE_DATE:
						xValueTextRenderer.renderValue(myJsVariable.getValue().getValueString());
						break;
					case TYPE_UNDEFINED:
						xValueTextRenderer.renderKeywordValue("undefined");
						break;
					case TYPE_NULL:
						xValueTextRenderer.renderKeywordValue("null");
						break;
				}
			}
		}, true);
	}
}
