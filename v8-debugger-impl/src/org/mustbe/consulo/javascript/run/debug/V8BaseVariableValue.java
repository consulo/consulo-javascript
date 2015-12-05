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

import javax.swing.Icon;

import org.chromium.sdk.JsArray;
import org.chromium.sdk.JsEvaluateContext;
import org.chromium.sdk.JsObject;
import org.chromium.sdk.JsValue;
import org.chromium.sdk.JsVariable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.util.text.StringUtil;
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
public abstract class V8BaseVariableValue extends XNamedValue
{
	@NotNull
	protected final JsEvaluateContext myEvaluateContext;

	protected V8BaseVariableValue(@NotNull JsEvaluateContext evaluateContext, @NotNull String name)
	{
		super(name);
		myEvaluateContext = evaluateContext;
	}

	@NotNull
	protected abstract JsValue getValue();

	@Override
	public void computeChildren(@NotNull XCompositeNode node)
	{
		XValueChildrenList valueChildrenList = new XValueChildrenList();

		JsValue value = getValue();
		if(value instanceof JsArray)
		{
			long length = ((JsArray) value).getLength();
			for(int i = 0; i < length; i++)
			{
				V8VariableValue.addValue(valueChildrenList, myEvaluateContext, ((JsArray) value).get(i));
			}
		}
		else if(value instanceof JsObject)
		{
			Collection<? extends JsVariable> properties = ((JsObject) value).getProperties();
			for(JsVariable property : properties)
			{
				V8VariableValue.addValue(valueChildrenList, myEvaluateContext, property);
			}
		}
		node.addChildren(valueChildrenList, true);
	}

	@NotNull
	protected Icon getIconForValue(JsValue value, JsValue.Type valueType)
	{
		if(value instanceof JsArray)
		{
			return AllIcons.Debugger.Db_array;
		}
		switch(valueType)
		{
			case TYPE_NUMBER:
			case TYPE_NULL:
			case TYPE_REGEXP:
			case TYPE_UNDEFINED:
			case TYPE_BOOLEAN:
			case TYPE_STRING:
				return AllIcons.Debugger.Db_primitive;
		}
		return AllIcons.Debugger.Value;
	}

	private static boolean canHaveChildren(JsValue value, JsValue.Type valueType)
	{
		if(value instanceof JsArray)
		{
			return ((JsArray) value).getLength() > 0;
		}
		switch(valueType)
		{
			case TYPE_NUMBER:
			case TYPE_NULL:
			case TYPE_REGEXP:
			case TYPE_UNDEFINED:
			case TYPE_STRING:
			case TYPE_BOOLEAN:
				return false;
		}
		return true;
	}

	@Override
	public void computePresentation(@NotNull XValueNode valueNode, @NotNull XValuePlace xValuePlace)
	{
		final JsValue value = getValue();
		final JsValue.Type valueType = value.getType();

		valueNode.setPresentation(getIconForValue(value, valueType), new XValuePresentation()
		{
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
						if(value instanceof JsArray)
						{
							return StringUtil.decapitalize(((JsArray) value).getClassName()) + "[" + ((JsArray) value).getLength() + "]";
						}
						else if(value instanceof JsObject)
						{
							return StringUtil.decapitalize(((JsObject) value).getClassName());
						}
						return null;
				}
			}

			@Override
			public void renderValue(@NotNull XValueTextRenderer textRenderer)
			{
				switch(value.getType())
				{
					case TYPE_NUMBER:
						textRenderer.renderValue(value.getValueString());
						break;
					case TYPE_STRING:
						textRenderer.renderStringValue(value.getValueString());
						break;
					case TYPE_FUNCTION:
						break;
					case TYPE_BOOLEAN:
						textRenderer.renderValue(value.getValueString());
						break;
					case TYPE_ERROR:
						break;
					case TYPE_REGEXP:
						textRenderer.renderStringValue(value.getValueString());
						break;
					case TYPE_DATE:
						textRenderer.renderValue(value.getValueString());
						break;
					case TYPE_UNDEFINED:
						textRenderer.renderValue("undefined");
						break;
					case TYPE_NULL:
						textRenderer.renderValue("null");
						break;
				}
			}
		}, canHaveChildren(value, valueType));
	}
}
