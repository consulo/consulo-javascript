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

package consulo.javascript.run.debug.v8;

import consulo.application.AllIcons;
import consulo.execution.debug.frame.*;
import consulo.execution.debug.frame.presentation.XValuePresentation;
import consulo.ui.image.Image;
import org.chromium.sdk.*;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import java.util.Collection;

/**
 * @author VISTALL
 * @since 20.03.14
 */
public abstract class V8BaseVariableValue extends XNamedValue
{
	@Nonnull
	protected final JsEvaluateContext myEvaluateContext;

	protected V8BaseVariableValue(@Nonnull JsEvaluateContext evaluateContext, @Nonnull String name)
	{
		super(name);
		myEvaluateContext = evaluateContext;
	}

	@Nonnull
	protected abstract JsValue getValue();

	@Override
	public void computeChildren(@Nonnull XCompositeNode node)
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

	@Nonnull
	protected Image getIconForValue(JsValue value, JsValue.Type valueType)
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
	public void computePresentation(@Nonnull XValueNode valueNode, @Nonnull XValuePlace xValuePlace)
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
							return ((JsArray) value).getClassName() + "[" + ((JsArray) value).getLength() + "]";
						}
						else if(value instanceof JsObject)
						{
							return ((JsObject) value).getClassName();
						}
						return null;
				}
			}

			@Override
			public void renderValue(@Nonnull XValueTextRenderer textRenderer)
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
