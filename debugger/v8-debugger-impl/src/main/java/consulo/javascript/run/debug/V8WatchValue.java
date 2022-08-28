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

package consulo.javascript.run.debug;

import javax.annotation.Nonnull;

import org.chromium.sdk.JsEvaluateContext;
import org.chromium.sdk.JsValue;
import consulo.application.AllIcons;
import consulo.ui.image.Image;

/**
 * @author VISTALL
 * @since 05.12.2015
 */
public class V8WatchValue extends V8BaseVariableValue
{
	private JsValue myValue;

	public V8WatchValue(@Nonnull JsEvaluateContext evaluateContext, @Nonnull String text, @Nonnull JsValue value)
	{
		super(evaluateContext, text);
		myValue = value;
	}

	@Nonnull
	@Override
	protected Image getIconForValue(JsValue value, JsValue.Type valueType)
	{
		return AllIcons.Debugger.Watch;
	}

	@Nonnull
	@Override
	protected JsValue getValue()
	{
		return myValue;
	}
}
