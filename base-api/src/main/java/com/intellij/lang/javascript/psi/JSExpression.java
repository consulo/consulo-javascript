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

package com.intellij.lang.javascript.psi;

import org.jetbrains.annotations.NotNull;
import com.intellij.util.ArrayFactory;
import consulo.annotations.RequiredReadAction;
import consulo.javascript.lang.psi.JavaScriptType;

/**
 * @author max
 * @since  6:46:19 PM Jan 30, 2005
 */
public interface JSExpression extends JSElement
{
	public static final JSExpression[] EMPTY_ARRAY = new JSExpression[0];

	public static ArrayFactory<JSExpression> ARRAY_FACTORY = new ArrayFactory<JSExpression>()
	{
		@NotNull
		@Override
		public JSExpression[] create(int count)
		{
			return count == 0 ? EMPTY_ARRAY : new JSExpression[count];
		}
	};

	@NotNull
	JSExpression replace(JSExpression other);

	@NotNull
	@RequiredReadAction
	JavaScriptType getType();
}
