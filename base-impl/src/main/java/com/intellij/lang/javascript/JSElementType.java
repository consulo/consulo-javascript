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

package com.intellij.lang.javascript;

import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.ast.IElementType;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 27, 2005
 * Time: 6:38:56 PM
 */
public class JSElementType extends IElementType
{
	public JSElementType(@NonNls @Nonnull String debugName)
	{
		super(debugName, JavaScriptLanguage.INSTANCE);
	}
}
