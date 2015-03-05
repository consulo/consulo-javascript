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

/**
 * @author Maxim.Mossienko
 */
@Deprecated
public final class DialectOptionHolder
{
	public final boolean isECMAL4Level;
	public final boolean isJavaScript1_6_OrBetter;
	public final boolean hasE4X;
	public final boolean isJavaScript1_7_OrBetter;
	public final boolean isJavaScript1_8_OrBetter;
	public final boolean isGwt;

	public DialectOptionHolder(boolean _ecma, boolean _gwt)
	{
		this(_ecma, _gwt, true);
	}

	public DialectOptionHolder(boolean _ecma, boolean _gwt, final boolean _e4x)
	{
		isECMAL4Level = _ecma;
		isGwt = _gwt;
		isJavaScript1_7_OrBetter = _e4x;
		isJavaScript1_8_OrBetter = _e4x;
		isJavaScript1_6_OrBetter = _e4x;
		hasE4X = _e4x;

		assert !isGwt || !isECMAL4Level;
	}
}
