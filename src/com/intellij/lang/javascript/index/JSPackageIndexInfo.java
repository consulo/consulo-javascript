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

package com.intellij.lang.javascript.index;

import org.jetbrains.annotations.NotNull;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: 11.03.2009
 * Time: 15:13:23
 * To change this template use File | Settings | File Templates.
 */
public class JSPackageIndexInfo
{
	public final String name;
	public final Kind kind;

	public JSPackageIndexInfo(@NotNull String name, @NotNull Kind kind)
	{
		this.kind = kind;
		this.name = name;
	}

	public boolean isEquivalentTo(@NotNull String el, @NotNull Kind currentKind)
	{
		return kind == currentKind && name.equals(el);
	}

	public enum Kind
	{
		PACKAGE, CLASS, VARIABLE, FUNCTION
	}

	@Override
	public boolean equals(Object o)
	{
		if(this == o)
		{
			return true;
		}
		if(o == null || getClass() != o.getClass())
		{
			return false;
		}

		JSPackageIndexInfo that = (JSPackageIndexInfo) o;

		if(kind != that.kind)
		{
			return false;
		}
		if(!name.equals(that.name))
		{
			return false;
		}

		return true;
	}

	@Override
	public int hashCode()
	{
		int result = name.hashCode();
		result = 31 * result + kind.hashCode();
		return result;
	}
}
