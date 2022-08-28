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

package com.intellij.lang.javascript.impl.flex.importer;

/**
 * @author Maxim.Mossienko
 *         Date: Oct 20, 2008
 *         Time: 7:03:18 PM
 */
class Multiname
{
	String[] nsset;
	String name;

	Multiname(String[] nsset, String name)
	{
		this.nsset = nsset;
		this.name = name;
	}

	@Override
	public String toString()
	{
		String s = "";
		if(hasNotEmptyNs())
		{
			s += nsset[0] + "::";
		}
		s += name;

		return s;
	}

	boolean hasNotEmptyNs()
	{
		return nsset != null && nsset.length > 0 && nsset[0] != null && nsset[0].length() > 0;
	}

	public boolean hasNamespace()
	{
		return hasNotEmptyNs() && (nsset[0].startsWith("http://") || nsset[0].equals("private") || nsset[0].equals("__AS3__.vec") ||
				nsset[0].indexOf('$') != -1 || nsset[0].indexOf("/private:") != -1);
	}

	public String getNsName()
	{
		// TODO: would be nice to pickup namespace var names automatically
		return nsset[0].equals("http://adobe.com/AS3/2006/builtin") ? "AS3" : nsset[0].equals("http://www.adobe.com/2006/flex/mx/internal") ?
				"mx_internal" : nsset[0].equals("__AS3__.vec") ? "__AS3__$vec" : nsset[0].equals("http://www.adobe.com/2008/actionscript/Flash10/") ? "flash10"
				: nsset[0].equals("http://www.adobe.com/2006/actionscript/flash/objectproxy") ? "object_proxy" : nsset[0].equals("http://www.adobe" +
				".com/2006/actionscript/flash/proxy") ? "flash_proxy" : getValidNsName();
	}

	public String getValidNsName()
	{
		return makeIdentifier(nsset[0]);
	}

	private static String makeIdentifier(String s)
	{
		StringBuilder builder = new StringBuilder(s.length());
		for(int i = 0; i < s.length(); ++i)
		{
			char ch = s.charAt(i);
			if(!Character.isJavaIdentifierPart(ch))
			{
				ch = '_';
			}
			builder.append(ch);
		}
		return builder.toString();
	}

	public boolean isStarReference()
	{
		return "*".equals(name);
	}
}
