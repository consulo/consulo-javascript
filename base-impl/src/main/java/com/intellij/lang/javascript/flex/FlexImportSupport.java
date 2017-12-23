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

package com.intellij.lang.javascript.flex;

import gnu.trove.THashMap;

import java.util.Map;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSImportStatement;
import com.intellij.lang.javascript.psi.resolve.JSImportedElementResolveResult;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.util.ArrayUtil;

/**
 * @author Maxim.Mossienko
 */
public class FlexImportSupport
{
	// flex2.compiler.mxml.lang.StandardDefs
	private static
	@NonNls
	String[] standardMxmlImports = new String[]{
			"mx.styles.*",
			"mx.binding.*",
			"mx.core.mx_internal",
			"mx.core.IDeferredInstance",
			"mx.core.IFactory",
			"mx.core.IPropertyChangeNotifier",
			"mx.core.ClassFactory",
			"mx.core.DeferredInstanceFromClass",
			"mx.core.DeferredInstanceFromFunction",
	};

	private static
	@NonNls
	String[] airOnlyImplicitImports = new String[]{
			"flash.data.*",
			"flash.desktop.*",
			"flash.filesystem.*",
			"flash.html.*",
			"flash.html.script.*"
	};

	private static
	@NonNls
	String[] implicitImports = new String[]{
			"flash.accessibility.*",
			"flash.debugger.*",
			"flash.display.*",
			"flash.errors.*",
			"flash.events.*",
			"flash.external.*",
			"flash.filters.*",
			"flash.geom.*",
			"flash.media.*",
			"flash.net.*",
			"flash.printing.*",
			"flash.profiler.*",
			"flash.system.*",
			"flash.text.*",
			"flash.ui.*",
			"flash.utils.*",
			"flash.xml.*"
	};

	private static final Map<String, Object> implicitImportListMap = new THashMap<String, Object>();
	private static final Map<String, Object> mxmlImportListMap = new THashMap<String, Object>();

	static
	{
		fillMapFromImportsArray(implicitImports, implicitImportListMap);
		fillMapFromImportsArray(standardMxmlImports, mxmlImportListMap);
	}

	private static void fillMapFromImportsArray(final String[] strings, final Map<String, Object> map)
	{
		for(String s : strings)
		{
			doAppendToMap(map, s);
		}
	}

	public static void appendToMap(final Map<String, Object> map, final JSImportStatement importStatement)
	{
		doAppendToMap(map, importStatement);
	}

	private static void doAppendToMap(final Map<String, Object> map, final Object stringOrStatement)
	{
		String s = stringOrStatement instanceof String ? (String) stringOrStatement : ((JSImportStatement) stringOrStatement).getImportText();

		final int index = s.lastIndexOf('.');
		if(index == -1)
		{
			return; // nothing to import, global symbol
		}
		final String key = s.substring(index + 1);
		final Object o = map.get(key);

		if(o == null)
		{
			map.put(key, stringOrStatement);
		}
		else if(o instanceof Object[])
		{
			map.put(key, ArrayUtil.append((Object[]) o, stringOrStatement));
		}
		else
		{
			map.put(key, new Object[]{
					stringOrStatement,
					o
			});
		}
	}

	public static JSImportedElementResolveResult resolveTypeNameUsingImplicitImports(@NotNull String referenceName, @NotNull JSFile file)
	{
		final PsiElement context = file.getContext();

		if(context != null)
		{
			JSImportedElementResolveResult expression = tryFindInMap(referenceName, file, implicitImportListMap, null);
			if(expression != null)
			{
				return expression;
			}
			expression = tryFindInMap(referenceName, file, mxmlImportListMap, null);
			if(expression != null)
			{
				return expression;
			}
		}
		return null;
	}


	public static JSImportedElementResolveResult tryFindInMap(final String referenceName, final PsiNamedElement file, final Map<String, Object> map,
			String qName)
	{
		JSImportedElementResolveResult resolveResult = tryEntry(map.get(referenceName), referenceName, file, qName);
		if(resolveResult == null)
		{
			resolveResult = tryEntry(map.get("*"), referenceName, file, qName);
		}
		return resolveResult;
	}

	public static JSImportedElementResolveResult tryFindInMap(final String referenceName, final PsiNamedElement file, final Map<String, Object> map)
	{
		return tryFindInMap(referenceName, file, map, null);
	}

	private static JSImportedElementResolveResult tryEntry(Object entry, String referenceName, PsiNamedElement file, String qName)
	{
		if(entry == null)
		{
			return null;
		}
		else if(entry instanceof Object[])
		{
			for(Object entryItem : (Object[]) entry)
			{
				JSImportedElementResolveResult expression = tryFindClass(referenceName, file, entryItem, qName);
				if(expression != null)
				{
					return expression;
				}
			}
			return null;
		}
		else
		{
			return tryFindClass(referenceName, file, entry, qName);
		}
	}

	private static JSImportedElementResolveResult tryFindClass(final String referenceName, final PsiNamedElement file, final Object entry, String qName)
	{
		String importString = entry instanceof String ? (String) entry : ((JSImportStatement) entry).getImportText();

		String nameToTry = importString.endsWith(".*") ? importString.substring(0, importString.length() - 1) + referenceName : importString;
		if(qName != null && !qName.equals(nameToTry))
		{
			return null;
		}

		PsiElement element = JSResolveUtil.findClassByQName(nameToTry, file);

		if(element != null)
		{
			return new JSImportedElementResolveResult(nameToTry, element, entry instanceof JSImportStatement ? (JSImportStatement) entry : null);
		}

		return null;
	}
}
