/*
 * Copyright 2000-2006 JetBrains s.r.o.
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

import gnu.trove.TObjectIntHashMap;

import java.io.DataOutputStream;

import com.intellij.openapi.project.Project;

/**
 * @by Maxim.Mossienko
 */
final class SerializationContext
{
	final DataOutputStream outputStream;
	final TObjectIntHashMap<String> myNames = new TObjectIntHashMap<String>();
	final TObjectIntHashMap<JSNamespace> myNameSpaces = new TObjectIntHashMap<JSNamespace>();
	final TObjectIntHashMap<JSPackage> myPackages = new TObjectIntHashMap<JSPackage>();
	final JSTypeEvaluateManager typeEvaluateManager;
	final BrowserSupportManager browserSupportManager;
	final JavaScriptIndex myIndex;
	private int myFilesCount;

	SerializationContext(DataOutputStream _outputStream, Project project, int filesCount)
	{
		outputStream = _outputStream;
		typeEvaluateManager = JSTypeEvaluateManager.getInstance(project);
		browserSupportManager = BrowserSupportManager.getInstance(project);
		myIndex = JavaScriptIndex.getInstance(project);
		myFilesCount = filesCount;
	}

	public void addName(final String name)
	{
		if(!myNames.contains(name))
		{
			myNames.put(name, myNames.size() + 1);
		}
	}

	public void decFileCount()
	{
		--myFilesCount;
	}

	public int getFilesCount()
	{
		return myFilesCount;
	}
}
