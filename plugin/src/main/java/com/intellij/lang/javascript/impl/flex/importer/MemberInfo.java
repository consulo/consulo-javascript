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

import javax.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 *         Date: Oct 20, 2008
 *         Time: 7:00:39 PM
 */
abstract class MemberInfo
{
	Traits parentTraits;
	int id;
	int kind;
	Multiname name;
	MetaData[] metadata;
	boolean isOverride;
	boolean isPublic;
	boolean isFinal;

	abstract void dump(Abc abc, String indent, String attr, final @Nonnull FlexByteCodeInformationProcessor processor);

	protected void dumpMetaData(String indent, final @Nonnull FlexByteCodeInformationProcessor processor)
	{
		if(metadata != null)
		{
			for(MetaData md : metadata)
			{
				if(processor.doDumpMetaData(md))
				{
					processor.append(indent);
					processor.processMetadata(md);
					processor.append("\n");
				}
			}
		}
	}

	String getParentName()
	{
		return parentTraits != null ? parentTraits.getClassName() : null;
	}
}
