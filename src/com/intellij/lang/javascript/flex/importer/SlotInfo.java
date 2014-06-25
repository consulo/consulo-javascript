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

package com.intellij.lang.javascript.flex.importer;

/**
 * @author Maxim.Mossienko
 *         Date: Oct 20, 2008
 *         Time: 7:01:03 PM
 */
class SlotInfo extends MemberInfo
{
	Multiname type;
	Object value;

	@Override
	void dump(Abc abc, String indent, String attr, final FlexByteCodeInformationProcessor processor)
	{
		if(!processor.doDumpMember(this))
		{
			return;
		}

		if(kind == Abc.TRAIT_Const || kind == Abc.TRAIT_Slot)
		{
			processor.processVariable(this, indent, attr);
			return;
		}

		processor.processClass(this, abc, attr, indent);
	}

	boolean isInterfaceClass()
	{
		if(!(value instanceof Traits))
		{
			return false;
		}
		return (((Traits) value).itraits.flags & Abc.CLASS_FLAG_interface) != 0;
	}

	public boolean isConst()
	{
		return Abc.traitKinds[kind].indexOf("const") != -1;
	}
}
