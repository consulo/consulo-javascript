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

import jakarta.annotation.Nonnull;
import org.jetbrains.annotations.NonNls;

/**
 * @author Maxim.Mossienko
 *         Date: Oct 20, 2008
 *         Time: 7:02:13 PM
 */
abstract class AbstractDumpProcessor implements FlexByteCodeInformationProcessor
{
	protected
	@NonNls
	StringBuilder sb = new StringBuilder();
	private boolean firstMetaDataMember;

	String getResult()
	{
		return sb.toString();
	}

	@Override
	public void append(@Nonnull @NonNls String str)
	{
		sb.append(str);
	}

	@Override
	public String getParentName(final MemberInfo member)
	{
		String parentName = null;

		if(member.parentTraits != null)
		{
			if(member.parentTraits.name instanceof Multiname)
			{
				final Multiname multiname = (Multiname) member.parentTraits.name;

				if(multiname.hasNamespace())
				{
					parentName = multiname.name;
				}
			}
			if(parentName == null)
			{
				parentName = member.parentTraits.name.toString().replaceAll("::", ".");
			}
		}
		return parentName;
	}

	public void addMetaDataValue(String s, String s1)
	{
		append(firstMetaDataMember ? "(" : ",");
		firstMetaDataMember = false;
		if(s != null)
		{
			append(s);
			append("=");
		}
		append(s1);
	}

	@Override
	public void processVariable(SlotInfo info, String indent, String attr)
	{
		appendFieldSeparator();

		attr = appendModifiers(info, attr);
		processModifierList(info, attr, indent);
		append(indent + attr);
		processMemberKindAndName(info);

		append(":");
		processMultinameAsPackageName(info.type, info.parentTraits.getClassName(), true);
		if(info.value != null)
		{
			processValue(info.type, info.value);
		}
	}

	@Override
	public void processFunction(MethodInfo methodInfo, boolean referenceNameRequested, Abc abc, String indent, String attr)
	{
		if(!referenceNameRequested)
		{
			appendMethodSeparator();
		}

		attr = appendModifiers(methodInfo, attr);
		processModifierList(methodInfo, attr, indent);

		append(indent + attr);

		processMemberKindAndName(methodInfo);
		final String parentName = methodInfo.getParentName();
		processArgumentList(methodInfo, parentName);
		append(":");
		processMultinameAsPackageName(methodInfo.returnType, parentName, referenceNameRequested);
	}

	protected void processMemberKindAndName(@Nonnull final MemberInfo member)
	{
		append(Abc.traitKinds[member.kind]);
		append(" ");

		if(member.name != null)
		{
			processMultinameAsPackageName(member.name, member.parentTraits.getClassName(), false);
		}
		else
		{
			append("undefined");
		}
	}

	protected abstract String appendModifiers(MemberInfo methodInfo, String attr);

	protected abstract void processValue(Multiname type, Object value);

	protected void processArgumentList(MethodInfo methodInfo, String parentName)
	{
		append("(");

		for(int i = 0; i < methodInfo.paramTypes.length; ++i)
		{
			final Multiname paramType = methodInfo.paramTypes[i];
			final boolean restParameter = FlexByteCodeInformationProcessor.REST_PARAMETER_TYPE.equals(paramType.name);
			if(restParameter && !dumpRestParameter())
			{
				break; // original one do not dump
			}
			if(i > 0)
			{
				append(",");
			}

			processParameter(methodInfo.paramNames != null ? methodInfo.paramNames[i] : "a" + (i > 0 ? "" + (i + 1) : ""), methodInfo.paramTypes[i],
					parentName, methodInfo.optionalValues != null && i < methodInfo.optionalValues.length ? methodInfo.optionalValues[i] : null, restParameter);
		}

		append(")");
	}

	protected abstract boolean dumpRestParameter();

	@Override
	public void processMetadata(MetaData metaData)
	{
		append("[");
		append(metaData.name);
		firstMetaDataMember = true;

		for(String n : metaData.keySet())
		{
			addMetaDataValue(!"*".equals(n) || doStarMetaAttrNameDump() ? n : null, '"' + metaData.get(n) + '"');
		}

		if(!firstMetaDataMember)
		{
			append(")");
		}
		append("]");
	}

	@Override
	public void processClass(SlotInfo slotInfo, Abc abc, String attr, String indent)
	{
		append("\n");

		@NonNls String def;
		final boolean isInterface = slotInfo.isInterfaceClass();
		if(isInterface)
		{
			def = "interface";
		}
		else
		{
			def = "class";
		}

		if(!doStarTypeDumpInExtends())
		{
			final String ns = slotInfo.name.hasNamespace() ? slotInfo.name.getNsName() : null;

			if(ns != null && ns.length() > 0)
			{
				attr += ns;
			}
			else
			{
				attr += "public";
			}
			attr += " ";
		}

		Traits ct = (Traits) slotInfo.value;
		Traits it = ct.itraits;
		if(!isInterface)
		{
			if((it.flags & Abc.CLASS_FLAG_final) != 0)
			{
				attr += "final ";
			}
			if((it.flags & Abc.CLASS_FLAG_sealed) == 0)
			{
				attr += "dynamic ";
			}
		}

		processModifierList(slotInfo, attr, indent);

		append(indent + attr + def + " ");
		processMultinameAsPackageName(slotInfo.name, null, true);
		dumpExtendsList(it);

		append("\n");
		String oldindent = indent;
		indent += Abc.TAB;

		dumpInterfacesList(indent, it, isInterface);
		append(oldindent + "{\n");
		setProcessingInterface(isInterface);

		it.init.dump(abc, indent, "", this);
		it.dump(abc, indent, "", this);
		ct.dump(abc, indent, "static ", this);
		ct.init.dump(abc, indent, "static ", this);
		append(oldindent + "}\n");
		setProcessingInterface(false);
	}

	protected void processModifierList(MemberInfo memberInfo, String attr, String indent)
	{
		memberInfo.dumpMetaData(indent, this);
	}

	protected void dumpExtendsList(Traits it)
	{
		if(!it.base.isStarReference() || doStarTypeDumpInExtends())
		{
			append(" extends ");
			processMultinameAsPackageName(it.base, null, true);
		}
	}

	protected void dumpInterfacesList(String indent, Traits it, boolean anInterface)
	{
		if(it.interfaces.length > 0)
		{
			append(indent + (anInterface && this instanceof AS3InterfaceDumper ? "extends " : "implements "));
			boolean first = true;

			for(Multiname name : it.interfaces)
			{
				if(!first)
				{
					append(",");
				}
				first = false;
				processMultinameAsPackageName(name, null, true);
			}
			append("\n");
		}
	}

	protected static String quote(final String s)
	{
		if(s.length() == 0)
		{
			return s;
		}
		final StringBuilder b = new StringBuilder(s.length());

		for(int i = 0; i < s.length(); ++i)
		{
			final char ch = s.charAt(i);

			if(ch == '\\' || ch == '"')
			{
				b.append('\\');
			}
			else if(ch == '\n')
			{
				b.append("\\n");
				continue;
			}
			else if(ch == '\r')
			{
				b.append("\\r");
				continue;
			}
			b.append(ch);
		}
		return b.toString();
	}
}
