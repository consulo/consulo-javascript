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
