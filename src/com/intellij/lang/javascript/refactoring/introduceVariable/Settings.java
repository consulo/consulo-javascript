package com.intellij.lang.javascript.refactoring.introduceVariable;

import com.intellij.lang.javascript.refactoring.BaseIntroduceSettings;

/**
 * @author ven
 */
public interface Settings extends BaseIntroduceSettings
{
	enum IntroducedVarType
	{
		VAR, CONST, LET
	}

	IntroducedVarType getIntroducedVarType();
}
