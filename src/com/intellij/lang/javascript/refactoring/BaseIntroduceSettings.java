package com.intellij.lang.javascript.refactoring;

/**
 * @author ven
 */
public interface BaseIntroduceSettings
{
	boolean isReplaceAllOccurences();

	String getVariableName();

	String getVariableType();
}