/*
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Nov 15, 2006
 * Time: 4:47:30 PM
 */
package com.intellij.javascript.documentation;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public interface JSDocumentationProcessor
{
	enum MetaDocType
	{
		RETURN, CONSTRUCTOR, METHOD, PARAMETER, PRIVATE, PUBLIC, PROTECTED, STATIC, DESCRIPTION, FINAL, REQUIRES, TYPE, NAMESPACE,
		OPTIONAL_PARAMETERS, EVENT, NOTE, DEPRECATED, SEE, DEFAULT, EXTENDS, CLASS, FIELD
	}

	boolean needsPlainCommentData();

	boolean onCommentLine(@NotNull String line);

	boolean onPatternMatch(@NotNull MetaDocType type, @Nullable String matchName, @Nullable final String matchValue,
			@Nullable String remainingLineContent, @NotNull final String line, final String patternMatched);
}