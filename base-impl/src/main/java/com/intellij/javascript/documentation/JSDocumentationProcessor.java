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

/*
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Nov 15, 2006
 * Time: 4:47:30 PM
 */
package com.intellij.javascript.documentation;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public interface JSDocumentationProcessor
{
	enum MetaDocType
	{
		RETURN, CONSTRUCTOR, METHOD, PARAMETER, PRIVATE, PUBLIC, PROTECTED, STATIC, DESCRIPTION, FINAL, REQUIRES, TYPE, NAMESPACE,
		OPTIONAL_PARAMETERS, EVENT, NOTE, DEPRECATED, SEE, DEFAULT, EXTENDS, CLASS, FIELD
	}

	boolean needsPlainCommentData();

	boolean onCommentLine(@Nonnull String line);

	boolean onPatternMatch(@Nonnull MetaDocType type, @Nullable String matchName, @Nullable final String matchValue,
			@Nullable String remainingLineContent, @Nonnull final String line, final String patternMatched);
}