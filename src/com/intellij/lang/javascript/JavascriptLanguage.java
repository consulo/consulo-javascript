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

package com.intellij.lang.javascript;

import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;

/**
 * User: max
 * Date: Jan 27, 2005
 * Time: 6:03:49 PM
 */
@Deprecated
public class JavascriptLanguage
{
	@Deprecated
	public static final DialectOptionHolder DIALECT_OPTION_HOLDER = new DialectOptionHolder(false, false);
	public static final JavaScriptLanguage INSTANCE = JavaScriptLanguage.INSTANCE;
}
