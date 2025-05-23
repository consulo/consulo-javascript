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

package com.intellij.lang.javascript.psi;

import consulo.annotation.access.RequiredWriteAction;
import consulo.language.util.IncorrectOperationException;

/**
 * @author max
 * @since 2005-01-30
 */
public interface JSStatement extends JSSourceElement {
    JSStatement[] EMPTY = new JSStatement[0];

    @RequiredWriteAction
    JSStatement addStatementBefore(JSStatement toAdd) throws IncorrectOperationException;

    @RequiredWriteAction
    JSStatement addStatementAfter(JSStatement toAdd) throws IncorrectOperationException;

    @RequiredWriteAction
    JSStatement replace(JSStatement with);
}
