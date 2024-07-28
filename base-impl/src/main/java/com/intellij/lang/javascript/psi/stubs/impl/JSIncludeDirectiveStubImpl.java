/*
 * Copyright 2000-2005 JetBrains s.r.o
 * Copyright 2013-2015 must-be.org
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

package com.intellij.lang.javascript.psi.stubs.impl;

import com.intellij.lang.javascript.psi.JSIncludeDirective;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSIncludeDirectiveStub;
import consulo.language.psi.stub.StubBase;
import consulo.language.psi.stub.StubElement;

/**
 * @author Maxim.Mossienko
 * Date: Jun 6, 2008
 * Time: 8:00:52 PM
 */
public class JSIncludeDirectiveStubImpl extends StubBase<JSIncludeDirective> implements JSIncludeDirectiveStub {
    private final String myIncludeText;

    public JSIncludeDirectiveStubImpl(
        final String includeText,
        final StubElement parentStub,
        final JSStubElementType<JSIncludeDirectiveStub, JSIncludeDirective> type
    ) {
        super(parentStub, type);
        myIncludeText = includeText;
    }

    @Override
    public String getIncludeText() {
        return myIncludeText;
    }
}