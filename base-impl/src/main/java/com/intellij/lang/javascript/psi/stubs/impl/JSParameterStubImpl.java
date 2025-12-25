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

import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.stubs.JSParameterStub;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.StubElement;

/**
 * @author Maxim.Mossienko
 * Date: Mar 26, 2008
 * Time: 11:29:19 PM
 */
public class JSParameterStubImpl extends JSVariableStubBaseImpl<JSParameter> implements JSParameterStub {
    public static final int REST_MASK = LAST_USED_MASK << 1;
    public static final int OPTIONAL_MASK = LAST_USED_MASK << 2;

    public JSParameterStubImpl(
        String name,
        int flags,
        String type,
        String initial,
        String qName,
        StubElement parentStub,
        IStubElementType elementType
    ) {
        super(name, flags, type, initial, qName, parentStub, elementType);
    }

    public static int buildFlags(JSParameter clazz) {
        int i = JSVariableStubBaseImpl.buildFlags(clazz);
        return i | (clazz.isRest() ? REST_MASK : 0) | (clazz.isOptional() ? OPTIONAL_MASK : 0);
    }

    @Override
    public boolean isRest() {
        return (myFlags & REST_MASK) != 0;
    }

    @Override
    public boolean isOptional() {
        return (myFlags & OPTIONAL_MASK) != 0;
    }
}