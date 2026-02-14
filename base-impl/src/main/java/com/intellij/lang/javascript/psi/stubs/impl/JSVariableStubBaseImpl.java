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

import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.stubs.JSVariableStubBase;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.StubElement;

/**
 * @author Maxim.Mossienko
 * Date: Mar 26, 2008
 * Time: 11:29:19 PM
 */
public abstract class JSVariableStubBaseImpl<T extends JSVariable> extends JSQualifiedObjectStubBase<T> implements JSVariableStubBase<T> {
    private String myTypeString;
    private static final int DEPRECATED_MASK = 1;
    public static final int CONST_MASK = 2;
    private static final int LOCAL_MASK = 4;
    static final int LAST_USED_MASK = LOCAL_MASK;
    private String myInitializerText;

    public JSVariableStubBaseImpl(
        String name,
        int flags,
        String type,
        String initial,
        String qName,
        StubElement parentStub,
        IStubElementType elementType
    ) {
        super(name, flags, qName, parentStub, elementType);
        myTypeString = type;
        myInitializerText = initial;
    }

    public static int buildFlags(JSVariable clazz) {
        return (clazz.isDeprecated() ? DEPRECATED_MASK : 0) | (clazz.isConst() ? CONST_MASK : clazz.isLocal() ? LOCAL_MASK : 0);
    }

    @Override
    public String getTypeString() {
        return myTypeString;
    }

    @Override
    public boolean isDeprecated() {
        return (myFlags & DEPRECATED_MASK) != 0;
    }

    @Override
    public boolean isConst() {
        return (myFlags & CONST_MASK) != 0;
    }

    @Override
    public String getInitializerText() {
        return myInitializerText;
    }

    @Override
    public boolean isLocal() {
        return (myFlags & LOCAL_MASK) != 0;
    }
}