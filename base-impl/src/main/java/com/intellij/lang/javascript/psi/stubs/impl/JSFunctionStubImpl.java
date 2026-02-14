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

import com.intellij.lang.javascript.psi.JSFunction;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import consulo.language.psi.stub.StubElement;

/**
 * @author Maxim.Mossienko
 * Date: Mar 26, 2008
 * Time: 7:11:48 PM
 */
public class JSFunctionStubImpl extends JSQualifiedObjectStubBase<JSFunction> implements JSFunctionStub {
    private String myReturnType;
    public static final int GET_PROPERTY_MASK = 1;
    public static final int SET_PROPERTY_MASK = 2;
    public static final int CONSTRUCTOR_MASK = 4;
    private static final int DEPRECATED_MASK = 8;
    private static final int REFERENCES_ARGUMENTS_MASK = 16;

    public JSFunctionStubImpl(
        String name,
        int flags,
        String qName,
        String returnType,
        StubElement parentStub,
        JSStubElementType elementType
    ) {
        super(name, flags, qName, parentStub, elementType);
        myReturnType = returnType;
    }

    public static int buildFlags(JSFunction clazz) {
        int val = clazz.isConstructor()
            ? CONSTRUCTOR_MASK
            : clazz.isGetProperty()
            ? GET_PROPERTY_MASK
            : clazz.isSetProperty()
            ? SET_PROPERTY_MASK
            : 0;
        return val | (clazz.isDeprecated() ? DEPRECATED_MASK : 0) | (clazz.isReferencesArguments() ? REFERENCES_ARGUMENTS_MASK : 0);
    }

    @Override
    public boolean isGetProperty() {
        return (myFlags & GET_PROPERTY_MASK) != 0;
    }

    @Override
    public boolean isSetProperty() {
        return (myFlags & SET_PROPERTY_MASK) != 0;
    }

    @Override
    public boolean isConstructor() {
        return (myFlags & CONSTRUCTOR_MASK) != 0;
    }

    @Override
    public boolean isDeprecated() {
        return (myFlags & DEPRECATED_MASK) != 0;
    }

    @Override
    public boolean isReferencesArguments() {
        return (myFlags & REFERENCES_ARGUMENTS_MASK) != 0;
    }

    @Override
    public String getReturnTypeString() {
        return myReturnType;
    }
}