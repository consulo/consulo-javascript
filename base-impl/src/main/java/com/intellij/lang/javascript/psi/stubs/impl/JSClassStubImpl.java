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

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.IStubElementType;

/**
 * @author Maxim.Mossienko
 * Date: Mar 26, 2008
 * Time: 7:11:48 PM
 */
public class JSClassStubImpl extends JSQualifiedObjectStubBase<JSClass> implements JSClassStub {
    public static final int INTERFACE_MASK = 1;
    private static final int DEPRECATED_MASK = 4;

    public JSClassStubImpl(
        String name,
        int flags,
        String qName,
        final StubElement parentStub,
        final IStubElementType<JSClassStub, JSClass> elementType
    ) {
        super(name, flags, qName, parentStub, elementType);
    }

    public static int getFlags(JSClass jsClass) {
        return (jsClass.isInterface() ? INTERFACE_MASK : 0) | (jsClass.isDeprecated() ? DEPRECATED_MASK : 0);
    }

    @Override
    public boolean isInterface() {
        return (myFlags & INTERFACE_MASK) != 0;
    }

    @Override
    public boolean isDeprecated() {
        return (myFlags & DEPRECATED_MASK) != 0;
    }
}
