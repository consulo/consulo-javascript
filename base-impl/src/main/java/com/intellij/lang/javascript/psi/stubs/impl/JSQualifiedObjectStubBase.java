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

import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.stubs.JSQualifiedStub;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.IStubElementType;

/**
 * @author Maxim.Mossienko
 * Date: Mar 26, 2008
 * Time: 7:11:48 PM
 */
abstract class JSQualifiedObjectStubBase<T extends JSQualifiedNamedElement> extends JSNamedObjectStubBase<T> implements JSQualifiedStub<T> {
    protected final String myQualifiedName;

    protected JSQualifiedObjectStubBase(
        String name,
        int flags,
        String qName,
        StubElement parent,
        IStubElementType elementType
    ) {
        super(name, flags, parent, elementType);

        myQualifiedName = qName;
    }

    @Override
    public String getQualifiedName() {
        return myQualifiedName;
    }
}