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

import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.stubs.JSAttributeListStub;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.StubBase;

/**
 * @author Maxim.Mossienko
 * Date: Mar 26, 2008
 * Time: 11:29:19 PM
 */
public class JSAttributeListStubImpl extends StubBase<JSAttributeList> implements JSAttributeListStub {
    private static final int VISIBILITY_TAG_SHIFT = 0;
    private static final int VISIBILITY_TAG_MASK = 0x3;
    private static final int OVERRIDE_PROPERTY_SHIFT = 2;
    private static final int STATIC_PROPERTY_SHIFT = 3;
    private static final int DYNAMIC_PROPERTY_SHIFT = 4;
    private static final int NATIVE_PROPERTY_SHIFT = 5;
    private static final int FINAL_PROPERTY_SHIFT = 6;
    private static final int VIRTUAL_PROPERTY_SHIFT = 7;

    private int myFlags;
    private String myNamespace;

    public JSAttributeListStubImpl(String namespace, int flags, StubElement parent, IStubElementType elementType) {
        super(parent, elementType);
        myNamespace = namespace;
        myFlags = flags;
    }

    @Override
    public JSAttributeList.AccessType getAccessType() {
        int i = (myFlags >> VISIBILITY_TAG_SHIFT) & VISIBILITY_TAG_MASK;
        return types[i];
    }

    private static final JSAttributeList.AccessType[] types = JSAttributeList.AccessType.values();

    @Override
    public boolean hasModifier(JSAttributeList.ModifierType modifier) {
        return ((myFlags >> getFlagShift(modifier)) & 0x1) != 0;
    }

    @Override
    public String getNamespace() {
        return myNamespace;
    }

    @Override
    public int getFlags() {
        return myFlags;
    }

    public static int getFlags(JSAttributeList attributeList) {
        JSAttributeList.AccessType accessType = attributeList.getAccessType();
        int ord = accessType.ordinal();

        int flags = (ord << VISIBILITY_TAG_SHIFT);

        for (JSAttributeList.ModifierType type : JSAttributeList.ModifierType.values()) {
            flags = setFlag(flags, type, attributeList.hasModifier(type));
        }
        return flags;
    }

    private static int getFlagShift(JSAttributeList.ModifierType modifier) {
        int shift = -1;
        if (modifier == JSAttributeList.ModifierType.STATIC) {
            shift = STATIC_PROPERTY_SHIFT;
        }
        else if (modifier == JSAttributeList.ModifierType.DYNAMIC) {
            shift = DYNAMIC_PROPERTY_SHIFT;
        }
        else if (modifier == JSAttributeList.ModifierType.OVERRIDE) {
            shift = OVERRIDE_PROPERTY_SHIFT;
        }
        else if (modifier == JSAttributeList.ModifierType.NATIVE) {
            shift = NATIVE_PROPERTY_SHIFT;
        }
        else if (modifier == JSAttributeList.ModifierType.FINAL) {
            shift = FINAL_PROPERTY_SHIFT;
        }
        else if (modifier == JSAttributeList.ModifierType.VIRTUAL) {
            shift = VIRTUAL_PROPERTY_SHIFT;
        }
        if (shift == -1) {
            throw new IllegalArgumentException("Illegal modifier passed:" + modifier);
        }
        return shift;
    }

    private static int setFlag(int old, JSAttributeList.ModifierType modifier, boolean value) {
        if (value) {
            return old | (1 << getFlagShift(modifier));
        }
        else {
            return old & ~(1 << getFlagShift(modifier));
        }
    }
}