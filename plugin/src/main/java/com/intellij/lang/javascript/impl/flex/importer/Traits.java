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

package com.intellij.lang.javascript.impl.flex.importer;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Maxim.Mossienko
 * Date: Oct 20, 2008
 * Time: 7:01:09 PM
 */
class Traits {
    Object name;
    MethodInfo init;
    Traits itraits;
    Multiname base;
    int flags;
    String protectedNs;
    Multiname interfaces[];
    Map<String, MemberInfo> names = new LinkedHashMap<String, MemberInfo>();
    Map<Integer, SlotInfo> slots = new LinkedHashMap<Integer, SlotInfo>();
    Map<Integer, MethodInfo> methods = new LinkedHashMap<Integer, MethodInfo>();
    Map<Integer, MemberInfo> members = new LinkedHashMap<Integer, MemberInfo>();

    @Override
    public String toString() {
        return name.toString();
    }

    public void dump(Abc abc, String indent, String attr, final FlexByteCodeInformationProcessor processor) {
        for (MemberInfo m : members.values()) {
            m.dump(abc, indent, attr, processor);
        }
    }

    String getClassName() {
        final String s = name.toString();
        if (s.endsWith(Abc.$)) {
            return s.substring(0, s.length() - 1);
        }
        return s;
    }
}
