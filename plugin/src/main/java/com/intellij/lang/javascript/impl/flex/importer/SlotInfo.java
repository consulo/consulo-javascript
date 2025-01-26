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

/**
 * @author Maxim.Mossienko
 * @since 2008-10-20
 */
class SlotInfo extends MemberInfo {
    Multiname type;
    Object value;

    @Override
    void dump(Abc abc, String indent, String attr, FlexByteCodeInformationProcessor processor) {
        if (!processor.doDumpMember(this)) {
            return;
        }

        if (kind == Abc.TRAIT_Const || kind == Abc.TRAIT_Slot) {
            processor.processVariable(this, indent, attr);
            return;
        }

        processor.processClass(this, abc, attr, indent);
    }

    boolean isInterfaceClass() {
        return value instanceof Traits traits && (traits.itraits.flags & Abc.CLASS_FLAG_interface) != 0;
    }

    public boolean isConst() {
        return Abc.traitKinds[kind].contains("const");
    }
}
