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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author Maxim.Mossienko
 * @since 2008-10-20
 */
class AbcDumper extends AbstractDumpProcessor {
    private boolean dumpCode;

    public AbcDumper(boolean _dumpCode) {
        dumpCode = _dumpCode;
    }

    @Override
    public void dumpStat(@Nonnull String stat) {
        sb.append(stat);
    }

    @Override
    public void hasError(@Nonnull String error) {
        sb.append(error);
    }

    @Override
    protected String appendModifiers(MemberInfo member, String attr) {
        attr += member instanceof MethodInfo methodInfo && (methodInfo.flags & Abc.NATIVE) != 0 ? "native " : "";
        return attr;
    }

    @Override
    public void processMultinameAsPackageName(
        @Nonnull Multiname name,
        @Nullable String parentName,
        boolean referenceNameRequested
    ) {
        append(name.toString());
    }

    @Override
    public void dumpToplevelAnonymousMethod(@Nonnull Abc abc, @Nonnull MethodInfo m) {
        m.dump(abc, "", "", this);
    }

    @Override
    public void dumpTopLevelTraits(@Nonnull Abc abc, @Nonnull Traits t, String indent) {
        sb.append(indent).append(t.name).append("\n");
        t.dump(abc, indent, "", this);
        t.init.dump(abc, indent, "", this);
    }

    @Override
    public boolean doDumpMember(@Nonnull MemberInfo memberInfo) {
        return true;
    }

    @Override
    public void appendMethodSeparator() {
        append("\n");
    }

    @Override
    public void appendFieldSeparator() {
        append("");
    }

    @Override
    public String getAbcInSwfIndent() {
        return "  ";
    }

    @Override
    public void processValue(Multiname type, Object value) {
        append(" = " + String.valueOf(value instanceof String ? ('"' + value.toString() + '"') : value));
    }

    @Override
    public boolean doDumpMetaData(@Nonnull MetaData md) {
        return true;
    }

    @Override
    public void processParameter(
        @Nonnull String name,
        @Nullable Multiname type,
        String parentName,
        @Nullable Multiname value,
        boolean rest
    ) {
        processMultinameAsPackageName(type, parentName, true);
    }

    @Override
    public boolean doStarTypeDumpInExtends() {
        return true;
    }

    @Override
    public boolean doStarMetaAttrNameDump() {
        return true;
    }

    @Override
    public void setProcessingInterface(boolean anInterface) {
    }

    @Override
    protected boolean dumpRestParameter() {
        return false;
    }

    @Override
    public void processFunction(MethodInfo methodInfo, boolean referenceNameRequested, Abc abc, String indent, String attr) {
        super.processFunction(methodInfo, referenceNameRequested, abc, indent, attr);
        append("\t/* disp_id " + methodInfo.id + "*/");

        if (!referenceNameRequested) { // !verbose -> anonymouse
            append("\n");
            if (dumpCode && methodInfo.code != null) {
                methodInfo.dumpCode(abc, indent, this);
            }
        }
    }

    @Override
    public void processVariable(SlotInfo info, String indent, String attr) {
        super.processVariable(info, indent, attr);
        append("\t/* slot_id " + info.id + " */\n");
    }
}
