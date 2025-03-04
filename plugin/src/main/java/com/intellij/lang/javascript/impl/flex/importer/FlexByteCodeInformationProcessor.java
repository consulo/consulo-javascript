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
interface FlexByteCodeInformationProcessor {
    void dumpStat(@Nonnull String stat);

    void hasError(@Nonnull String error);

    void append(@Nonnull String str);

    void processMultinameAsPackageName(@Nonnull Multiname name, @Nullable String parentName, boolean referenceNameRequested);

    void dumpToplevelAnonymousMethod(@Nonnull Abc abc, @Nonnull MethodInfo m);

    void dumpTopLevelTraits(@Nonnull Abc abc, @Nonnull Traits t, String indent);

    boolean doDumpMember(@Nonnull MemberInfo memberInfo);

    void appendMethodSeparator();

    void appendFieldSeparator();

    String getAbcInSwfIndent();

    boolean doDumpMetaData(@Nonnull MetaData md);

    String REST_PARAMETER_TYPE = "...";

    void processParameter(@Nonnull String name, @Nullable Multiname type, String parentName, @Nullable Multiname value, boolean rest);

    boolean doStarTypeDumpInExtends();

    boolean doStarMetaAttrNameDump();

    void setProcessingInterface(boolean anInterface);

    String getParentName(MemberInfo member);

    void processVariable(SlotInfo info, String indent, String attr);

    void processFunction(MethodInfo methodInfo, boolean referenceNameRequested, Abc abc, String indent, String attr);

    void processMetadata(MetaData metaData);

    void processClass(SlotInfo slotInfo, Abc abc, String attr, String indent);
}
