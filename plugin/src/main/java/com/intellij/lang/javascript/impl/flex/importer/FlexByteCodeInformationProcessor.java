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

import org.jetbrains.annotations.NonNls;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author Maxim.Mossienko
 * Date: Oct 20, 2008
 * Time: 7:02:07 PM
 */
interface FlexByteCodeInformationProcessor {
    void dumpStat(@Nonnull @NonNls String stat);

    void hasError(@Nonnull String error);

    void append(@Nonnull @NonNls String str);

    void processMultinameAsPackageName(@Nonnull Multiname name, @Nullable String parentName, boolean referenceNameRequested);

    void dumpToplevelAnonymousMethod(final @Nonnull Abc abc, final @Nonnull MethodInfo m);

    void dumpTopLevelTraits(final @Nonnull Abc abc, final @Nonnull Traits t, final String indent);

    boolean doDumpMember(final @Nonnull MemberInfo memberInfo);

    void appendMethodSeparator();

    void appendFieldSeparator();

    String getAbcInSwfIndent();

    boolean doDumpMetaData(final @Nonnull MetaData md);

    String REST_PARAMETER_TYPE = "...";

    void processParameter(final @Nonnull String name, @Nullable Multiname type, String parentName, @Nullable Multiname value, boolean rest);

    boolean doStarTypeDumpInExtends();

    boolean doStarMetaAttrNameDump();

    void setProcessingInterface(final boolean anInterface);

    String getParentName(final MemberInfo member);

    void processVariable(SlotInfo info, String indent, String attr);

    void processFunction(MethodInfo methodInfo, boolean referenceNameRequested, Abc abc, String indent, String attr);

    void processMetadata(MetaData metaData);

    void processClass(SlotInfo slotInfo, Abc abc, String attr, String indent);
}
