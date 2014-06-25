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

package com.intellij.lang.javascript.flex.importer;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * @author Maxim.Mossienko
 *         Date: Oct 20, 2008
 *         Time: 7:02:07 PM
 */
interface FlexByteCodeInformationProcessor
{
	void dumpStat(@NotNull @NonNls String stat);

	void hasError(@NotNull String error);

	void append(@NotNull @NonNls String str);

	void processMultinameAsPackageName(@NotNull Multiname name, @Nullable String parentName, boolean referenceNameRequested);

	void dumpToplevelAnonymousMethod(final @NotNull Abc abc, final @NotNull MethodInfo m);

	void dumpTopLevelTraits(final @NotNull Abc abc, final @NotNull Traits t, final String indent);

	boolean doDumpMember(final @NotNull MemberInfo memberInfo);

	void appendMethodSeparator();

	void appendFieldSeparator();

	String getAbcInSwfIndent();

	boolean doDumpMetaData(final @NotNull MetaData md);

	String REST_PARAMETER_TYPE = "...";

	void processParameter(final @NotNull String name, @Nullable Multiname type, String parentName, @Nullable Multiname value, boolean rest);

	boolean doStarTypeDumpInExtends();

	boolean doStarMetaAttrNameDump();

	void setProcessingInterface(final boolean anInterface);

	String getParentName(final MemberInfo member);

	void processVariable(SlotInfo info, String indent, String attr);

	void processFunction(MethodInfo methodInfo, boolean referenceNameRequested, Abc abc, String indent, String attr);

	void processMetadata(MetaData metaData);

	void processClass(SlotInfo slotInfo, Abc abc, String attr, String indent);
}
