/*
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

package org.mustbe.consulo.javascript.lang.psi.impl.fragment;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.tree.IElementType;

/**
 * @author VISTALL
 * @since 05.12.2015
 */
public class JavaScriptFragmentFileImpl extends PsiFileImpl
{
	protected JavaScriptFragmentFileImpl(@NotNull IElementType elementType, @NotNull FileViewProvider provider)
	{
		super(elementType, elementType, provider);
	}

	@NotNull
	@Override
	public FileType getFileType()
	{
		return JavaScriptFileType.INSTANCE;
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		visitor.visitFile(this);
	}
}
