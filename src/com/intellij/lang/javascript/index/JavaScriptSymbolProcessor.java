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

package com.intellij.lang.javascript.index;

import org.jetbrains.annotations.NonNls;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;

/**
 * @by yole, maxim.mossienko
 */
public interface JavaScriptSymbolProcessor
{
	boolean processFunction(final String nameId, JSNamedElement function);

	boolean processClass(final String nameId, JSNamedElement clazz);

	boolean processVariable(final String nameId, JSNamedElement variable);

	boolean acceptsFile(PsiFile file);

	PsiFile getBaseFile();

	boolean processProperty(final String nameId, final JSNamedElement property);

	boolean processDefinition(final String nameId, final JSNamedElement refExpr);

	boolean processNamespace(final String nameId, final JSNamedElement refExpr);

	boolean processImplicitNamespace(final String nameId, final PsiElement refExpr, boolean finalReference);

	boolean processImplicitFunction(final String nameId, final PsiElement refExpr);

	boolean processImplicitVariable(final String nameId, final PsiElement refExpr);

	String getRequiredNameId();

	boolean processTag(final String nameId, PsiNamedElement namedElement, @NonNls final String attrName);

	abstract class DefaultSymbolProcessor implements JavaScriptSymbolProcessor
	{
		protected PsiFile currentFile;

		@Override
		public boolean processFunction(final String nameId, final JSNamedElement function)
		{
			return process(function);
		}

		@Override
		public boolean processClass(final String nameId, final JSNamedElement clazz)
		{
			return process(clazz);
		}

		@Override
		public boolean processVariable(final String nameId, final JSNamedElement variable)
		{
			return process(variable);
		}

		@Override
		public boolean acceptsFile(final PsiFile file)
		{
			currentFile = file;
			return true;
		}

		@Override
		public boolean processProperty(final String nameId, final JSNamedElement property)
		{
			return process(property);
		}

		@Override
		public boolean processDefinition(final String nameId, final JSNamedElement refExpr)
		{
			return process(refExpr);
		}

		@Override
		public boolean processNamespace(final String nameId, final JSNamedElement refExpr)
		{
			return process(refExpr);
		}

		@Override
		public boolean processImplicitNamespace(final String nameId, final PsiElement refExpr, boolean finalReference)
		{
			return process(refExpr);
		}

		@Override
		public boolean processImplicitFunction(final String nameId, final PsiElement refExpr)
		{
			return process(refExpr);
		}

		@Override
		public boolean processImplicitVariable(final String nameId, final PsiElement refExpr)
		{
			return process(refExpr);
		}

		@Override
		public boolean processTag(final String nameId, final PsiNamedElement namedElement, @NonNls final String attrName)
		{
			return process(namedElement);
		}

		protected abstract boolean process(final PsiElement namedElement);
	}
}
