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
	boolean processFunction(JSNamespace namespace, final int nameId, JSNamedElement function);

	boolean processClass(JSNamespace namespace, final int nameId, JSNamedElement clazz);

	boolean processVariable(JSNamespace namespace, final int nameId, JSNamedElement variable);

	boolean acceptsFile(PsiFile file);

	PsiFile getBaseFile();

	boolean processProperty(final JSNamespace namespace, final int nameId, final JSNamedElement property);

	boolean processDefinition(final JSNamespace namespace, final int nameId, final JSNamedElement refExpr);

	boolean processNamespace(final JSNamespace namespace, final int nameId, final JSNamedElement refExpr);

	boolean processImplicitNamespace(final JSNamespace namespace, final int nameId, final PsiElement refExpr, boolean finalReference);

	boolean processImplicitFunction(final JSNamespace namespace, final int nameId, final PsiElement refExpr);

	boolean processImplicitVariable(final JSNamespace namespace, final int nameId, final PsiElement refExpr);

	int getRequiredNameId();

	boolean processTag(JSNamespace namespace, final int nameId, PsiNamedElement namedElement, @NonNls final String attrName);

	abstract class DefaultSymbolProcessor implements JavaScriptSymbolProcessor
	{
		protected PsiFile currentFile;

		@Override
		public boolean processFunction(final JSNamespace namespace, final int nameId, final JSNamedElement function)
		{
			return process(function, namespace);
		}

		@Override
		public boolean processClass(final JSNamespace namespace, final int nameId, final JSNamedElement clazz)
		{
			return process(clazz, namespace);
		}

		@Override
		public boolean processVariable(final JSNamespace namespace, final int nameId, final JSNamedElement variable)
		{
			return process(variable, namespace);
		}

		@Override
		public boolean acceptsFile(final PsiFile file)
		{
			currentFile = file;
			return true;
		}

		@Override
		public boolean processProperty(final JSNamespace namespace, final int nameId, final JSNamedElement property)
		{
			return process(property, namespace);
		}

		@Override
		public boolean processDefinition(final JSNamespace namespace, final int nameId, final JSNamedElement refExpr)
		{
			return process(refExpr, namespace);
		}

		@Override
		public boolean processNamespace(final JSNamespace namespace, final int nameId, final JSNamedElement refExpr)
		{
			return process(refExpr, namespace);
		}

		@Override
		public boolean processImplicitNamespace(final JSNamespace namespace, final int nameId, final PsiElement refExpr, boolean finalReference)
		{
			return process(refExpr, namespace);
		}

		@Override
		public boolean processImplicitFunction(final JSNamespace namespace, final int nameId, final PsiElement refExpr)
		{
			return process(refExpr, namespace);
		}

		@Override
		public boolean processImplicitVariable(final JSNamespace namespace, final int nameId, final PsiElement refExpr)
		{
			return process(refExpr, namespace);
		}

		@Override
		public boolean processTag(final JSNamespace namespace, final int nameId, final PsiNamedElement namedElement, @NonNls final String attrName)
		{
			return process(namedElement, namespace);
		}

		protected abstract boolean process(final PsiElement namedElement, final JSNamespace namespace);
	}
}
