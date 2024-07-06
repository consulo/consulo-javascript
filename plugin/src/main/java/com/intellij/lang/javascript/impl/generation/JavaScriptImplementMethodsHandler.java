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

/*
 * @author max
 */
package com.intellij.lang.javascript.impl.generation;

import com.intellij.lang.javascript.impl.validation.BaseCreateMethodsFix;
import com.intellij.lang.javascript.impl.validation.ImplementMethodsFix;
import com.intellij.lang.javascript.impl.validation.ImplementedMethodProcessor;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.generation.ImplementMethodHandler;
import consulo.localize.LocalizeValue;

import java.util.Collection;

@ExtensionImpl
public class JavaScriptImplementMethodsHandler extends BaseJSGenerateHandler implements ImplementMethodHandler
{
	@Override
	protected void collectCandidates(final JSClass clazz, final Collection<JSNamedElementNode> candidates)
	{
		ImplementedMethodProcessor processor = new ImplementedMethodProcessor(clazz)
		{
			@Override
			protected void addNonimplementedFunction(final JSFunction function)
			{
				candidates.add(new JSNamedElementNode(function));
			}
		};

		JSResolveUtil.processInterfaceMethods(clazz, processor);
	}

	@Override
  protected LocalizeValue getTitle()
	{
		return JavaScriptLocalize.methodsToImplementChooserTitle();
  }

  @Override
	protected BaseCreateMethodsFix createFix(final JSClass clazz)
	{
		return new ImplementMethodsFix(clazz);
	}
}