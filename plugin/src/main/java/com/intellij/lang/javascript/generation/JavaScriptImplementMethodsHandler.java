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
package com.intellij.lang.javascript.generation;

import java.util.Collection;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.validation.BaseCreateMethodsFix;
import com.intellij.lang.javascript.validation.ImplementMethodsFix;
import com.intellij.lang.javascript.validation.ImplementedMethodProcessor;

public class JavaScriptImplementMethodsHandler extends BaseJSGenerateHandler
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
	protected String getTitleKey()
	{
		return "methods.to.implement.chooser.title";
	}

	@Override
	protected BaseCreateMethodsFix createFix(final JSClass clazz)
	{
		return new ImplementMethodsFix(clazz);
	}
}