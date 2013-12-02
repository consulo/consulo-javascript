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
	protected void collectCandidates(final JSClass clazz, final Collection<JSNamedElementNode> candidates)
	{
		ImplementedMethodProcessor processor = new ImplementedMethodProcessor(clazz)
		{
			protected void addNonimplementedFunction(final JSFunction function)
			{
				candidates.add(new JSNamedElementNode(function));
			}
		};

		JSResolveUtil.processInterfaceMethods(clazz, processor);
	}

	protected String getTitleKey()
	{
		return "methods.to.implement.chooser.title";
	}

	protected BaseCreateMethodsFix createFix(final JSClass clazz)
	{
		return new ImplementMethodsFix(clazz);
	}
}