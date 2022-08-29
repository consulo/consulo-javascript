/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.intention.switchtoif;

import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import org.intellij.idea.lang.javascript.psiutil.JSRecursiveElementVisitor;

class LabelSearchVisitor extends JSRecursiveElementVisitor
{
	private final String labelName;
	private boolean used;

	LabelSearchVisitor(String name)
	{
		this.labelName = name;
	}

	@Override
	public void visitJSReferenceExpression(JSReferenceExpression expression)
	{
	}

	@Override
	public void visitJSLabeledStatement(JSLabeledStatement statement)
	{
		final String labelText = statement.getLabel();

		this.used = (labelText != null && labelText.equals(this.labelName));
	}

	public boolean isUsed()
	{
		return this.used;
	}
}
