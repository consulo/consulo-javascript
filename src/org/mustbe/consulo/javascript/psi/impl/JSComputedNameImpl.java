/*
 * Copyright 2013-2016 must-be.org
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

package org.mustbe.consulo.javascript.psi.impl;

import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.psi.JSComputedName;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;

/**
 * @author VISTALL
 * @since 03.03.2016
 */
public class JSComputedNameImpl extends JSElementImpl implements JSComputedName
{
	public JSComputedNameImpl(ASTNode node)
	{
		super(node);
	}

	@Nullable
	@Override
	public JSExpression getExpression()
	{
		return findChildByClass(JSExpression.class);
	}
}
