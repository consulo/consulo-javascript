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

package com.intellij.lang.javascript.refactoring.introduceVariable;

import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.refactoring.JSBaseIntroduceHandler;
import com.intellij.openapi.project.Project;

/**
 * @author ven
 */
public class JSIntroduceVariableHandler extends JSBaseIntroduceHandler<JSStatement, Settings, JSIntroduceVariableDialog>
{

	@Override
	protected String getRefactoringName()
	{
		return JSBundle.message("javascript.introduce.variable.title");
	}

	@Override
	protected String getCannotIntroduceMessagePropertyKey()
	{
		return "javascript.introduce.variable.error.no.expression.selected";
	}

	@Override
	protected String getDeclText(Settings settings)
	{
		return settings.getIntroducedVarType().toString().toLowerCase() + " " + settings.getVariableName();
	}

	@Override
	protected JSIntroduceVariableDialog createDialog(final Project project, final JSExpression expression, final JSExpression[] occurrences)
	{
		return new JSIntroduceVariableDialog(project, occurrences, expression);
	}
}
