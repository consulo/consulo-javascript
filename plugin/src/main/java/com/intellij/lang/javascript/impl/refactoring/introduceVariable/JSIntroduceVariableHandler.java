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

package com.intellij.lang.javascript.impl.refactoring.introduceVariable;

import com.intellij.lang.javascript.impl.refactoring.JSBaseIntroduceHandler;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.localize.LocalizeValue;
import consulo.project.Project;
import consulo.ui.annotation.RequiredUIAccess;

/**
 * @author ven
 */
public class JSIntroduceVariableHandler extends JSBaseIntroduceHandler<JSStatement, Settings, JSIntroduceVariableDialog> {
    @Override
    protected String getRefactoringName() {
        return JavaScriptLocalize.javascriptIntroduceVariableTitle().get();
    }

    @Override
    protected LocalizeValue getCannotIntroduceMessage() {
        return JavaScriptLocalize.javascriptIntroduceVariableErrorNoExpressionSelected();
    }

    @Override
    protected String getDeclText(Settings settings) {
        return settings.getIntroducedVarType().toString().toLowerCase() + " " + settings.getVariableName();
    }

    @Override
    @RequiredUIAccess
    protected JSIntroduceVariableDialog createDialog(Project project, JSExpression expression, JSExpression[] occurrences) {
        return new JSIntroduceVariableDialog(project, occurrences, expression);
    }
}
