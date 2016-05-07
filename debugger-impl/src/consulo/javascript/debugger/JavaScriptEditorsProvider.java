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

package consulo.javascript.debugger;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.lang.javascript.psi.JSElementFactory;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.xdebugger.evaluation.XDebuggerEditorsProviderBase;

/**
 * @author VISTALL
 * @since 5/8/2016
 */
public class JavaScriptEditorsProvider extends XDebuggerEditorsProviderBase
{
	public static final JavaScriptEditorsProvider INSTANCE = new JavaScriptEditorsProvider();

	@Override
	protected PsiFile createExpressionCodeFragment(@NotNull Project project, @NotNull String text, @Nullable PsiElement element, boolean isPhysical)
	{
		return JSElementFactory.createExpressionCodeFragment(project, text, element, isPhysical);
	}

	@NotNull
	@Override
	public FileType getFileType()
	{
		return JavaScriptFileType.INSTANCE;
	}
}
