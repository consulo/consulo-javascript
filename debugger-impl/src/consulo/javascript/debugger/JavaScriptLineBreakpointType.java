/*
 * Copyright 2013-2014 must-be.org
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

import org.consulo.lombok.annotations.LazyInstance;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.javascript.JSDebuggerSupportUtils;
import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.breakpoints.XLineBreakpointTypeBase;
import com.intellij.xdebugger.evaluation.EvaluationMode;
import com.intellij.xdebugger.evaluation.XDebuggerEditorsProvider;

/**
 * @author VISTALL
 * @since 20.03.14
 */
public class JavaScriptLineBreakpointType extends XLineBreakpointTypeBase
{
	@NotNull
	@LazyInstance
	public static JavaScriptLineBreakpointType getInstance()
	{
		return EXTENSION_POINT_NAME.findExtension(JavaScriptLineBreakpointType.class);
	}

	public JavaScriptLineBreakpointType()
	{
		super("javascript", "JavaScript Breakpoints", new XDebuggerEditorsProvider()
		{
			@NotNull
			@Override
			public FileType getFileType()
			{
				return JavaScriptFileType.INSTANCE;
			}

			@NotNull
			@Override
			public Document createDocument(@NotNull Project project, @NotNull String s, @Nullable XSourcePosition sourcePosition, @NotNull EvaluationMode evaluationMode)
			{
				return JSDebuggerSupportUtils.createDocument(s, project, sourcePosition == null ? null : sourcePosition.getFile(), sourcePosition == null ? 0 : sourcePosition.getOffset());
			}
		});
	}
}
