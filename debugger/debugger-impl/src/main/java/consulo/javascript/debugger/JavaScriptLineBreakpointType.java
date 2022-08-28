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

import com.intellij.javascript.JSDebuggerSupportUtils;
import consulo.javascript.language.JavaScriptFileType;
import consulo.annotation.component.ExtensionImpl;
import consulo.document.Document;
import consulo.execution.debug.XSourcePosition;
import consulo.execution.debug.breakpoint.XLineBreakpointTypeBase;
import consulo.execution.debug.evaluation.EvaluationMode;
import consulo.execution.debug.evaluation.XDebuggerEditorsProvider;
import consulo.project.Project;
import consulo.virtualFileSystem.fileType.FileType;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 20.03.14
 */
@ExtensionImpl
public class JavaScriptLineBreakpointType extends XLineBreakpointTypeBase
{
	@Nonnull
	public static JavaScriptLineBreakpointType getInstance()
	{
		return EXTENSION_POINT_NAME.findExtensionOrFail(JavaScriptLineBreakpointType.class);
	}

	public JavaScriptLineBreakpointType()
	{
		super("javascript", "JavaScript Breakpoints", new XDebuggerEditorsProvider()
		{
			@Nonnull
			@Override
			public FileType getFileType()
			{
				return JavaScriptFileType.INSTANCE;
			}

			@Nonnull
			@Override
			public Document createDocument(@Nonnull Project project, @Nonnull String s, @Nullable XSourcePosition sourcePosition, @Nonnull EvaluationMode evaluationMode)
			{
				return JSDebuggerSupportUtils.createDocument(s, project, sourcePosition == null ? null : sourcePosition.getFile(), sourcePosition == null ? 0 : sourcePosition.getOffset());
			}
		});
	}
}
