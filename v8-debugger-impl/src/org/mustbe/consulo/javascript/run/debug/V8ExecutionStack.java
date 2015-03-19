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

package org.mustbe.consulo.javascript.run.debug;

import java.util.ArrayList;
import java.util.List;

import org.chromium.sdk.CallFrame;
import org.chromium.sdk.DebugContext;
import org.jetbrains.annotations.Nullable;
import com.intellij.xdebugger.frame.XExecutionStack;
import com.intellij.xdebugger.frame.XStackFrame;

/**
 * @author VISTALL
 * @since 20.03.14
 */
public class V8ExecutionStack extends XExecutionStack
{
	private final DebugContext myDebugContext;

	public V8ExecutionStack(DebugContext debugContext)
	{
		super("");
		myDebugContext = debugContext;
	}

	@Nullable
	@Override
	public XStackFrame getTopFrame()
	{
		List<? extends CallFrame> callFrames = myDebugContext.getCallFrames();
		return callFrames.isEmpty() ? null : new V8StackFrame(callFrames.get(0));
	}

	@Override
	public void computeStackFrames(int i, XStackFrameContainer frameContainer)
	{
		List<? extends CallFrame> callFrames = myDebugContext.getCallFrames();
		List<V8StackFrame> stackFrames = new ArrayList<V8StackFrame>(callFrames.size());

		for(int a = i; i < callFrames.size(); i ++)
		{
			CallFrame callFrame = callFrames.get(a);

			stackFrames.add(new V8StackFrame(callFrame));
		}

		frameContainer.addStackFrames(stackFrames, true);
	}
}
