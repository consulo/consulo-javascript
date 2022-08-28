/*
 * Copyright 2013-2015 must-be.org
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

package consulo.javascript.run.debug;

import consulo.execution.debug.frame.XExecutionStack;
import org.chromium.sdk.DebugContext;
import javax.annotation.Nullable;

import consulo.execution.debug.frame.XSuspendContext;

/**
 * @author VISTALL
 * @since 20.03.14
 */
public class V8SuspendContext extends XSuspendContext
{
	private V8ExecutionStack myStack;

	public V8SuspendContext(DebugContext debugContext)
	{
		myStack = new V8ExecutionStack(debugContext);
	}

	@Nullable
	@Override
	public XExecutionStack getActiveExecutionStack()
	{
		return myStack;
	}

	@Override
	public XExecutionStack[] getExecutionStacks()
	{
		return new XExecutionStack[] {myStack};
	}
}
