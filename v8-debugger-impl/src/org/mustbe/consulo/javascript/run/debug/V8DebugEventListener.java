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

package org.mustbe.consulo.javascript.run.debug;

import java.util.Collection;

import org.chromium.sdk.Breakpoint;
import org.chromium.sdk.DebugContext;
import org.chromium.sdk.DebugEventListener;
import org.chromium.sdk.Script;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.xdebugger.breakpoints.XLineBreakpoint;

/**
 * @author VISTALL
 * @since 20.03.14
 */
public class V8DebugEventListener implements DebugEventListener
{
	private final V8DebugProcess myV8DebugProcess;

	private boolean myFirstPausing = true;

	public V8DebugEventListener(V8DebugProcess v8DebugProcess)
	{
		myV8DebugProcess = v8DebugProcess;
	}

	@Override
	public void suspended(DebugContext debugContext)
	{
		if(myFirstPausing)
		{
			myFirstPausing = false;
			debugContext.continueVm(DebugContext.StepAction.CONTINUE, 0, null, null);
			return;
		}

		myV8DebugProcess.setCurrentDebugContext(debugContext);

		Collection<? extends Breakpoint> breakpointsHit = debugContext.getBreakpointsHit();
		if(!breakpointsHit.isEmpty())
		{
			XLineBreakpoint<?> lineBreakpoint = myV8DebugProcess.getXBreakpointByVmBreakpoint(breakpointsHit.iterator().next());
			if(lineBreakpoint != null)
			{
				myV8DebugProcess.getSession().breakpointReached(lineBreakpoint, null, new V8SuspendContext(debugContext));
				return;
			}
		}
		myV8DebugProcess.getSession().positionReached(new V8SuspendContext(debugContext));
	}

	@Override
	public void resumed()
	{
		myV8DebugProcess.setCurrentDebugContext(null);
	}

	@Override
	public void disconnected()
	{
		myV8DebugProcess.setCurrentDebugContext(null);
	}

	@Override
	public void scriptLoaded(Script script)
	{
		String name = script.getName();
		if(name == null)
		{
			return;
		}
		myV8DebugProcess.addScript(script);
	}

	@Override
	public void scriptCollected(Script script)
	{

	}

	@Override
	public VmStatusListener getVmStatusListener()
	{
		return null;
	}

	@Override
	public void scriptContentChanged(Script script)
	{

	}

}
