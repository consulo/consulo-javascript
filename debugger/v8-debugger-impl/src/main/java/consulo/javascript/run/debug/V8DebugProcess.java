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

import consulo.execution.ExecutionResult;
import consulo.process.ExecutionException;
import consulo.execution.debug.XDebugSession;
import consulo.execution.debug.XDebuggerBundle;
import consulo.annotation.UsedInPlugin;
import org.chromium.sdk.JavascriptVmFactory;
import org.chromium.sdk.StandaloneVm;

import javax.annotation.Nonnull;
import java.net.InetSocketAddress;

/**
 * @author VISTALL
 * @since 20.03.14
 */
@UsedInPlugin
public class V8DebugProcess extends V8BaseDebugProcess<StandaloneVm>
{
	public V8DebugProcess(@Nonnull XDebugSession session, ExecutionResult result, int port) throws ExecutionException
	{
		super(session, result);

		myVm = JavascriptVmFactory.getInstance().createStandalone(new InetSocketAddress("localhost", port), null);
	}

	@UsedInPlugin
	public void attach() throws Exception
	{
		myVm.attach(new V8DebugEventListener(this));
	}

	@Override
	public String getCurrentStateMessage()
	{
		if(myVm == null)
		{
			return XDebuggerBundle.message("debugger.state.message.disconnected");
		}
		else
		{
			if(myVm.isAttached())
			{
				return "Attached";
			}
			else
			{
				String disconnectReason = myVm.getDisconnectReason();
				if(disconnectReason == null)
				{
					return XDebuggerBundle.message("debugger.state.message.disconnected");
				}
				return "Disconnected: " + disconnectReason;
			}
		}
	}
}
