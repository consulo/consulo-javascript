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

import java.net.InetSocketAddress;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.chromium.sdk.Breakpoint;
import org.chromium.sdk.BrowserFactory;
import org.chromium.sdk.DebugContext;
import org.chromium.sdk.JavascriptVm;
import org.chromium.sdk.Script;
import org.chromium.sdk.StandaloneVm;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.Exported;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.ExecutionResult;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.ui.ExecutionConsole;
import com.intellij.execution.ui.RunnerLayoutUi;
import com.intellij.icons.AllIcons;
import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.lang.javascript.JavaScriptIcons;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.ui.content.Content;
import com.intellij.util.ui.UIUtil;
import com.intellij.xdebugger.XDebugProcess;
import com.intellij.xdebugger.XDebugSession;
import com.intellij.xdebugger.XDebuggerBundle;
import com.intellij.xdebugger.XDebuggerManager;
import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.breakpoints.XBreakpointHandler;
import com.intellij.xdebugger.breakpoints.XBreakpointManager;
import com.intellij.xdebugger.breakpoints.XBreakpointProperties;
import com.intellij.xdebugger.breakpoints.XLineBreakpoint;
import com.intellij.xdebugger.evaluation.XDebuggerEditorsProvider;
import com.intellij.xdebugger.evaluation.XDebuggerEditorsProviderBase;
import com.intellij.xdebugger.ui.XDebugTabLayouter;

/**
 * @author VISTALL
 * @since 20.03.14
 */
public class V8DebugProcess extends XDebugProcess
{
	private final ExecutionResult myResult;
	private final StandaloneVm myVm;
	private DebugContext myCurrentDebugContext;
	private JavaScriptListPanel<Script> myScriptListPanel;

	private final XBreakpointManager myXBreakpointManager;

	private Map<Breakpoint, XLineBreakpoint> myBreakpoints = new HashMap<Breakpoint, XLineBreakpoint>();

	public V8DebugProcess(@NotNull XDebugSession session, ExecutionResult result, int port) throws ExecutionException
	{
		super(session);
		myScriptListPanel = new JavaScriptListPanel<Script>(session.getProject())
		{
			@Nullable
			@Override
			public VirtualFile toVirtualFile(@NotNull Script value, boolean toOpen)
			{
				return V8ScriptUtil.toVirtualFile(value, toOpen);
			}
		};
		myResult = result;
		myXBreakpointManager = XDebuggerManager.getInstance(getSession().getProject()).getBreakpointManager();
		getSession().setPauseActionSupported(true);
		myVm = BrowserFactory.getInstance().createStandalone(new InetSocketAddress("localhost", port), null);
	}

	@Exported
	public void attach() throws Exception
	{
		myVm.attach(new V8DebugEventListener(this));
	}

	@Nullable
	@Override
	protected ProcessHandler doGetProcessHandler()
	{
		return myResult.getProcessHandler();
	}

	@Override
	public boolean checkCanPerformCommands()
	{
		return myVm != null && myVm.isAttached();
	}

	@NotNull
	@Override
	public XDebuggerEditorsProvider getEditorsProvider()
	{
		return new XDebuggerEditorsProviderBase()
		{
			@Override
			protected PsiFile createExpressionCodeFragment(@NotNull Project project, @NotNull String s, @Nullable PsiElement element, boolean b)
			{
				return null;
			}

			@NotNull
			@Override
			public FileType getFileType()
			{
				return JavaScriptFileType.INSTANCE;
			}
		};
	}

	@NotNull
	@Override
	public ExecutionConsole createConsole()
	{
		return myResult.getExecutionConsole();
	}

	@Override
	public boolean checkCanInitBreakpoints()
	{
		return false;
	}

	@NotNull
	@Override
	public XBreakpointHandler<?>[] getBreakpointHandlers()
	{
		return new XBreakpointHandler[]{
				new XBreakpointHandler<XLineBreakpoint<XBreakpointProperties>>(JavaScriptLineBreakpointType.class)
				{
					@Override
					public void registerBreakpoint(@NotNull final XLineBreakpoint xBreakpoint)
					{
						String presentableFilePath = xBreakpoint.getPresentableFilePath();
						int line = xBreakpoint.getLine();
						myVm.setBreakpoint(new Breakpoint.Target.ScriptName(presentableFilePath), line, 0, true, null,
								new JavascriptVm.BreakpointCallback()
						{
							@Override
							public void success(Breakpoint breakpoint)
							{
								myBreakpoints.put(breakpoint, xBreakpoint);
								myXBreakpointManager.updateBreakpointPresentation(xBreakpoint, AllIcons.Debugger.Db_verified_breakpoint, null);
							}

							@Override
							public void failure(String s)
							{
								myXBreakpointManager.updateBreakpointPresentation(xBreakpoint, AllIcons.Debugger.Db_invalid_breakpoint, s);
							}
						}, null);
					}

					@Override
					public void unregisterBreakpoint(@NotNull XLineBreakpoint xBreakpoint, boolean b)
					{
						String presentableFilePath = xBreakpoint.getPresentableFilePath();
						myVm.setBreakpoint(new Breakpoint.Target.ScriptName(presentableFilePath), xBreakpoint.getLine(), 0, false, null, null, null);
					}
				}
		};
	}

	@Override
	public void startPausing()
	{
		myVm.suspend(new JavascriptVm.SuspendCallback()
		{
			@Override
			public void success()
			{
				//
			}

			@Override
			public void failure(Exception e)
			{

			}
		});
	}

	@Override
	public void startStepOver()
	{
		if(myCurrentDebugContext != null)
		{
			myCurrentDebugContext.continueVm(DebugContext.StepAction.OVER, 0, null, null);
		}
	}

	@Override
	public void startStepInto()
	{
		if(myCurrentDebugContext != null)
		{
			myCurrentDebugContext.continueVm(DebugContext.StepAction.IN, 0, null, null);
		}
	}

	@Override
	public void startStepOut()
	{
		if(myCurrentDebugContext != null)
		{
			myCurrentDebugContext.continueVm(DebugContext.StepAction.OUT, 0, null, null);
		}
	}

	@Override
	public void stop()
	{
		myVm.detach();
		myBreakpoints.clear();

		ApplicationManager.getApplication().runReadAction(new Runnable()
		{
			@Override
			public void run()
			{
				Collection<? extends XLineBreakpoint<XBreakpointProperties>> breakpoints = myXBreakpointManager.getBreakpoints(JavaScriptLineBreakpointType
						.class);
				for(XLineBreakpoint<XBreakpointProperties> breakpoint : breakpoints)
				{
					myXBreakpointManager.updateBreakpointPresentation(breakpoint, null, null);
				}
			}
		});
	}

	@Override
	public void resume()
	{
		DebugContext currentDebugContext = myCurrentDebugContext;
		if(currentDebugContext == null)
		{
			return;
		}
		currentDebugContext.continueVm(DebugContext.StepAction.CONTINUE, 0, null, null);
	}

	@Override
	public void runToPosition(@NotNull XSourcePosition xSourcePosition)
	{

	}

	@NotNull
	@Override
	public XDebugTabLayouter createTabLayouter()
	{
		return new XDebugTabLayouter()
		{
			@Override
			public void registerAdditionalContent(@NotNull RunnerLayoutUi ui)
			{
				Content content = ui.createContent("ScriptListView", myScriptListPanel, "Scripts", JavaScriptIcons.JavaScript, null);
				content.setCloseable(false);

				ui.addContent(content);
			}
		};
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

	public void addScript(final Script script)
	{
		UIUtil.invokeLaterIfNeeded(new Runnable()
		{
			@Override
			public void run()
			{
				myScriptListPanel.add(script);
			}
		});
	}

	@Nullable
	public XLineBreakpoint<?> getXBreakpointByVmBreakpoint(Breakpoint breakpoint)
	{
		return myBreakpoints.get(breakpoint);
	}

	public void setCurrentDebugContext(DebugContext debugContext)
	{
		myCurrentDebugContext = debugContext;
	}
}
