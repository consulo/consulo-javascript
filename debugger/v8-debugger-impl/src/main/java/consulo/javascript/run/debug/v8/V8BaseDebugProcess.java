/*
 * Copyright 2013-2020 consulo.io
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

package consulo.javascript.run.debug.v8;

import consulo.application.AllIcons;
import consulo.application.Application;
import consulo.execution.ExecutionResult;
import consulo.execution.debug.*;
import consulo.execution.debug.breakpoint.XBreakpointHandler;
import consulo.execution.debug.breakpoint.XBreakpointProperties;
import consulo.execution.debug.breakpoint.XExpression;
import consulo.execution.debug.breakpoint.XLineBreakpoint;
import consulo.execution.debug.evaluation.XDebuggerEditorsProvider;
import consulo.execution.debug.frame.XSuspendContext;
import consulo.execution.debug.localize.XDebuggerLocalize;
import consulo.execution.debug.ui.XDebugTabLayouter;
import consulo.execution.ui.ExecutionConsole;
import consulo.execution.ui.layout.RunnerLayoutUi;
import consulo.javascript.debugger.JavaScriptEditorsProvider;
import consulo.javascript.debugger.JavaScriptLineBreakpointType;
import consulo.javascript.debugger.JavaScriptListPanel;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.process.ExecutionException;
import consulo.process.ProcessHandler;
import consulo.ui.ex.awt.UIUtil;
import consulo.ui.ex.content.Content;
import consulo.virtualFileSystem.VirtualFile;
import jakarta.annotation.Nullable;
import org.chromium.sdk.Breakpoint;
import org.chromium.sdk.DebugContext;
import org.chromium.sdk.JavascriptVm;
import org.chromium.sdk.Script;

import jakarta.annotation.Nonnull;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * @author VISTALL
 * @since 2020-06-16
 */
public abstract class V8BaseDebugProcess<V extends JavascriptVm> extends XDebugProcess
{
	protected V myVm;

	private final ExecutionResult myResult;
	private DebugContext myCurrentDebugContext;
	private JavaScriptListPanel<Script> myScriptListPanel;

	private final XBreakpointManager myXBreakpointManager;

	private Map<Breakpoint, XLineBreakpoint> myBreakpoints = new HashMap<>();

	public V8BaseDebugProcess(@Nonnull XDebugSession session, ExecutionResult result) throws ExecutionException
	{
		super(session);
		myScriptListPanel = new JavaScriptListPanel<Script>(session.getProject())
		{
			@Nullable
			@Override
			public VirtualFile toVirtualFile(@Nonnull Script value, boolean toOpen)
			{
				return V8ScriptUtil.toVirtualFile(value, toOpen);
			}
		};
		myResult = result;
		myXBreakpointManager = XDebuggerManager.getInstance(getSession().getProject()).getBreakpointManager();
		getSession().setPauseActionSupported(true);
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

	@Nonnull
	@Override
	public XDebuggerEditorsProvider getEditorsProvider()
	{
		return JavaScriptEditorsProvider.INSTANCE;
	}

	@Nonnull
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

	@Nonnull
	@Override
	public XBreakpointHandler<?>[] getBreakpointHandlers()
	{
		return new XBreakpointHandler[]{
				new XBreakpointHandler<XLineBreakpoint<XBreakpointProperties>>(JavaScriptLineBreakpointType.class)
				{
					@Override
					public void registerBreakpoint(@Nonnull final XLineBreakpoint lineBreakpoint)
					{
						String presentableFilePath = lineBreakpoint.getPresentableFilePath();
						int line = lineBreakpoint.getLine();
						XExpression conditionExpression = lineBreakpoint.getConditionExpression();
						String expression = conditionExpression == null ? null : conditionExpression.getExpression();
						myVm.setBreakpoint(new Breakpoint.Target.ScriptName(presentableFilePath), line, Breakpoint.EMPTY_VALUE, true, expression, new JavascriptVm.BreakpointCallback()
						{
							@Override
							public void success(Breakpoint breakpoint)
							{
								myBreakpoints.put(breakpoint, lineBreakpoint);
								myXBreakpointManager.updateBreakpointPresentation(lineBreakpoint, AllIcons.Debugger.Db_verified_breakpoint, null);
							}

							@Override
							public void failure(String s)
							{
								myXBreakpointManager.updateBreakpointPresentation(lineBreakpoint, AllIcons.Debugger.Db_invalid_breakpoint, s);
							}
						}, null);
					}

					@Override
					public void unregisterBreakpoint(@Nonnull XLineBreakpoint breakpoint, boolean b)
					{
						String presentableFilePath = breakpoint.getPresentableFilePath();
						myVm.setBreakpoint(new Breakpoint.Target.ScriptName(presentableFilePath), breakpoint.getLine(), Breakpoint.EMPTY_VALUE, false, null, null, null);
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
	public void startStepOver(@Nullable XSuspendContext context)
	{
		if(myCurrentDebugContext != null)
		{
			myCurrentDebugContext.continueVm(DebugContext.StepAction.OVER, 0, null, null);
		}
	}

	@Override
	public void startStepInto(@Nullable XSuspendContext context)
	{
		if(myCurrentDebugContext != null)
		{
			myCurrentDebugContext.continueVm(DebugContext.StepAction.IN, 0, null, null);
		}
	}

	@Override
	public void startStepOut(@Nullable XSuspendContext context)
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

		Application.get().runReadAction(new Runnable()
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
	public void resume(@Nullable XSuspendContext context)
	{
		DebugContext currentDebugContext = myCurrentDebugContext;
		if(currentDebugContext == null)
		{
			return;
		}
		currentDebugContext.continueVm(DebugContext.StepAction.CONTINUE, 0, null, null);
	}

	@Override
	public void runToPosition(@Nonnull XSourcePosition position, @Nullable XSuspendContext context)
	{
	}

	@Nonnull
	@Override
	public XDebugTabLayouter createTabLayouter()
	{
		return new XDebugTabLayouter()
		{
			@Override
			public void registerAdditionalContent(@Nonnull RunnerLayoutUi ui)
			{
				Content content = ui.createContent("ScriptListView", myScriptListPanel, "Scripts", JavaScriptIconGroup.javascript(), null);
				content.setCloseable(false);

				ui.addContent(content);
			}
		};
	}

	@Override
	public String getCurrentStateMessage()
	{
		if (myVm == null)
		{
			return XDebuggerLocalize.debuggerStateMessageDisconnected().get();
		}
		else
		{
			if (myVm.isAttached())
			{
				return "Attached";
			}
			else
			{
				return XDebuggerLocalize.debuggerStateMessageDisconnected().get();
			}
		}
	}

	public void addScript(final Script script)
	{
		UIUtil.invokeLaterIfNeeded(() -> myScriptListPanel.add(script));
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
