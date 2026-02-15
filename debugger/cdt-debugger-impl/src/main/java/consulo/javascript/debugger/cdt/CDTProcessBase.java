package consulo.javascript.debugger.cdt;

import com.github.kklisura.cdt.protocol.ChromeDevTools;
import com.github.kklisura.cdt.protocol.commands.Debugger;
import com.github.kklisura.cdt.protocol.types.debugger.Location;
import com.github.kklisura.cdt.protocol.types.debugger.SetBreakpointByUrl;
import consulo.application.Application;
import consulo.application.concurrent.ApplicationConcurrency;
import consulo.execution.ExecutionResult;
import consulo.execution.debug.*;
import consulo.execution.debug.breakpoint.*;
import consulo.execution.debug.evaluation.XDebuggerEditorsProvider;
import consulo.execution.debug.frame.XSuspendContext;
import consulo.execution.debug.icon.ExecutionDebugIconGroup;
import consulo.execution.debug.localize.XDebuggerLocalize;
import consulo.execution.debug.ui.XDebugTabLayouter;
import consulo.execution.ui.ExecutionConsole;
import consulo.execution.ui.layout.RunnerLayoutUi;
import consulo.javascript.debugger.JavaScriptEditorsProvider;
import consulo.javascript.debugger.JavaScriptLineBreakpointType;
import consulo.javascript.debugger.JavaScriptListPanel;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.localize.LocalizeValue;
import consulo.process.ExecutionException;
import consulo.process.ProcessHandler;
import consulo.ui.ex.content.Content;
import consulo.util.io.Url;
import consulo.util.io.Urls;
import consulo.virtualFileSystem.VirtualFile;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.function.Consumer;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public abstract class CDTProcessBase extends XDebugProcess {
    private final ExecutionResult myResult;

    private CDTScriptHolder myScripts = new CDTScriptHolder();

    private JavaScriptListPanel<CDTScript> myScriptListPanel;

    private final XBreakpointManager myXBreakpointManager;

    private ChromeDevTools myChromeDevTools;

    private ExecutorService myExecutor;

    private Map<String, CDTBreakpointInfo> myBreakpoints = new HashMap<>();

    public CDTProcessBase(@Nonnull XDebugSession session, ExecutionResult result) throws ExecutionException {
        super(session);
        myScriptListPanel = new JavaScriptListPanel<>(session.getProject()) {
            @Nullable
            @Override
            public VirtualFile toVirtualFile(@Nonnull CDTScript value, boolean toOpen) {
                return value.toVirtualFile();
            }
        };
        myExecutor = Application.get().getInstance(ApplicationConcurrency.class).createBoundedScheduledExecutorService("CDTProcess", 1);

        myResult = result;
        myXBreakpointManager = XDebuggerManager.getInstance(getSession().getProject()).getBreakpointManager();
        getSession().setPauseActionSupported(true);
    }

    public void initTools(ChromeDevTools devTools) {
        myChromeDevTools = devTools;

        Debugger debugger = myChromeDevTools.getDebugger();

        debugger.onScriptParsed(event -> {
            CDTScript cdtScript = new CDTScript(event);

            myScripts.add(cdtScript);

            getSession().getProject().getUIAccess().execute(() -> myScriptListPanel.add(cdtScript));
        });

        debugger.onPaused(event -> {
            XBreakpoint<?> breakpoint = null;
            List<String> hitBreakpoints = event.getHitBreakpoints();
            if (hitBreakpoints != null) {
                for (String hitBreakpoint : hitBreakpoints) {
                    CDTBreakpointInfo info = myBreakpoints.get(hitBreakpoint);
                    if (info != null) {
                        breakpoint = info.getBreakpoint();
                        break;
                    }
                }
            }

            XDebugSession debugSession = getSession();
            if (breakpoint != null) {
                debugSession.breakpointReached(breakpoint, null, new CDTSuspendContext(event, this));
            }
            else {
                debugSession.positionReached(new CDTSuspendContext(event, this));
            }
        });
    }

    public CDTScriptHolder getScripts() {
        return myScripts;
    }

    public void invoke(Consumer<ChromeDevTools> runnable) {
        if (myChromeDevTools == null) {
            return;
        }

        myExecutor.execute(() -> runnable.accept(myChromeDevTools));
    }

    @Nullable
    @Override
    protected ProcessHandler doGetProcessHandler() {
        return myResult.getProcessHandler();
    }

    @Override
    public boolean checkCanPerformCommands() {
        return myChromeDevTools != null;
    }

    @Nonnull
    @Override
    public XDebuggerEditorsProvider getEditorsProvider() {
        return JavaScriptEditorsProvider.INSTANCE;
    }

    @Nonnull
    @Override
    public ExecutionConsole createConsole() {
        return myResult.getExecutionConsole();
    }

    @Override
    public boolean checkCanInitBreakpoints() {
        return false;
    }

    @Nonnull
    @Override
    public XBreakpointHandler<?>[] getBreakpointHandlers() {
        return new XBreakpointHandler[]{
            new XBreakpointHandler<XLineBreakpoint<XBreakpointProperties>>(JavaScriptLineBreakpointType.class) {
                @Override
                public void registerBreakpoint(@Nonnull final XLineBreakpoint lineBreakpoint) {
                    String presentableFilePath = lineBreakpoint.getFileUrl();
                    int line = lineBreakpoint.getLine();
                    XExpression conditionExpression = lineBreakpoint.getConditionExpression();
                    String expression = conditionExpression == null ? null : conditionExpression.getExpression();

                    invoke((devTools) -> {
                        Url url = Urls.newFromIdea(lineBreakpoint.getFileUrl());

                        String externalForm = url.toExternalForm();

                        SetBreakpointByUrl setBreakpointByUrl = devTools.getDebugger().setBreakpointByUrl(line, externalForm, null, null, null, expression);

                        List<Location> locations = setBreakpointByUrl.getLocations();

                        myBreakpoints.put(setBreakpointByUrl.getBreakpointId(), new CDTBreakpointInfo(locations, lineBreakpoint));

                        if (!locations.isEmpty()) {
                            myXBreakpointManager.updateBreakpointPresentation(lineBreakpoint, ExecutionDebugIconGroup.breakpointBreakpointvalid(), null);
                        }

                        // TODO error handler
                    });
                }

                @Override
                public void unregisterBreakpoint(@Nonnull XLineBreakpoint breakpoint, boolean b) {
                    String presentableFilePath = breakpoint.getPresentableFilePath();
                    // myVm.setBreakpoint(new Breakpoint.Target.ScriptName(presentableFilePath), breakpoint.getLine(), Breakpoint.EMPTY_VALUE, false, null, null, null);
                }
            }
        };
    }

    @Override
    public void startPausing() {
        ChromeDevTools chromeDevTools = myChromeDevTools;

        if (chromeDevTools != null) {
            invoke((devTools) -> devTools.getDebugger().pause());
        }
    }

    @Override
    public void startStepOver(@Nullable XSuspendContext context) {
        invoke((chromeDevTools) -> chromeDevTools.getDebugger().stepOver());
    }

    @Override
    public void startStepInto(@Nullable XSuspendContext context) {
        invoke((chromeDevTools) -> chromeDevTools.getDebugger().stepInto());
    }

    @Override
    public void startStepOut(@Nullable XSuspendContext context) {
        invoke((chromeDevTools) -> chromeDevTools.getDebugger().stepOut());
    }

    @Override
    public void stop() {
        myExecutor.shutdown();

        myBreakpoints.clear();

        Application.get().runReadAction(new Runnable() {
            @Override
            public void run() {
                Collection<? extends XLineBreakpoint<XBreakpointProperties>> breakpoints = myXBreakpointManager.getBreakpoints(JavaScriptLineBreakpointType
                    .class);
                for (XLineBreakpoint<XBreakpointProperties> breakpoint : breakpoints) {
                    myXBreakpointManager.updateBreakpointPresentation(breakpoint, null, null);
                }
            }
        });
    }

    @Override
    public void resume(@Nullable XSuspendContext context) {
        invoke((chromeDevTools) -> chromeDevTools.getDebugger().resume());
    }

    @Override
    public void runToPosition(@Nonnull XSourcePosition position, @Nullable XSuspendContext context) {
    }

    @Nonnull
    @Override
    public XDebugTabLayouter createTabLayouter() {
        return new XDebugTabLayouter() {
            @Override
            public void registerAdditionalContent(@Nonnull RunnerLayoutUi ui) {
                Content content = ui.createContent("ScriptListView", myScriptListPanel, "Scripts", JavaScriptIconGroup.javascript(), null);
                content.setCloseable(false);

                ui.addContent(content);
            }
        };
    }

    @Nonnull
    @Override
    public LocalizeValue getCurrentStateMessage() {
        if (myChromeDevTools == null) {
            return XDebuggerLocalize.debuggerStateMessageDisconnected();
        }
        else {
            return LocalizeValue.localizeTODO("Attached");
        }
    }

//    @Nullable
//    public XLineBreakpoint<?> getXBreakpointByVmBreakpoint(Breakpoint breakpoint) {
//        return myBreakpoints.get(breakpoint);
//    }
}
