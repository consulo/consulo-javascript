package consulo.javascript.debugger.cdt;

import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Set;
import java.util.ArrayList;
import java.nio.file.Path;
import consulo.javascript.debugger.cdt.sourcemap.SourceMap;
import com.github.kklisura.cdt.protocol.types.debugger.SetBreakpoint;
import com.github.kklisura.cdt.protocol.ChromeDevTools;
import com.github.kklisura.cdt.protocol.commands.Debugger;
import com.github.kklisura.cdt.protocol.events.debugger.Paused;
import com.github.kklisura.cdt.protocol.types.debugger.Location;
import com.github.kklisura.cdt.protocol.types.debugger.SetBreakpointByUrl;
import consulo.application.Application;
import consulo.application.ReadAction;
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
import org.jspecify.annotations.Nullable;

import java.net.SocketException;
import java.util.Collection;
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

    private final Map<String, CDTBreakpointInfo> myBreakpoints = new ConcurrentHashMap<>();
    // the protocol breakpoint ids of each line breakpoint, for unregistering
    private final Map<XLineBreakpoint<?>, List<String>> myBreakpointIds = new ConcurrentHashMap<>();
    // line breakpoints in original sources, bound through the source maps of loaded scripts
    private final Set<XLineBreakpoint<?>> mySourceMappedBreakpoints = ConcurrentHashMap.newKeySet();

    /** How many original lines below a breakpoint without code a source-mapped breakpoint may move to. */
    private static final int SOURCE_MAP_LINE_LOOKAHEAD = 20;

    public CDTProcessBase(XDebugSession session, ExecutionResult result) throws ExecutionException {
        super(session);
        myScriptListPanel = new JavaScriptListPanel<>();

        myExecutor = Application.get().getInstance(ApplicationConcurrency.class).createBoundedScheduledExecutorService("CDTProcess", 1);

        myResult = result;
        myXBreakpointManager = XDebuggerManager.getInstance(getSession().getProject()).getBreakpointManager();
        getSession().setPauseActionSupported(true);
    }

    public void initTools(ChromeDevTools devTools) {
        myChromeDevTools = devTools;

        Debugger debugger = myChromeDevTools.getDebugger();

        debugger.onScriptParsed(event -> {
            CDTScript cdtScript = new CDTScript(getSession().getProject(), event);

            myScripts.add(cdtScript);

            getSession().getProject().getUIAccess().execute(() -> myScriptListPanel.add(cdtScript));

            if (cdtScript.hasSourceMap() && !mySourceMappedBreakpoints.isEmpty()) {
                // ordered before any resume the pause handlers queue, so breakpoints bind before the script runs on
                invoke(tools -> {
                    for (XLineBreakpoint<?> breakpoint : mySourceMappedBreakpoints) {
                        bindSourceMapped(tools, cdtScript, breakpoint);
                    }
                });
            }
        });

        debugger.onPaused(this::onPause);

        devTools.getRuntime().onExecutionContextDestroyed(executionContextDestroyed -> {
            getProcessHandler().destroyProcess();
        });

        initOthers();
    }

    protected void initOthers() {
        invoke(devTools -> {
            ReadAction.run(() -> getSession().initBreakpoints());

            devTools.getRuntime().runIfWaitingForDebugger();
        });
    }

    public CDTScriptHolder getScripts() {
        return myScripts;
    }

    public void invoke(Consumer<ChromeDevTools> runnable) {
        if (myChromeDevTools == null) {
            return;
        }

        myExecutor.execute(() -> {
            try {
                runnable.accept(myChromeDevTools);
            }
            catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    protected void onPause(Paused event) {
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

    @Override
    public XDebuggerEditorsProvider getEditorsProvider() {
        return JavaScriptEditorsProvider.INSTANCE;
    }

    @Override
    public ExecutionConsole createConsole() {
        return myResult.getExecutionConsole();
    }

    @Override
    public boolean checkCanInitBreakpoints() {
        return false;
    }

    /**
     * Line breakpoint types of original sources (languages compiled to JavaScript) whose
     * breakpoints bind through the source maps of the loaded scripts: by default the types
     * the {@link SourceMappedBreakpointTypeProvider}s contribute.
     */
    protected List<Class<? extends XLineBreakpointType<?>>> getSourceMappedBreakpointTypes() {
        List<Class<? extends XLineBreakpointType<?>>> types = new ArrayList<>();
        for (SourceMappedBreakpointTypeProvider provider : Application.get().getExtensionPoint(SourceMappedBreakpointTypeProvider.class).getExtensionList()) {
            types.add(provider.getBreakpointType());
        }
        return types;
    }

    @Override
    @SuppressWarnings("unchecked")
    public XBreakpointHandler<?>[] getBreakpointHandlers() {
        List<XBreakpointHandler<?>> handlers = new ArrayList<>();
        handlers.add(new XBreakpointHandler<>(JavaScriptLineBreakpointType.class) {
            @Override
            public void registerBreakpoint(XLineBreakpoint lineBreakpoint) {
                registerScriptBreakpoint(lineBreakpoint);
            }

            @Override
            public void unregisterBreakpoint(XLineBreakpoint breakpoint, boolean temporary) {
                removeBreakpoint(breakpoint);
            }
        });
        for (Class<? extends XLineBreakpointType<?>> type : getSourceMappedBreakpointTypes()) {
            handlers.add(new XBreakpointHandler(type) {
                @Override
                public void registerBreakpoint(XBreakpoint breakpoint) {
                    registerSourceMappedBreakpoint((XLineBreakpoint<?>) breakpoint);
                }

                @Override
                public void unregisterBreakpoint(XBreakpoint breakpoint, boolean temporary) {
                    removeBreakpoint((XLineBreakpoint<?>) breakpoint);
                }
            });
        }
        return handlers.toArray(XBreakpointHandler[]::new);
    }

    private void registerScriptBreakpoint(XLineBreakpoint<?> lineBreakpoint) {
        int line = lineBreakpoint.getLine();
        String expression = conditionOf(lineBreakpoint);

        invoke((devTools) -> {
            Url url = Urls.newFromIdea(lineBreakpoint.getFileUrl());

            String externalForm = url.toExternalForm();

            SetBreakpointByUrl setBreakpointByUrl = devTools.getDebugger().setBreakpointByUrl(line, externalForm, null, null, null, expression);

            List<Location> locations = setBreakpointByUrl.getLocations();

            rememberBreakpoint(setBreakpointByUrl.getBreakpointId(), new CDTBreakpointInfo(locations, lineBreakpoint), lineBreakpoint);

            if (!locations.isEmpty()) {
                myXBreakpointManager.updateBreakpointPresentation(lineBreakpoint, ExecutionDebugIconGroup.breakpointBreakpointvalid(), null);
            }

            // TODO error handler
        });
    }

    private void registerSourceMappedBreakpoint(XLineBreakpoint<?> lineBreakpoint) {
        mySourceMappedBreakpoints.add(lineBreakpoint);
        invoke(devTools -> {
            for (CDTScript script : myScripts.getAll()) {
                bindSourceMapped(devTools, script, lineBreakpoint);
            }
        });
    }

    /** Binds a breakpoint of an original source in {@code script} when the script's source map covers that source. */
    private void bindSourceMapped(ChromeDevTools devTools, CDTScript script, XLineBreakpoint<?> lineBreakpoint) {
        if (!script.hasSourceMap() || !mySourceMappedBreakpoints.contains(lineBreakpoint)) {
            return;
        }
        SourceMap sourceMap = script.getSourceMap();
        Path file = SourceMappedPositions.toLocalPath(lineBreakpoint.getFileUrl());
        if (sourceMap == null || file == null) {
            return;
        }
        int sourceIndex = SourceMappedPositions.sourceIndex(sourceMap, file);
        if (sourceIndex < 0) {
            return;
        }
        SourceMap.Mapping mapping = sourceMap.findGenerated(sourceIndex, lineBreakpoint.getLine(), SOURCE_MAP_LINE_LOOKAHEAD);
        if (mapping == null) {
            return;
        }

        Location location = new Location();
        location.setScriptId(script.getId());
        location.setLineNumber(mapping.generatedLine());
        location.setColumnNumber(mapping.generatedColumn());
        SetBreakpoint result = devTools.getDebugger().setBreakpoint(location, conditionOf(lineBreakpoint));
        Location actual = result.getActualLocation();
        rememberBreakpoint(result.getBreakpointId(),
            new CDTBreakpointInfo(actual == null ? List.of() : List.of(actual), lineBreakpoint),
            lineBreakpoint);
        myXBreakpointManager.updateBreakpointPresentation(lineBreakpoint, ExecutionDebugIconGroup.breakpointBreakpointvalid(), null);
    }

    private void rememberBreakpoint(String breakpointId, CDTBreakpointInfo info, XLineBreakpoint<?> lineBreakpoint) {
        myBreakpoints.put(breakpointId, info);
        myBreakpointIds.computeIfAbsent(lineBreakpoint, key -> new CopyOnWriteArrayList<>()).add(breakpointId);
    }

    private void removeBreakpoint(XLineBreakpoint<?> lineBreakpoint) {
        mySourceMappedBreakpoints.remove(lineBreakpoint);
        List<String> ids = myBreakpointIds.remove(lineBreakpoint);
        if (ids == null || ids.isEmpty()) {
            return;
        }
        invoke(devTools -> {
            for (String id : ids) {
                myBreakpoints.remove(id);
                devTools.getDebugger().removeBreakpoint(id);
            }
        });
    }

    private static @Nullable String conditionOf(XLineBreakpoint<?> lineBreakpoint) {
        XExpression conditionExpression = lineBreakpoint.getConditionExpression();
        return conditionExpression == null ? null : conditionExpression.getExpression();
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
        if (myChromeDevTools instanceof AutoCloseable closeable) {
            try {
                closeable.close();
            }
            catch (Exception ignored) {
            }
        }

        myExecutor.shutdown();

        // every breakpoint this session bound, source-mapped ones included, loses its "verified" mark
        List<XLineBreakpoint<?>> bound = new ArrayList<>(myBreakpointIds.keySet());
        bound.addAll(mySourceMappedBreakpoints);

        myBreakpoints.clear();
        myBreakpointIds.clear();
        mySourceMappedBreakpoints.clear();

        Application.get().runReadAction(new Runnable() {
            @Override
            public void run() {
                Collection<? extends XLineBreakpoint<XBreakpointProperties>> breakpoints = myXBreakpointManager.getBreakpoints(JavaScriptLineBreakpointType
                    .class);
                for (XLineBreakpoint<XBreakpointProperties> breakpoint : breakpoints) {
                    myXBreakpointManager.updateBreakpointPresentation(breakpoint, null, null);
                }
                for (XLineBreakpoint<?> breakpoint : bound) {
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
    public void runToPosition(XSourcePosition position, @Nullable XSuspendContext context) {
    }

    @Override
    public XDebugTabLayouter createTabLayouter() {
        return new XDebugTabLayouter() {
            @Override
            public void registerAdditionalContent(RunnerLayoutUi ui) {
                Content content = ui.createContent("ScriptListView", myScriptListPanel, "Scripts", JavaScriptIconGroup.javascript(), null);
                content.setCloseable(false);

                ui.addContent(content);
            }
        };
    }

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
