package consulo.javascript.debugger.browser;

import consulo.annotation.component.ExtensionImpl;
import consulo.execution.ExecutionResult;
import consulo.execution.configuration.RunProfile;
import consulo.execution.configuration.RunProfileState;
import consulo.execution.debug.DefaultDebugExecutor;
import consulo.execution.debug.XDebuggerManager;
import consulo.execution.runner.DefaultProgramRunner;
import consulo.execution.runner.ExecutionEnvironment;
import consulo.execution.ui.RunContentDescriptor;
import consulo.javascript.debugger.cdt.CDTProcessBase;
import consulo.process.ExecutionException;

import java.util.Objects;

/**
 * @author VISTALL
 * @since 2020-06-15
 */
@ExtensionImpl
public class JavaScriptBrowserDebugRunner extends DefaultProgramRunner {
    @Override
    public String getRunnerId() {
        return "browser-javascript-debug";
    }

    @Override
    protected RunContentDescriptor doExecute(RunProfileState state, ExecutionEnvironment env) throws ExecutionException {
        return XDebuggerManager.getInstance(env.getProject()).startSession(env, xDebugSession -> {
            ExecutionResult result = Objects.requireNonNull(state.execute(env.getExecutor(), env.getRunner()));

            BrowserDebuggerProcess processHandler = (BrowserDebuggerProcess) result.getProcessHandler();

            BrowserCDTDebugProcess process = new BrowserCDTDebugProcess(xDebugSession, result);

            processHandler.setDebugProcess(process);
            
            return process;
        }).getRunContentDescriptor();
    }

    @Override
    public boolean canRun(String executorId, RunProfile runProfile) {
        return executorId.equals(DefaultDebugExecutor.EXECUTOR_ID) && runProfile instanceof JavaScriptBrowserConfiguration;
    }
}
