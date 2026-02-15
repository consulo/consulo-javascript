package consulo.javascript.debugger.browser.process;

import com.github.kklisura.cdt.protocol.events.debugger.Paused;
import consulo.execution.debug.frame.XExecutionStack;
import consulo.execution.debug.frame.XSuspendContext;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class CDTSuspendContext extends XSuspendContext {
    private CDTExecutionStack myStack;

    public CDTSuspendContext(Paused event, CDTProcess process) {
        myStack = new CDTExecutionStack("", event.getCallFrames(), process);
    }

    @Nullable
    @Override
    public XExecutionStack getActiveExecutionStack() {
        return myStack;
    }

    @Override
    public XExecutionStack[] getExecutionStacks() {
        return new XExecutionStack[] {myStack};
    }
}
