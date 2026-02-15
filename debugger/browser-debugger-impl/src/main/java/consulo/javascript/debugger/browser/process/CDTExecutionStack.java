package consulo.javascript.debugger.browser.process;

import com.github.kklisura.cdt.protocol.types.debugger.CallFrame;
import consulo.execution.debug.frame.XExecutionStack;
import consulo.execution.debug.frame.XStackFrame;
import jakarta.annotation.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class CDTExecutionStack extends XExecutionStack {
    private final List<CDTStackFrame> myFrames;

    public CDTExecutionStack(String displayName,
                             List<CallFrame> callFrames,
                             CDTProcess process) {
        super(displayName);

        myFrames = new ArrayList<>(callFrames.size());
        for (CallFrame callFrame : callFrames) {
            myFrames.add(new CDTStackFrame(callFrame, process));
        }
    }

    @Nullable
    @Override
    public XStackFrame getTopFrame() {
        return myFrames.isEmpty() ? null : myFrames.getFirst();
    }

    @Override
    public void computeStackFrames(XStackFrameContainer container) {
        container.addStackFrames(myFrames, true);
    }
}
