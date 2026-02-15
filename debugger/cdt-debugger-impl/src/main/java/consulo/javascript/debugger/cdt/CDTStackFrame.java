package consulo.javascript.debugger.cdt;

import com.github.kklisura.cdt.protocol.types.debugger.CallFrame;
import com.github.kklisura.cdt.protocol.types.debugger.Location;
import com.github.kklisura.cdt.protocol.types.debugger.Scope;
import com.github.kklisura.cdt.protocol.types.runtime.RemoteObject;
import com.github.kklisura.cdt.protocol.types.runtime.RemoteObjectType;
import consulo.application.Application;
import consulo.execution.debug.XSourcePosition;
import consulo.execution.debug.XSourcePositionFactory;
import consulo.execution.debug.frame.XCompositeNode;
import consulo.execution.debug.frame.XStackFrame;
import consulo.execution.debug.frame.XValueChildrenList;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.List;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class CDTStackFrame extends XStackFrame {
    private final CallFrame myCallFrame;
    private final CDTProcessBase myProcess;

    private XSourcePosition mySourcePosition;

    public CDTStackFrame(CallFrame callFrame, CDTProcessBase process) {
        myCallFrame = callFrame;
        myProcess = process;

        Location location = callFrame.getLocation();

        if (location != null) {
            String scriptId = location.getScriptId();

            CDTScript script = process.getScripts().find(scriptId);
            if (script != null) {
                mySourcePosition = Application.get().getInstance(XSourcePositionFactory.class).createPosition(
                    script.toVirtualFile(),
                    location.getLineNumber(),
                    location.getColumnNumber()
                );
            }
        }
    }

    @Nullable
    @Override
    public XSourcePosition getSourcePosition() {
        return mySourcePosition;
    }

    @Nullable
    @Override
    public Object getEqualityObject() {
        return myCallFrame.getCallFrameId();
    }

    @Override
    public void computeChildren(@Nonnull XCompositeNode node) {
        myProcess.invoke(devTools -> {
            List<Scope> scopeChain = myCallFrame.getScopeChain();

            XValueChildrenList list = new XValueChildrenList();
            for (Scope scope : scopeChain) {
                RemoteObject object = scope.getObject();
                if (object == null) {
                    continue;
                }

                if (object.getType() != RemoteObjectType.OBJECT) {
                    continue;
                }

                list.add(new CDTScopeValue(scope, object, myProcess));
            }

            node.addChildren(list, true);
        });
    }
}
