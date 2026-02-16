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
import consulo.ui.ex.ColoredTextContainer;
import consulo.ui.ex.SimpleTextAttributes;
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

    private final Location myLocation;
    private final CDTScript myScript;
    private final String myScriptId;

    public CDTStackFrame(CallFrame callFrame, CDTProcessBase process) {
        myCallFrame = callFrame;
        myProcess = process;

        myLocation = callFrame.getLocation();

        if (myLocation != null) {
            myScriptId = myLocation.getScriptId();
            myScript = process.getScripts().find(myScriptId);

            if (myScript != null) {
                mySourcePosition = Application.get().getInstance(XSourcePositionFactory.class).createPosition(
                    myScript.toVirtualFile(),
                    myLocation.getLineNumber(),
                    myLocation.getColumnNumber()
                );
            }
        }
        else {
            myScript = null;
            myScriptId = null;
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
    public void customizePresentation(ColoredTextContainer component) {
        XSourcePosition position = getSourcePosition();
        if (position != null) {
            component.append(position.getFile().getName(), SimpleTextAttributes.REGULAR_ATTRIBUTES);
            component.append(":" + (position.getLine() + 1), SimpleTextAttributes.REGULAR_ATTRIBUTES);
        }
        else if (myScript != null) {
            component.append(myScript.getPath(), SimpleTextAttributes.REGULAR_ATTRIBUTES);
            component.append(":" + (myLocation.getLineNumber() + 1), SimpleTextAttributes.REGULAR_ATTRIBUTES);
        }
        else {
            component.append(myScriptId, SimpleTextAttributes.REGULAR_ATTRIBUTES);
        }
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

                list.addTopGroup(new CDTScopeValue(scope, object, myProcess));
            }

            node.addChildren(list, true);
        });
    }
}
