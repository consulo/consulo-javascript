package consulo.javascript.debugger.cdt;

import com.github.kklisura.cdt.protocol.types.runtime.Properties;
import com.github.kklisura.cdt.protocol.types.runtime.PropertyDescriptor;
import com.github.kklisura.cdt.protocol.types.runtime.RemoteObject;
import com.github.kklisura.cdt.protocol.types.runtime.RemoteObjectType;
import consulo.execution.debug.frame.*;
import consulo.execution.debug.frame.presentation.XValuePresentation;
import consulo.execution.debug.icon.ExecutionDebugIconGroup;
import consulo.ui.image.Image;
import jakarta.annotation.Nonnull;

import java.util.List;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class CDTRemoteObjectValue extends XNamedValue {
    @Nonnull
    private final RemoteObject myRemoteObject;

    private final CDTProcessBase myProcess;

    public CDTRemoteObjectValue(@Nonnull String name, @Nonnull RemoteObject remoteObject, CDTProcessBase process) {
        super(name);
        myRemoteObject = remoteObject;
        myProcess = process;
    }

    @Override
    public void computeChildren(@Nonnull XCompositeNode node) {
        if (myRemoteObject.getType() != RemoteObjectType.OBJECT) {
            node.addChildren(XValueChildrenList.EMPTY, true);
            return;
        }

        myProcess.invoke(devTools -> {
            String objectId = myRemoteObject.getObjectId();

            Properties properties = devTools.getRuntime().getProperties(objectId);

            List<PropertyDescriptor> result = properties.getResult();

            XValueChildrenList list = new XValueChildrenList();

            for (PropertyDescriptor propertyDescriptor : result) {
                RemoteObject value = propertyDescriptor.getValue();

                list.add(new CDTRemoteObjectValue(propertyDescriptor.getName(), value, myProcess));
            }

            node.addChildren(list, true);
        });
    }

    @Nonnull
    protected Image getObjectIcon() {
        return ExecutionDebugIconGroup.nodeValue();
    }

    @Override
    public void computePresentation(@Nonnull XValueNode node, @Nonnull XValuePlace place) {
        if (myRemoteObject.getType() == RemoteObjectType.OBJECT) {
            node.setPresentation(getObjectIcon(), "", "", true);
        }
        else {
            node.setPresentation(ExecutionDebugIconGroup.nodePrimitive(), new XValuePresentation() {
                @Override
                public void renderValue(@Nonnull XValueTextRenderer renderer) {
                    switch (myRemoteObject.getType()) {
                        case STRING:
                            renderer.renderStringValue(String.valueOf(myRemoteObject.getValue()));
                            break;
                        case NUMBER:
                        case BIGINT:
                            renderer.renderNumericValue(String.valueOf(myRemoteObject.getValue()));
                            break;
                        case BOOLEAN:
                            renderer.renderKeywordValue(String.valueOf(myRemoteObject.getValue()));
                            break;
                    }
                }
            }, myRemoteObject.getType() == RemoteObjectType.OBJECT);
        }
    }
}
