package consulo.javascript.debugger.cdt;

import com.github.kklisura.cdt.protocol.types.debugger.Location;
import consulo.execution.debug.breakpoint.XBreakpoint;

import java.util.List;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class CDTBreakpointInfo {
    private List<Location> myLocations;
    private XBreakpoint<?> myBreakpoint;

    public CDTBreakpointInfo(List<Location> locations, XBreakpoint<?> breakpoint) {
        myLocations = locations;
        myBreakpoint = breakpoint;
    }

    public List<Location> getLocations() {
        return myLocations;
    }

    public void setLocations(List<Location> locations) {
        myLocations = locations;
    }

    public XBreakpoint<?> getBreakpoint() {
        return myBreakpoint;
    }

    public void setBreakpoint(XBreakpoint<?> breakpoint) {
        myBreakpoint = breakpoint;
    }
}
