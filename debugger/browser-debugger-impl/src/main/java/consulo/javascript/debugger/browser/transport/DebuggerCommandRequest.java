package consulo.javascript.debugger.browser.transport;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class DebuggerCommandRequest {
    public String message = "debugger_command";

    public int tabId;

    public Object command;
}
