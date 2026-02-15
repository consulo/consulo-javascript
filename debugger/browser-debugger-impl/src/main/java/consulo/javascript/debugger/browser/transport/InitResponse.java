package consulo.javascript.debugger.browser.transport;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class InitResponse {
    public String message = "init";
    public int tabId;
    public String status = "accepted";
    public String version = "7.4";

    public InitResponse(int tabId, boolean accepted) {
        this.tabId = tabId;
        this.status = accepted ? "accepted" : "";
    }
}
