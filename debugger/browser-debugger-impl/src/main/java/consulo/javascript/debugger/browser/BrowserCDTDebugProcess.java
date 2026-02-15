package consulo.javascript.debugger.browser;

import consulo.execution.ExecutionResult;
import consulo.execution.debug.XDebugSession;
import consulo.javascript.debugger.cdt.CDTProcessBase;
import consulo.javascript.debugger.cdt.ChromeDevToolsFactory;
import consulo.process.ExecutionException;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class BrowserCDTDebugProcess extends CDTProcessBase {
    public BrowserCDTDebugProcess(@Nonnull XDebugSession session, ExecutionResult result) throws ExecutionException {
        super(session, result);
    }

    public void init(SessionHolder.BrowserSession session) {
        initTools(ChromeDevToolsFactory.create(new ProxyWebSocketService(session)));
    }
}
