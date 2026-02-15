package consulo.javascript.debugger.browser.transport;

import com.google.gson.Gson;
import consulo.annotation.component.ExtensionImpl;
import consulo.application.ReadAction;
import consulo.builtinWebServer.webSocket.WebSocketAccepter;
import consulo.builtinWebServer.webSocket.WebSocketConnection;
import consulo.execution.debug.XDebugSession;
import consulo.execution.ui.console.ConsoleViewContentType;
import consulo.javascript.debugger.browser.BrowserCDTDebugProcess;
import consulo.javascript.debugger.browser.SessionHolder;
import consulo.javascript.debugger.cdt.CDTProcessBase;
import jakarta.annotation.Nonnull;
import jakarta.inject.Inject;

import java.nio.charset.StandardCharsets;
import java.util.Map;

/**
 * @author VISTALL
 * @since 2020-06-15
 */
@ExtensionImpl
public class ExtensionWebSocketAccepter implements WebSocketAccepter {
    public static final Gson GSON = new Gson();

    private final SessionHolder mySessionHolder;

    @Inject
    public ExtensionWebSocketAccepter(SessionHolder sessionHolder) {
        mySessionHolder = sessionHolder;
    }

    @Override
    public void accept(@Nonnull WebSocketConnection webSocketConnection, @Nonnull byte[] bytes) {
        accept(webSocketConnection, new String(bytes, StandardCharsets.UTF_8));
    }

    @Override
    public void accept(@Nonnull WebSocketConnection conn, @Nonnull String text) {
        try {
            SessionHolder.BrowserSession session = null;

            Object jsonObject = GSON.fromJson(text, Object.class);
            if (jsonObject instanceof Map map) {
                Object message = map.get("message");

                if (message instanceof String messageStr) {
                    switch (messageStr) {
                        case "init": {
                            String url = (String) map.get("url");
                            int tabId = getInt(map.get("tabId"));

                            session = mySessionHolder.initNew(url, tabId, conn);

                            conn.send(GSON.toJson(new InitResponse(tabId, session != null)));
                            if (session != null) {
                                conn.send(GSON.toJson(new AttachDebuggerResponse(tabId)));
                            }
                            break;
                        }
                        case "debugger_detached": {
                            int tabId = getInt(map.get("tabId"));

                            session = mySessionHolder.find(tabId);

                            if (session != null) {
                                CDTProcessBase debugProcess = session.getDebugProcess();

                                XDebugSession xDebugSession = debugProcess.getSession();

                                xDebugSession.getConsoleView().print("Browser disconnected\n", ConsoleViewContentType.LOG_INFO_OUTPUT);

                                debugProcess.getProcessHandler().destroyProcess();
                            }
                            break;
                        }
                        case "debugger_command_response": {
                            int tabId = getInt(map.get("tabId"));

                            session = mySessionHolder.find(tabId);

                            if (session != null) {
                                Object response = map.get("response");

                                String responseJson = GSON.toJson(response);

                                session.fireHandlers(responseJson);
                            }
                            break;
                        }
                        case "debugger_attached": {
                            int tabId = getInt(map.get("tabId"));

                            session = mySessionHolder.find(tabId);

                            if (session != null) {
                                BrowserCDTDebugProcess debugProcess = session.getDebugProcess();

                                XDebugSession xDebugSession = debugProcess.getSession();

                                debugProcess.init(session);

                                xDebugSession.getConsoleView().print("Browser connected\n", ConsoleViewContentType.LOG_INFO_OUTPUT);

                                ReadAction.nonBlocking(xDebugSession::initBreakpoints).submitDefault();
                            }

                            break;
                        }
                    }
                }
            }
        }
        catch (Exception ignored) {
        }
    }

    private static int getInt(Object value) {
        if (value instanceof Number number) {
            return number.intValue();
        }

        if (value instanceof String str) {
            return Integer.parseInt(str);
        }

        throw new UnsupportedOperationException(value + " is not number");
    }
}
