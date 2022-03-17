package consulo.javascript.debugger.browser.transport;

import consulo.builtInServer.websocket.WebSocketAccepter;
import consulo.builtInServer.websocket.WebSocketConnection;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2020-06-15
 */
public class ExtensionWebSocketAccepter implements WebSocketAccepter
{
	@Override
	public void accept(@Nonnull WebSocketConnection webSocketConnection, @Nonnull byte[] bytes)
	{
		System.out.println(bytes.toString());
	}

	@Override
	public void accept(@Nonnull WebSocketConnection webSocketConnection, @Nonnull String text)
	{
		System.out.println(text);
	}
}
