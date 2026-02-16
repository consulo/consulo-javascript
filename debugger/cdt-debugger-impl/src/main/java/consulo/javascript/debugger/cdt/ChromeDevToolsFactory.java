package consulo.javascript.debugger.cdt;

import com.github.kklisura.cdt.services.WebSocketService;
import com.github.kklisura.cdt.services.config.ChromeDevToolsServiceConfiguration;
import com.github.kklisura.cdt.services.executors.EventExecutorService;
import com.github.kklisura.cdt.services.impl.ChromeDevToolsServiceImpl;
import com.github.kklisura.cdt.services.invocation.CommandInvocationHandler;
import com.github.kklisura.cdt.services.utils.ProxyUtils;
import consulo.proxy.advanced.AdvancedProxyBuilder;

import java.lang.reflect.Method;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class ChromeDevToolsFactory {
    public static ChromeDevToolsServiceImpl create(WebSocketService webSocketService) {
        ChromeDevToolsServiceConfiguration conf = new ChromeDevToolsServiceConfiguration();
        conf.setEventExecutorService(new EventExecutorService() {
            @Override
            public void execute(Runnable runnable) {
                runnable.run();
            }

            @Override
            public void shutdown() {

            }
        });

        if (Boolean.FALSE) {
            webSocketService = new LoggingWebSocketService(webSocketService);
        }

        // Create invocation handler
        CommandInvocationHandler commandInvocationHandler = new CommandInvocationHandler();

        // Setup command cache for this session
        Map<Method, Object> commandsCache = new ConcurrentHashMap<>();

        ChromeDevToolsServiceImpl service = AdvancedProxyBuilder.create(ChromeDevToolsServiceImpl.class)
            .withSuperConstructorArguments(webSocketService, conf)
            .withInvocationHandler((proxy, method, args) -> {
                return commandsCache.computeIfAbsent(
                    method,
                    key -> {
                        Class<?> returnType = method.getReturnType();
                        return ProxyUtils.createProxy(returnType, commandInvocationHandler);
                    });
            })
            .build();

        commandInvocationHandler.setChromeDevToolsService(service);

        return service;
    }
}
