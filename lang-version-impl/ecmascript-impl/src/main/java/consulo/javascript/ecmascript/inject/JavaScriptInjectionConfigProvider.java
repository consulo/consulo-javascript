package consulo.javascript.ecmascript.inject;

import consulo.annotation.component.ExtensionImpl;
import consulo.language.inject.advanced.InjectionConfigProvider;

import java.io.InputStream;

/**
 * The default injections into JavaScript strings ({@code jsInjections.xml}): HTML into innerHTML, insertAdjacentHTML,
 * document.write...
 *
 * @since 2026-10-31
 */
@ExtensionImpl
public class JavaScriptInjectionConfigProvider implements InjectionConfigProvider {
    @Override
    public InputStream openConfigFileStream() throws Exception {
        return getClass().getResourceAsStream("/consulo/javascript/ecmascript/inject/jsInjections.xml");
    }
}
