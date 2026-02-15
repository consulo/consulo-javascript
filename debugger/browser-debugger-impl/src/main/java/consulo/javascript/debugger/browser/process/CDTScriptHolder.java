package consulo.javascript.debugger.browser.process;

import consulo.util.collection.Lists;

import java.util.List;
import java.util.Objects;

/**
 * @author VISTALL
 * @since 2026-02-15
 */
public class CDTScriptHolder {
    private final List<CDTScript> myScripts = Lists.newLockFreeCopyOnWriteList();

    public void add(CDTScript script) {
        myScripts.add(script);
    }

    public CDTScript find(String scriptId) {
        for (CDTScript script : myScripts) {
            if (Objects.equals(scriptId, script.getId())) {
                return script;
            }
        }

        return null;
    }
}
