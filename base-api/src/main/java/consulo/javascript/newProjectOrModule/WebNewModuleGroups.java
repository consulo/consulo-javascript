package consulo.javascript.newProjectOrModule;

import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.localize.LocalizeValue;
import consulo.module.creation.scratch.NewModuleContext;
import consulo.module.creation.scratch.NewModuleContextGroup;

/**
 * The top-level "Web (JavaScript)" group of the new project / module wizard: the projects of web frameworks (Vue.js,
 * Nuxt, Angular...) are its subgroups. Every builder asks for it here, so it has one name and icon whichever builder
 * comes first. (Node.js stays a group of its own: not every Node.js project is a web project.)
 *
 * @since 2026-10-31
 */
public final class WebNewModuleGroups {
    public static final String WEB_GROUP_ID = "web";

    private WebNewModuleGroups() {
    }

    public static NewModuleContextGroup getWebGroup(NewModuleContext context) {
        return context.addGroup(WEB_GROUP_ID, LocalizeValue.localizeTODO("Web (JavaScript)"), JavaScriptIconGroup.javascript());
    }
}
