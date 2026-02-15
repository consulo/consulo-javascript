package consulo.javascript.debugger.browser;

import consulo.annotation.component.ExtensionImpl;
import consulo.application.AllIcons;
import consulo.execution.configuration.ConfigurationFactory;
import consulo.execution.configuration.ConfigurationTypeBase;
import consulo.execution.configuration.RunConfiguration;
import consulo.execution.configuration.RunConfigurationModule;
import consulo.javascript.module.extension.JavaScriptModuleExtension;
import consulo.localize.LocalizeValue;
import consulo.module.extension.ModuleExtensionHelper;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2020-06-15
 */
@ExtensionImpl
public class JavaScriptBrowserConfigurationType extends ConfigurationTypeBase {
    public JavaScriptBrowserConfigurationType() {
        super("JavaScriptBrowserConfigurationType", LocalizeValue.localizeTODO("JavaScript Debug"), AllIcons.RunConfigurations.Web_app);

        addFactory(new ConfigurationFactory(this) {
            @Override
            public RunConfiguration createTemplateConfiguration(Project project) {
                return new JavaScriptBrowserConfiguration(new RunConfigurationModule(project), this);
            }

            @Override
            public boolean isApplicable(@Nonnull Project project) {
                return ModuleExtensionHelper.getInstance(project).hasModuleExtension(JavaScriptModuleExtension.class);
            }
        });
    }
}
