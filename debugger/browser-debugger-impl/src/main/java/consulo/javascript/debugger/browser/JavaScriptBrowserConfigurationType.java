package consulo.javascript.debugger.browser;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationTypeBase;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.execution.configurations.RunConfigurationModule;
import com.intellij.icons.AllIcons;
import com.intellij.openapi.project.Project;
import consulo.javascript.module.extension.JavaScriptModuleExtension;
import consulo.module.extension.ModuleExtensionHelper;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2020-06-15
 */
public class JavaScriptBrowserConfigurationType extends ConfigurationTypeBase
{
	public JavaScriptBrowserConfigurationType()
	{
		super("JavaScriptBrowserConfigurationType", "JavaScript Debug", "", AllIcons.RunConfigurations.Web_app);

		addFactory(new ConfigurationFactory(this)
		{
			@Override
			public RunConfiguration createTemplateConfiguration(Project project)
			{
				return new JavaScriptBrowserConfiguration(new RunConfigurationModule(project), this);
			}

			@Override
			public boolean isApplicable(@Nonnull Project project)
			{
				return ModuleExtensionHelper.getInstance(project).hasModuleExtension(JavaScriptModuleExtension.class);
			}
		});
	}
}
