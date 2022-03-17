package consulo.javascript.debugger.browser;

import com.intellij.compiler.options.CompileStepBeforeRun;
import com.intellij.execution.DefaultExecutionResult;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.ExecutionResult;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.*;
import com.intellij.execution.filters.TextConsoleBuilder;
import com.intellij.execution.filters.TextConsoleBuilderFactory;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.execution.runners.ProgramRunner;
import com.intellij.execution.runners.RunConfigurationWithSuppressedDefaultRunAction;
import com.intellij.execution.ui.ConsoleView;
import com.intellij.ide.browsers.BrowserFamily;
import com.intellij.ide.browsers.BrowserLauncher;
import com.intellij.ide.browsers.WebBrowser;
import com.intellij.ide.browsers.WebBrowserManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.xdebugger.DefaultDebugProcessHandler;
import consulo.annotation.access.RequiredReadAction;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Arrays;
import java.util.Collection;

/**
 * @author VISTALL
 * @since 2020-06-15
 */
public class JavaScriptBrowserConfiguration extends ModuleBasedConfiguration<RunConfigurationModule> implements RunConfigurationWithSuppressedDefaultRunAction, CompileStepBeforeRun.Suppressor
{
	public JavaScriptBrowserConfiguration(String name, RunConfigurationModule configurationModule, ConfigurationFactory factory)
	{
		super(name, configurationModule, factory);
	}

	public JavaScriptBrowserConfiguration(RunConfigurationModule configurationModule, ConfigurationFactory factory)
	{
		super(configurationModule, factory);
	}

	@Override
	@RequiredReadAction
	public Collection<Module> getValidModules()
	{
		ModuleManager manager = ModuleManager.getInstance(getProject());
		return Arrays.asList(manager.getModules());
	}

	@Nonnull
	@Override
	public SettingsEditor<? extends RunConfiguration> getConfigurationEditor()
	{
		return new JavaScriptBrowserConfigurationEditor();
	}

	@Nullable
	@Override
	public RunProfileState getState(@Nonnull Executor executor, @Nonnull ExecutionEnvironment environment) throws ExecutionException
	{
		return new RunProfileState()
		{
			@Nullable
			@Override
			public ExecutionResult execute(Executor executor, @Nonnull ProgramRunner programRunner) throws ExecutionException
			{
				TextConsoleBuilder builder = TextConsoleBuilderFactory.getInstance().createBuilder(environment.getProject());

				ConsoleView console = builder.getConsole();

				WebBrowser browser = WebBrowserManager.getInstance().getFirstBrowserOrNull(BrowserFamily.CHROME);

				if(browser == null)
				{
					throw new ExecutionException("Can't find Chrome browser");
				}

				BrowserLauncher.getInstance().browse("http://localhost", browser, environment.getProject());

				return new DefaultExecutionResult(console, new DefaultDebugProcessHandler());
			}
		};
	}
}
