package consulo.javascript.debugger.browser;

import consulo.annotation.access.RequiredReadAction;
import consulo.application.Application;
import consulo.compiler.execution.CompileStepBeforeRun;
import consulo.execution.DefaultExecutionResult;
import consulo.execution.ExecutionResult;
import consulo.execution.configuration.*;
import consulo.execution.configuration.fragment.RunConfigurationFragment;
import consulo.execution.executor.Executor;
import consulo.execution.runner.ExecutionEnvironment;
import consulo.execution.runner.ProgramRunner;
import consulo.execution.ui.console.ConsoleView;
import consulo.execution.ui.console.TextConsoleBuilder;
import consulo.execution.ui.console.TextConsoleBuilderFactory;
import consulo.module.Module;
import consulo.module.ModuleManager;
import consulo.process.ExecutionException;
import consulo.util.lang.StringUtil;
import consulo.webBrowser.BrowserLauncher;
import consulo.webBrowser.WebBrowser;
import consulo.webBrowser.WebBrowserManager;
import org.jspecify.annotations.Nullable;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * @author VISTALL
 * @since 2020-06-15
 */
public class JavaScriptBrowserConfiguration extends ModuleBasedConfiguration<RunConfigurationModule> implements RunConfigurationWithSuppressedDefaultRunAction, CompileStepBeforeRun.Suppressor {
    public JavaScriptBrowserConfiguration(String name, RunConfigurationModule configurationModule, ConfigurationFactory factory) {
        super(name, configurationModule, factory);
    }

    public JavaScriptBrowserConfiguration(RunConfigurationModule configurationModule, ConfigurationFactory factory) {
        super(configurationModule, factory);
    }

    @Override
    protected Class<JavaScriptBrowserConfigurationOptions> getOptionsClass() {
        return JavaScriptBrowserConfigurationOptions.class;
    }

    @Override
    public JavaScriptBrowserConfigurationOptions getOptions() {
        return (JavaScriptBrowserConfigurationOptions) super.getOptions();
    }

    @Override
    protected void convertLegacyState(LegacyRunConfigurationConverter converter) {
        super.convertLegacyState(converter);

        JavaScriptBrowserConfigurationOptions options = new JavaScriptBrowserConfigurationOptions();
        options.setUrl(converter.readXml(JavaScriptBrowserConfigurationLegacyState.class).URL);
        converter.setOptions(options);
    }

    @Override
    public List<RunConfigurationFragment<?>> createFragments() {
        return List.of(new JavaScriptBrowserUrlFragment());
    }

    public @Nullable String getUrl() {
        return getOptions().getUrl();
    }

    public void setUrl(@Nullable String url) {
        getOptions().setUrl(url);
    }

    @Override
    @RequiredReadAction
    public Collection<Module> getValidModules() {
        ModuleManager manager = ModuleManager.getInstance(getProject());
        return Arrays.asList(manager.getModules());
    }

    @Nullable
    @Override
    public RunProfileState getState(Executor executor, ExecutionEnvironment environment) throws ExecutionException {
        String url = getUrl();
        return new RunProfileState() {
            @Override
            public ExecutionResult execute(Executor executor, ProgramRunner programRunner) throws ExecutionException {
                TextConsoleBuilder builder = TextConsoleBuilderFactory.getInstance().createBuilder(environment.getProject());

                ConsoleView console = builder.getConsole();

                WebBrowserManager manager = WebBrowserManager.getInstance();
                WebBrowser webBrowser = manager.getFirstActiveBrowser();

                if (webBrowser == null) {
                    throw new ExecutionException("Can't find Chrome browser");
                }

                if (url == null || StringUtil.isEmpty(url)) {
                    throw new ExecutionException("URL is empty");
                }

                BrowserLauncher.getInstance().browse(url, webBrowser, environment.getProject());

                SessionHolder holder = Application.get().getInstance(SessionHolder.class);

                SessionHolder.BrowserSession browserSession = holder.prepareSession(url);

                return new DefaultExecutionResult(console, new BrowserDebuggerProcess(browserSession));
            }
        };
    }
}
