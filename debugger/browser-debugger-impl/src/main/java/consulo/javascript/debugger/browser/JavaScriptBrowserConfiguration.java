package consulo.javascript.debugger.browser;

import consulo.annotation.access.RequiredReadAction;
import consulo.application.Application;
import consulo.compiler.execution.CompileStepBeforeRun;
import consulo.execution.DefaultExecutionResult;
import consulo.execution.ExecutionResult;
import consulo.execution.configuration.*;
import consulo.execution.configuration.ui.SettingsEditor;
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
import consulo.util.xml.serializer.InvalidDataException;
import consulo.util.xml.serializer.WriteExternalException;
import consulo.util.xml.serializer.XmlSerializer;
import consulo.webBrowser.BrowserLauncher;
import consulo.webBrowser.WebBrowser;
import consulo.webBrowser.WebBrowserManager;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import org.jdom.Element;

import java.util.Arrays;
import java.util.Collection;

/**
 * @author VISTALL
 * @since 2020-06-15
 */
public class JavaScriptBrowserConfiguration extends ModuleBasedConfiguration<RunConfigurationModule> implements RunConfigurationWithSuppressedDefaultRunAction, CompileStepBeforeRun.Suppressor {
    public String URL = "http://localhost";

    public JavaScriptBrowserConfiguration(String name, RunConfigurationModule configurationModule, ConfigurationFactory factory) {
        super(name, configurationModule, factory);
    }

    public JavaScriptBrowserConfiguration(RunConfigurationModule configurationModule, ConfigurationFactory factory) {
        super(configurationModule, factory);
    }

    @Override
    public void readExternal(Element element) throws InvalidDataException {
        super.readExternal(element);
        XmlSerializer.deserializeInto(this, element);
    }

    @Override
    public void writeExternal(Element element) throws WriteExternalException {
        super.writeExternal(element);
        XmlSerializer.serializeInto(this, element);
    }

    @Override
    @RequiredReadAction
    public Collection<Module> getValidModules() {
        ModuleManager manager = ModuleManager.getInstance(getProject());
        return Arrays.asList(manager.getModules());
    }

    @Nonnull
    @Override
    public SettingsEditor<? extends RunConfiguration> getConfigurationEditor() {
        return new JavaScriptBrowserConfigurationEditor();
    }

    @Nullable
    @Override
    public RunProfileState getState(@Nonnull Executor executor, @Nonnull ExecutionEnvironment environment) throws ExecutionException {
        return new RunProfileState() {
            @Override
            public ExecutionResult execute(Executor executor, @Nonnull ProgramRunner programRunner) throws ExecutionException {
                TextConsoleBuilder builder = TextConsoleBuilderFactory.getInstance().createBuilder(environment.getProject());

                ConsoleView console = builder.getConsole();

                WebBrowserManager manager = WebBrowserManager.getInstance();
                WebBrowser webBrowser = manager.getFirstActiveBrowser();

                if (webBrowser == null) {
                    throw new ExecutionException("Can't find Chrome browser");
                }

                if (StringUtil.isEmpty(URL)) {
                    throw new ExecutionException("URL is empty");
                }

                BrowserLauncher.getInstance().browse(URL, webBrowser, environment.getProject());

                SessionHolder holder = Application.get().getInstance(SessionHolder.class);

                SessionHolder.BrowserSession browserSession = holder.prepareSession(URL);

                return new DefaultExecutionResult(console, new BrowserDebuggerProcess(browserSession));
            }
        };
    }
}
